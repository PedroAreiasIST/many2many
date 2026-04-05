// MeshCrack.cs
//
// Copyright (C) 2026 Pedro Miguel de Almeida Areias
//
// This file is part of ManyToMany.
//
// ManyToMany is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// ManyToMany is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with ManyToMany. If not, see <https://www.gnu.org/licenses/>.

// MeshCrack.cs - Crack insertion via signed field (level set) method
// License: GPLv3

using static Numerical.MeshGeometry;
using static Numerical.MeshConstants;

namespace Numerical;

/// <summary>
///     Crack insertion operations: signed field crack creation for 2D and 3D meshes,
///     node duplication, tip detection, and crack-related mesh manipulation.
/// </summary>
public static class MeshCrack
{
    // ========================================================================
    // SIGNED FIELD CRACK INSERTION
    // ========================================================================
    
    /// <summary>
    /// Signed field function delegate for level set crack insertion.
    /// Returns: positive on one side, negative on other side, zero on crack.
    /// </summary>
    /// <example>
    /// SINGLE LEVEL SET EXAMPLES:
    /// Horizontal crack at y=0.5: (x,y,z) => y - 0.5
    /// Circular crack: (x,y,z) => Math.Sqrt((x-cx)^2 + (y-cy)^2) - radius
    /// Planar 3D crack: (x,y,z) => z - 0.5
    /// 
    /// TWO LEVEL SET EXAMPLES:
    /// Level set 1 defines crack surface, level set 2 defines active region.
    /// Crack forms where surface crosses zero AND region is ≤ 0.
    /// 
    /// 2D horizontal crack from x=0.2 to x=1.5:
    ///   crackSurface = (x,y,z) => y - 0.5
    ///   activeRegion = (x,y,z) => (x < 0.2) ? (x - 0.2) : (x > 1.5) ? (x - 1.5) : -0.1
    /// 
    /// 3D circular crack (plane z=0.5, radius 0.3 at center):
    ///   crackSurface = (x,y,z) => z - 0.5
    ///   activeRegion = (x,y,z) => Math.Sqrt((x-0.5)^2 + (y-0.5)^2) - 0.3
    /// </example>
    public delegate double SignedFieldFunction(double x, double y, double z);
    
    /// <summary>
    /// Create crack in mesh using signed field (level set method).
    /// 
    /// Algorithm:
    /// 1. Find edges where field changes sign
    /// 2. Refine those edges, positioning new nodes at exact zero-crossing
    /// 3. Classify original nodes by field sign (positive/negative)
    /// 4. Duplicate all crack nodes except tips
    /// 5. Elements with all positive original nodes use duplicates
    /// 
    /// Works for all crack types:
    /// - Boundary-to-boundary
    /// - Edge-to-interior (with singular tip)
    /// - Closed loops
    /// - Any shape defined by implicit function
    /// </summary>
    /// <param name="mesh">Input mesh</param>
    /// <param name="coordinates">Node coordinates</param>
    /// <param name="signedField">Signed distance/level set function (defines crack surface)</param>
    /// <param name="regionField">Optional second level set to define crack active region (crack exists where regionField ≤ 0). If null, crack extends along entire zero level set.</param>
    /// <param name="enableSmoothing">Enable CVT smoothing after refinement (disable for multi-crack patterns)</param>
    /// <returns>Cracked mesh and coordinates</returns>
    
    /// <summary>
    /// Helper to safely get z-coordinate from 2D or 3D coordinate array
    /// </summary>
    private static double GetZ(double[,] coords, int nodeId)
    {
        return coords.GetLength(1) == 3 ? coords[nodeId, 2] : 0.0;
    }
    
    public static (SimplexMesh, double[,]) CreateCrackFromSignedField(
        SimplexMesh mesh,
        double[,] coordinates,
        SignedFieldFunction signedField,
        SignedFieldFunction? regionField = null,
        bool enableMeshPerturbation = false,
        bool enableSmoothing = false,
        double visualizationOffset = 0.0)
    {
        int originalNodeCount = mesh.Count<Node>();
        int coordDim = coordinates.GetLength(1);  // 2 for 2D, 3 for 3D
        
        // Clone coordinates to avoid modifying input
        coordinates = (double[,])coordinates.Clone();
        
        // OPTIONAL: MESH PERTURBATION to avoid exact zeros in signed field
        if (enableMeshPerturbation)
        {
            Console.WriteLine($"  → Applying mesh perturbation...");
            
            // Calculate average edge length
            SimplexRemesher.DiscoverEdges(mesh);
            double avgEdgeLength = 0.0;
            int numEdges = 0;
            for (int i = 0; i < mesh.Count<Edge>(); i++)
            {
                var nodes = mesh.NodesOf<Edge, Node>(i);
                int n1 = nodes[0], n2 = nodes[1];
                double dx = coordinates[n2, 0] - coordinates[n1, 0];
                double dy = coordinates[n2, 1] - coordinates[n1, 1];
                double dz = coordDim == 3 ? coordinates[n2, 2] - coordinates[n1, 2] : 0.0;
                avgEdgeLength += Math.Sqrt(dx * dx + dy * dy + dz * dz);
                numEdges++;
            }
            avgEdgeLength /= numEdges;
            
            // Perturb each node by a tiny random amount (1e-6 * avgEdgeLength)
            double perturbationScale = 1e-6 * avgEdgeLength;
            var random = new Random(12345);  // Fixed seed for reproducibility
            
            for (int i = 0; i < mesh.Count<Node>(); i++)
            {
                coordinates[i, 0] += (random.NextDouble() * 2.0 - 1.0) * perturbationScale;
                coordinates[i, 1] += (random.NextDouble() * 2.0 - 1.0) * perturbationScale;
                if (coordDim == 3)
                    coordinates[i, 2] += (random.NextDouble() * 2.0 - 1.0) * perturbationScale;
            }
            
            Console.WriteLine($"  → Mesh perturbed by ±{perturbationScale:E3}");
        }
        else
        {
            SimplexRemesher.DiscoverEdges(mesh);
        }
        
        // Step 1: Find ALL edges that cross the signed field
        var edgesToRefine = new List<(int, int)>();
        
        Console.WriteLine($"  → Total edges in mesh: {mesh.Count<Edge>()}");
        
        int crossingEdges = 0;
        int regionFiltered = 0;
        
        for (int i = 0; i < mesh.Count<Edge>(); i++)
        {
            var nodes = mesh.NodesOf<Edge, Node>(i);
            int n1 = nodes[0], n2 = nodes[1];
            
            double f1 = signedField(coordinates[n1, 0], coordinates[n1, 1], GetZ(coordinates, n1));
            double f2 = signedField(coordinates[n2, 0], coordinates[n2, 1], GetZ(coordinates, n2));
            
            // Check if crack surface crosses this edge (sign change)
            // With perturbation, f1*f2 == 0 should be extremely rare
            bool crossesCrack = f1 * f2 < 0;
            
            if (crossesCrack)
            {
                crossingEdges++;
                
                // If region field is provided, check if edge is in active region
                if (regionField != null)
                {
                    double r1 = regionField(coordinates[n1, 0], coordinates[n1, 1], GetZ(coordinates, n1));
                    double r2 = regionField(coordinates[n2, 0], coordinates[n2, 1], GetZ(coordinates, n2));
                    
                    // Check if crossing point is in active region
                    // Approximate crossing point using linear interpolation
                    double t = Math.Abs(f2 - f1) > 1e-14 ? -f1 / (f2 - f1) : 0.5;
                    t = Math.Max(0.0, Math.Min(1.0, t));
                    
                    double xCross = coordinates[n1, 0] + t * (coordinates[n2, 0] - coordinates[n1, 0]);
                    double yCross = coordinates[n1, 1] + t * (coordinates[n2, 1] - coordinates[n1, 1]);
                    double zCross = GetZ(coordinates, n1) + t * (GetZ(coordinates, n2) - GetZ(coordinates, n1));
                    double rCross = regionField(xCross, yCross, zCross);
                    
                    // Only refine edge if crossing point is in active region (r ≤ 0)
                    if (rCross <= 0)
                    {
                        edgesToRefine.Add((n1, n2));
                    }
                    else
                    {
                        regionFiltered++;
                    }
                }
                else
                {
                    // No region constraint - refine all crossing edges
                    edgesToRefine.Add((n1, n2));
                }
            }
        }
        
        Console.WriteLine($"  → Edges with sign change: {crossingEdges}");
        Console.WriteLine($"  → Filtered by region: {regionFiltered}");
        Console.WriteLine($"  → Edges to refine: {edgesToRefine.Count}");
        
        if (edgesToRefine.Count == 0)
        {
            Console.WriteLine($"  ⚠️  No edges cross the crack surface in active region");
            return (mesh, coordinates);
        }
        
        Console.WriteLine($"  → Refining {edgesToRefine.Count} edges that cross crack");
        
        // Step 2: Refine those edges once
        var refinedMesh = mesh;
        var refinedCoords = coordinates;
        
        {
            // Cache signed field values for all nodes (avoid recomputation)
            var cachedFieldValues = new double[refinedMesh.Count<Node>()];
            var cachedRegionValues = new double[refinedMesh.Count<Node>()];

            for (int i = 0; i < refinedMesh.Count<Node>(); i++)
            {
                cachedFieldValues[i] = signedField(refinedCoords[i, 0], refinedCoords[i, 1], GetZ(refinedCoords, i));
                if (regionField != null)
                    cachedRegionValues[i] = regionField(refinedCoords[i, 0], refinedCoords[i, 1], GetZ(refinedCoords, i));
            }

            // Find edges to refine in current mesh
            var currentEdgesToRefine = new HashSet<(int, int)>();

            for (int i = 0; i < refinedMesh.Count<Tri3>(); i++)
            {
                var n = refinedMesh.NodesOf<Tri3, Node>(i);
                int n1 = n[0], n2 = n[1], n3 = n[2];

                double f1 = cachedFieldValues[n1];
                double f2 = cachedFieldValues[n2];
                double f3 = cachedFieldValues[n3];

                // Check region constraint
                bool checkRegion = (regionField != null);
                bool refineEdge12 = false, refineEdge23 = false, refineEdge31 = false;

                if (f1 * f2 <= 0)
                {
                    if (checkRegion)
                    {
                        double r1 = cachedRegionValues[n1];
                        double r2 = cachedRegionValues[n2];
                        refineEdge12 = (r1 <= 0 || r2 <= 0);
                    }
                    else refineEdge12 = true;
                }

                if (f2 * f3 <= 0)
                {
                    if (checkRegion)
                    {
                        double r2 = cachedRegionValues[n2];
                        double r3 = cachedRegionValues[n3];
                        refineEdge23 = (r2 <= 0 || r3 <= 0);
                    }
                    else refineEdge23 = true;
                }

                if (f3 * f1 <= 0)
                {
                    if (checkRegion)
                    {
                        double r3 = cachedRegionValues[n3];
                        double r1 = cachedRegionValues[n1];
                        refineEdge31 = (r3 <= 0 || r1 <= 0);
                    }
                    else refineEdge31 = true;
                }

                if (refineEdge12) currentEdgesToRefine.Add(n1 < n2 ? (n1, n2) : (n2, n1));
                if (refineEdge23) currentEdgesToRefine.Add(n2 < n3 ? (n2, n3) : (n3, n2));
                if (refineEdge31) currentEdgesToRefine.Add(n1 < n3 ? (n1, n3) : (n3, n1));
            }

            if (currentEdgesToRefine.Count > 0)
            {
                Console.WriteLine($"  → Refining {currentEdgesToRefine.Count} edges");
                (refinedMesh, _) = MeshRefinement.Refine(refinedMesh, currentEdgesToRefine.ToList());
                refinedCoords = MeshRefinement.InterpolateCoordinates(refinedMesh, refinedCoords);
            }
        }
        
        // Step 3: Snap new crack nodes to EXACT zero-crossing (surface = 0)
        int snappedCount = 0;
        double maxSnapError = 0.0;
        int skippedTooClose = 0;
        int skippedOutsideRegion = 0;
        int skippedWouldInvert = 0;
        
        for (int i = originalNodeCount; i < refinedMesh.Count<Node>(); i++)
        {
            var parents = refinedMesh.Get<Node, ParentNodes>(i);
            
            if (parents.Parent1 != parents.Parent2)
            {
                int p1 = parents.Parent1, p2 = parents.Parent2;
                
                double x1 = refinedCoords[p1, 0], y1 = refinedCoords[p1, 1], z1 = GetZ(refinedCoords, p1);
                double x2 = refinedCoords[p2, 0], y2 = refinedCoords[p2, 1], z2 = GetZ(refinedCoords, p2);
                
                double f1 = signedField(x1, y1, z1);
                double f2 = signedField(x2, y2, z2);
                
                                // Snap to zero-crossing if edge crosses crack

                // Robust edge–surface intersection (curved implicit surfaces may intersect with same-sign endpoints)
                if (TryFindEdgeRootOnSegment(signedField,
                        x1, y1, z1,
                        x2, y2, z2,
                        out double tRoot,
                        out double xRoot, out double yRoot, out double zRoot,
                        out double phiRoot))
                {
                    // Current interpolated position
                    double xCurr = refinedCoords[i, 0];
                    double yCurr = refinedCoords[i, 1];
                    double zCurr = refinedCoords[i, 2];
                    double fCurr = signedField(xCurr, yCurr, zCurr);

                    // Don't snap if already very close to surface
                    double edgeLength = Math.Sqrt((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1) + (z2-z1)*(z2-z1));
                    double snapTolerance = Math.Max(1e-6, 1e-5 * edgeLength); // 0.001% of edge

                    if (Math.Abs(fCurr) < snapTolerance)
                    {
                        skippedTooClose++;
                        continue;
                    }

                    // CRITICAL: Only snap if the new position is INSIDE the crack region
                    if (regionField != null)
                    {
                        double regionValue = regionField(xRoot, yRoot, zRoot);
                        if (regionValue > 0)
                        {
                            skippedOutsideRegion++;
                            continue;
                        }
                    }

                    double snapDist = Math.Sqrt((xRoot-xCurr)*(xRoot-xCurr) + (yRoot-yCurr)*(yRoot-yCurr) + (zRoot-zCurr)*(zRoot-zCurr));

                    if (snapDist > snapTolerance)
                    {
                        bool wouldInvert = false; // (kept disabled as before)
                        if (!wouldInvert)
                        {
                            refinedCoords[i, 0] = xRoot;
                            refinedCoords[i, 1] = yRoot;
                            refinedCoords[i, 2] = zRoot;

                            double finalError = Math.Abs(signedField(refinedCoords[i, 0], refinedCoords[i, 1], refinedCoords[i, 2]));
                            maxSnapError = Math.Max(maxSnapError, finalError);
                            snappedCount++;
                        }
                        else
                        {
                            skippedWouldInvert++;
                        }
                    }
                    else
                    {
                        skippedTooClose++;
                    }
                }

            }
        }
        
        Console.WriteLine($"  → Snapped {snappedCount} nodes to surface=0 (max error: {maxSnapError:E3})");
        
        // Step 4: RECALCULATE nodal values of surface and region on refined mesh
        Console.WriteLine($"  → Recalculating surface and region values on ALL refined nodes...");
        
        var surfaceValues = new double[refinedMesh.Count<Node>()];
        var regionValues = new double[refinedMesh.Count<Node>()];
        
        for (int i = 0; i < refinedMesh.Count<Node>(); i++)
        {
            surfaceValues[i] = signedField(refinedCoords[i, 0], refinedCoords[i, 1], GetZ(refinedCoords, i));
            
            if (regionField != null)
            {
                regionValues[i] = regionField(refinedCoords[i, 0], refinedCoords[i, 1], GetZ(refinedCoords, i));
            }
            else
            {
                regionValues[i] = -1.0; // All nodes in active region if no region field
            }
        }
        
        // Discover edges before computing average length
        SimplexRemesher.DiscoverEdges(refinedMesh);
        
        // Compute average edge length for tolerance
        double avgEdgeLen = 0;
        int numEdges2 = 0;
        for (int i = 0; i < refinedMesh.Count<Edge>(); i++)
        {
            var n = refinedMesh.NodesOf<Edge, Node>(i);
            double dx = refinedCoords[n[1],0] - refinedCoords[n[0],0];
            double dy = refinedCoords[n[1],1] - refinedCoords[n[0],1];
            double dz = coordDim == 3 ? refinedCoords[n[1],2] - refinedCoords[n[0],2] : 0;
            avgEdgeLen += Math.Sqrt(dx*dx + dy*dy + dz*dz);
            numEdges2++;
        }
        avgEdgeLen /= numEdges2;
        
        // Step 5: Identify crack nodes (nodes where |surface| ≈ 0 AND region < -tol)
        // Use 0.1% of avgEdgeLen to exclude crack tip boundary
        const double zeroTolerance = 1e-5;
        double regionTolerance = 0.001 * avgEdgeLen;  // 0.1% of average edge
        var crackNodes = new HashSet<int>();
        int rejectedByRegion = 0;
        
        for (int i = 0; i < refinedMesh.Count<Node>(); i++)
        {
            if (Math.Abs(surfaceValues[i]) < zeroTolerance)
            {
                if (regionValues[i] < -regionTolerance)
                {
                    crackNodes.Add(i);
                }
                else
                {
                    rejectedByRegion++;
                }
            }
        }
        
        if (rejectedByRegion > 0)
        {
            Console.WriteLine($"  → Rejected {rejectedByRegion} nodes at crack tip (r ≥ {-regionTolerance:E2})");
        }
        
        Console.WriteLine($"  → Found {crackNodes.Count} crack nodes (|surface| < {zeroTolerance:E1}, region < {-regionTolerance:E2})");
        
        if (crackNodes.Count == 0)
        {
            Console.WriteLine($"  ⚠️  No crack nodes found!");
            return (mesh, coordinates);
        }
        
        // Step 6: Identify ORIGINAL mesh boundary (before refinement)
        Console.WriteLine($"  → Identifying original mesh boundary...");
        var originalBoundaryNodes = FindBoundaryNodes(mesh);
        Console.WriteLine($"  → Found {originalBoundaryNodes.Count} boundary nodes in original mesh");
        
        // Map refined nodes back to original boundary
        // A refined node is on original boundary if its parents are both on original boundary
        var refinedBoundaryNodes = new HashSet<int>();
        for (int i = 0; i < refinedMesh.Count<Node>(); i++)
        {
            var parents = refinedMesh.Get<Node, ParentNodes>(i);
            if (parents.Parent1 == parents.Parent2)
            {
                // Original node (not created by refinement)
                if (originalBoundaryNodes.Contains(parents.Parent1))
                    refinedBoundaryNodes.Add(i);
            }
            else
            {
                // Refined node (created on edge between Parent1 and Parent2)
                if (originalBoundaryNodes.Contains(parents.Parent1) && 
                    originalBoundaryNodes.Contains(parents.Parent2))
                    refinedBoundaryNodes.Add(i);
            }
        }
        
        Console.WriteLine($"  → Mapped to {refinedBoundaryNodes.Count} boundary nodes in refined mesh");
        
        // Step 7: Identify tip nodes (crack nodes at crack boundary/termination)
        // A tip node is a crack node that has neighbors OUTSIDE the crack region
        var tipNodes = new HashSet<int>();
        
        if (regionField != null)
        {
            // Build node-to-node connectivity
            var nodeNeighbors = BuildNodeNeighborsTri(refinedMesh);
            
            // Check each crack node for neighbors outside active region
            // Interior tips (crack terminates inside mesh): DON'T duplicate
            // Boundary tips (crack reaches mesh edge): DO duplicate
            foreach (int crackNode in crackNodes)
            {
                // Check if any neighbor is outside the crack region (r > 0)
                bool hasFarNeighbor = false;
                foreach (int neighbor in nodeNeighbors[crackNode])
                {
                    if (regionValues[neighbor] > 0)
                    {
                        hasFarNeighbor = true;
                        break;
                    }
                }
                
                // If this is a tip AND not on mesh boundary, don't duplicate it
                if (hasFarNeighbor && !refinedBoundaryNodes.Contains(crackNode))
                {
                    tipNodes.Add(crackNode);
                }
            }
        }
        
        Console.WriteLine($"  → Found {tipNodes.Count} tip nodes (will NOT be duplicated)");
        
        // Step 8: Nodes to duplicate = crack nodes - tip nodes
        var nodesToDuplicate = new HashSet<int>(crackNodes);
        nodesToDuplicate.ExceptWith(tipNodes);
        
        Console.WriteLine($"  → Will duplicate {nodesToDuplicate.Count} nodes");
        
        if (nodesToDuplicate.Count == 0)
        {
            Console.WriteLine($"  → No nodes to duplicate - returning refined mesh");
            return (refinedMesh, refinedCoords);
        }
        
        // Step 9: Duplicate nodes CAREFULLY
        return DuplicateNodesCarefully(
            refinedMesh,
            refinedCoords,
            nodesToDuplicate,
            surfaceValues,
            visualizationOffset,
            enableSmoothing);
    }
    
    /// <summary>
    /// Duplicate crack nodes to create discontinuity.
    /// Element-based approach: for each element, determine which side based on non-crack nodes.
    /// </summary>
    private static (SimplexMesh, double[,]) DuplicateNodesCarefully(
        SimplexMesh mesh,
        double[,] coords,
        HashSet<int> nodesToDuplicate,
        double[] surfaceValues,
        double visualizationOffset = 0.0,
        bool enableSmoothing = false)
    {
        Console.WriteLine($"  → Starting element-based crack duplication...");
        
        int totalNodes = mesh.Count<Node>();
        int coordDim = coords.GetLength(1);
        
        // CREATE ALL NODES (originals + duplicates)
        var crackedMesh = new SimplexMesh();
        int finalNodeCount = totalNodes + nodesToDuplicate.Count;
        var crackedCoords = new double[finalNodeCount, coordDim];
        
        var originalMap = new Dictionary<int, int>();
        var duplicateMap = new Dictionary<int, int>();
        
        // Copy all original nodes
        for (int i = 0; i < totalNodes; i++)
        {
            int newId = crackedMesh.Add<Node>();
            originalMap[i] = newId;
            
            for (int d = 0; d < coordDim; d++)
                crackedCoords[newId, d] = coords[i, d];
            
            var parents = mesh.Get<Node, ParentNodes>(i);
            crackedMesh.Set<Node, ParentNodes>(newId, parents);
        }
        
        // Create duplicates
        foreach (int nodeId in nodesToDuplicate)
        {
            int dupId = crackedMesh.Add<Node>();
            duplicateMap[nodeId] = dupId;
            
            for (int d = 0; d < coordDim; d++)
                crackedCoords[dupId, d] = coords[nodeId, d];
            
            var parents = mesh.Get<Node, ParentNodes>(nodeId);
            crackedMesh.Set<Node, ParentNodes>(dupId, parents);
        }
        
        Console.WriteLine($"  → Created {totalNodes} original + {nodesToDuplicate.Count} duplicate nodes");
        
        // RECREATE ELEMENTS - determine side based on non-crack nodes
        int elementsCreated = 0;
        int positiveSideElements = 0;
        int negativeSideElements = 0;
        int duplicatesUsed = 0;
        
        for (int i = 0; i < mesh.Count<Tri3>(); i++)
        {
            var nodes = mesh.NodesOf<Tri3, Node>(i);
            var newNodes = new int[3];
            
            // Determine element side from first non-crack node
            double sideValue = 0.0;
            bool hasNonCrackNode = false;
            
            for (int j = 0; j < 3; j++)
            {
                if (!nodesToDuplicate.Contains(nodes[j]))
                {
                    sideValue = surfaceValues[nodes[j]];
                    hasNonCrackNode = true;
                    break;
                }
            }
            
            // If element has no non-crack nodes, default to negative side
            bool usePositiveSide = hasNonCrackNode && (sideValue > 0);
            
            if (usePositiveSide)
                positiveSideElements++;
            else
                negativeSideElements++;
            
            // Assign nodes
            for (int j = 0; j < 3; j++)
            {
                if (!nodesToDuplicate.Contains(nodes[j]))
                {
                    // Non-crack node
                    newNodes[j] = originalMap[nodes[j]];
                }
                else
                {
                    // Crack node - use duplicate for positive, original for negative
                    if (usePositiveSide)
                    {
                        newNodes[j] = duplicateMap[nodes[j]];
                        duplicatesUsed++;
                    }
                    else
                    {
                        newNodes[j] = originalMap[nodes[j]];
                    }
                }
            }
            
            crackedMesh.Add<Tri3, Node>(newNodes[0], newNodes[1], newNodes[2]);
            elementsCreated++;
        }
        
        Console.WriteLine($"  → Created {elementsCreated} elements");
        Console.WriteLine($"  → Positive side: {positiveSideElements} elements, Negative side: {negativeSideElements} elements");
        Console.WriteLine($"  → Duplicate nodes used {duplicatesUsed} times in elements");
        Console.WriteLine($"  → Final mesh: {crackedMesh.Count<Node>()} nodes, {crackedMesh.Count<Tri3>()} triangles");
        
        // OPTIONAL: VISUALIZATION OFFSET (separate crack surfaces)
        if (visualizationOffset > 0)
        {
            Console.WriteLine($"  → Applying visualization offset: {visualizationOffset}mm");
            
            // Simple approach: offset along surface normal approximation
            foreach (var crackNode in nodesToDuplicate)
            {
                int origId = originalMap[crackNode];
                int dupId = duplicateMap[crackNode];
                
                // Approximate normal from surface gradient (simplified)
                double nx = 0, ny = 0;
                
                // Use surface values to estimate gradient direction
                double surfVal = surfaceValues[crackNode];
                if (Math.Abs(surfVal) < 1e-6)
                {
                    // On crack - use simple approximation
                    nx = 0;
                    ny = 1.0;  // Default to y-direction
                }
                
                // Normalize
                double len = Math.Sqrt(nx * nx + ny * ny + 1e-10);
                nx /= len;
                ny /= len;
                
                // Offset nodes in opposite directions
                crackedCoords[origId, 0] -= (visualizationOffset / 2.0) * nx;
                crackedCoords[origId, 1] -= (visualizationOffset / 2.0) * ny;
                
                crackedCoords[dupId, 0] += (visualizationOffset / 2.0) * nx;
                crackedCoords[dupId, 1] += (visualizationOffset / 2.0) * ny;
            }
        }
        
        // OPTIONAL: SMOOTHING (with crack nodes fixed as boundaries)
        if (enableSmoothing)
        {
            Console.WriteLine($"  → Smoothing mesh (all boundaries fixed)...");
            
            var boundaryNodes = new HashSet<int>();
            
            // Fix ALL boundary nodes in the cracked mesh (includes crack surfaces)
            var crackedBoundary = FindBoundaryNodes(crackedMesh);
            foreach (var node in crackedBoundary)
                boundaryNodes.Add(node);
            
            Console.WriteLine($"  → Fixed {boundaryNodes.Count} boundary nodes (mesh edges + crack surfaces)");
            
            // Fast conservative Laplacian smoothing
            var smoothedCoords = (double[,])crackedCoords.Clone();
            int iterations = 3;  // Reduced from 5
            double relaxation = 0.25;  // More conservative (was 0.5)
            
            // Build neighbor map once (FAST)
            var nodeNeighbors = BuildNodeNeighborsTri(crackedMesh);
            
            for (int iter = 0; iter < iterations; iter++)
            {
                var newCoords = (double[,])smoothedCoords.Clone();
                int rejected = 0;
                
                for (int nodeId = 0; nodeId < crackedMesh.Count<Node>(); nodeId++)
                {
                    if (boundaryNodes.Contains(nodeId)) continue;
                    
                    var neighbors = nodeNeighbors[nodeId];
                    if (neighbors.Count == 0) continue;
                    
                    // Compute centroid
                    double[] centroid = new double[coordDim];
                    foreach (var nbr in neighbors)
                        for (int d = 0; d < coordDim; d++)
                            centroid[d] += smoothedCoords[nbr, d];
                    
                    for (int d = 0; d < coordDim; d++)
                        centroid[d] /= neighbors.Count;
                    
                    // Conservative move
                    double[] newPos = new double[coordDim];
                    for (int d = 0; d < coordDim; d++)
                        newPos[d] = smoothedCoords[nodeId, d] + relaxation * (centroid[d] - smoothedCoords[nodeId, d]);
                    
                    // Quality check: reject if move inverts any adjacent triangle
                    bool valid = true;
                    var incidentTris = crackedMesh.ElementsAt<Tri3, Node>(nodeId);
                    foreach (var elemId in incidentTris)
                    {
                        var n = crackedMesh.NodesOf<Tri3, Node>(elemId);

                        // Compute area with newPos substituted for nodeId (avoids full array clone)
                        double x0 = n[0] == nodeId ? newPos[0] : smoothedCoords[n[0], 0];
                        double y0 = n[0] == nodeId ? newPos[1] : smoothedCoords[n[0], 1];
                        double x1 = n[1] == nodeId ? newPos[0] : smoothedCoords[n[1], 0];
                        double y1 = n[1] == nodeId ? newPos[1] : smoothedCoords[n[1], 1];
                        double x2 = n[2] == nodeId ? newPos[0] : smoothedCoords[n[2], 0];
                        double y2 = n[2] == nodeId ? newPos[1] : smoothedCoords[n[2], 1];

                        double area2 = (x1 - x0) * (y2 - y0) - (y1 - y0) * (x2 - x0);

                        if (area2 <= 0)
                        {
                            valid = false;
                            rejected++;
                            break;
                        }
                    }
                    
                    if (valid)
                    {
                        for (int d = 0; d < coordDim; d++)
                            newCoords[nodeId, d] = newPos[d];
                    }
                }
                
                smoothedCoords = newCoords;
            }
            
            CorrectTriangleOrientations(crackedMesh, smoothedCoords);
            return (crackedMesh, smoothedCoords);
        }
        
        CorrectTriangleOrientations(crackedMesh, crackedCoords);
        return (crackedMesh, crackedCoords);
    }
    
    /// <summary>
    /// Find boundary nodes (nodes that share boundary edges).
    /// </summary>
    public static HashSet<int> FindBoundaryNodes(SimplexMesh mesh)
    {
        var boundaryNodes = new HashSet<int>();
        
        SimplexRemesher.DiscoverEdges(mesh);
        
        // Boundary edges are shared by exactly 1 triangle
        for (int e = 0; e < mesh.Count<Edge>(); e++)
        {
            if (mesh.CountElementsSharingSubEntity<Tri3, Edge, Node>(e) == 1)
            {
                var edgeNodes = mesh.NodesOf<Edge, Node>(e);
                boundaryNodes.Add(edgeNodes[0]);
                boundaryNodes.Add(edgeNodes[1]);
            }
        }
        
        return boundaryNodes;
    }
    
    #region Crack Insertion - 3D (Tetrahedral Meshes)
    
    /// <summary>
/// Robustly finds an intersection (root) of an implicit signed field on a line segment.
/// This is needed for curved fields (e.g. cylinders) where the segment may intersect even if
/// both endpoints have the same sign (two roots are possible in general).
/// </summary>
private static bool TryFindEdgeRootOnSegment(
    SignedFieldFunction phi,
    double x1, double y1, double z1,
    double x2, double y2, double z2,
    out double tRoot,
    out double xRoot, out double yRoot, out double zRoot,
    out double phiAtRoot)
{
    // Conservative sampling to detect sign changes / near-zero points.
    // Keep this small for performance; 9 points catches most curved intersections.
    const int S = 9; // includes endpoints
    const double nearZero = 1e-12;

    double bestAbs = double.PositiveInfinity;
    double bestT = 0.5;
    double bestPhi = double.PositiveInfinity;

    double prevT = 0.0;
    double prevPhi = phi(x1, y1, z1);

    bestAbs = Math.Abs(prevPhi);
    bestT = 0.0;
    bestPhi = prevPhi;

    // Track best bracketing interval (closest to mid-segment)
    bool haveBracket = false;
    double aT = 0.0, bT = 1.0;
    double aPhi = prevPhi, bPhi = double.NaN;
    double bestBracketScore = double.PositiveInfinity;

    for (int k = 1; k < S; k++)
    {
        double t = (double)k / (S - 1);
        double x = x1 + t * (x2 - x1);
        double y = y1 + t * (y2 - y1);
        double z = z1 + t * (z2 - z1);
        double v = phi(x, y, z);

        double av = Math.Abs(v);
        if (av < bestAbs)
        {
            bestAbs = av;
            bestT = t;
            bestPhi = v;
        }

        // Bracket if sign changes or either endpoint is near zero
        if ((prevPhi == 0.0) || (v == 0.0) || (prevPhi * v < 0.0) || (Math.Abs(prevPhi) < nearZero) || (Math.Abs(v) < nearZero))
        {
            double mid = 0.5 * (prevT + t);
            double score = Math.Abs(mid - 0.5); // prefer closest to midpoint (stable, symmetric)
            if (score < bestBracketScore)
            {
                bestBracketScore = score;
                haveBracket = true;
                aT = prevT; bT = t;
                aPhi = prevPhi; bPhi = v;
            }
        }

        prevT = t;
        prevPhi = v;
    }

    // If we didn't find a bracket but we got extremely close somewhere, treat it as an intersection.
    if (!haveBracket)
    {
        if (bestAbs > 1e-9) // not close enough -> no reliable intersection
        {
            tRoot = 0; xRoot = yRoot = zRoot = 0; phiAtRoot = 0;
            return false;
        }

        tRoot = bestT;
        xRoot = x1 + tRoot * (x2 - x1);
        yRoot = y1 + tRoot * (y2 - y1);
        zRoot = z1 + tRoot * (z2 - z1);
        phiAtRoot = bestPhi;
        return true;
    }

    // Bisection on the selected bracket (works even if phi is nonlinear, as long as continuous)
    double loT = aT, hiT = bT;
    double loPhi = aPhi, hiPhi = bPhi;

    // If the bracket is degenerate (exact zero at one endpoint), return that
    if (Math.Abs(loPhi) < nearZero)
    {
        tRoot = loT;
        xRoot = x1 + tRoot * (x2 - x1);
        yRoot = y1 + tRoot * (y2 - y1);
        zRoot = z1 + tRoot * (z2 - z1);
        phiAtRoot = loPhi;
        return true;
    }
    if (Math.Abs(hiPhi) < nearZero)
    {
        tRoot = hiT;
        xRoot = x1 + tRoot * (x2 - x1);
        yRoot = y1 + tRoot * (y2 - y1);
        zRoot = z1 + tRoot * (z2 - z1);
        phiAtRoot = hiPhi;
        return true;
    }

    // Ensure opposite signs; if not, still bisect toward minimum |phi|
    bool opposite = loPhi * hiPhi < 0.0;

    double tMid = 0.5 * (loT + hiT);
    double phiMid = 0.0;

    for (int iter = 0; iter < 40; iter++)
    {
        tMid = 0.5 * (loT + hiT);
        double xm = x1 + tMid * (x2 - x1);
        double ym = y1 + tMid * (y2 - y1);
        double zm = z1 + tMid * (z2 - z1);
        phiMid = phi(xm, ym, zm);

        if (Math.Abs(phiMid) < 1e-12) break;

        if (opposite)
        {
            // standard sign bisection
            if (loPhi * phiMid < 0.0)
            {
                hiT = tMid; hiPhi = phiMid;
            }
            else
            {
                loT = tMid; loPhi = phiMid;
            }
        }
        else
        {
            // fallback: shrink interval toward smaller |phi|
            if (Math.Abs(loPhi) < Math.Abs(hiPhi))
            {
                hiT = tMid; hiPhi = phiMid;
            }
            else
            {
                loT = tMid; loPhi = phiMid;
            }
        }
    }

    tRoot = tMid;
    xRoot = x1 + tRoot * (x2 - x1);
    yRoot = y1 + tRoot * (y2 - y1);
    zRoot = z1 + tRoot * (z2 - z1);
    phiAtRoot = phiMid;
    return true;
}

public static (SimplexMesh, double[,]) CreateCrackFromSignedField3D(
        SimplexMesh mesh,
        double[,] coordinates,
        SignedFieldFunction signedField,
        SignedFieldFunction? regionField = null,
        bool enableMeshPerturbation = false,
        bool enableSmoothing = false)
    {
        Console.WriteLine($"  → Creating 3D crack with mesh refinement and node duplication...");
        Console.WriteLine($"  → Initial mesh: {mesh.Count<Node>()} nodes, {mesh.Count<Tet4>()} tetrahedra");
        
        // Find edges that cross the crack surface
        SimplexRemesher.DiscoverEdges(mesh);
        var edgesToRefine = new List<(int, int)>();
        
        int totalCrossingEdges = 0;
        int regionFilteredEdges = 0;
        
        for (int i = 0; i < mesh.Count<Edge>(); i++)
        {
            var nodes = mesh.NodesOf<Edge, Node>(i);
            int n1 = nodes[0], n2 = nodes[1];
            
            // Robust edge–surface intersection (curved implicit surfaces may intersect with same-sign endpoints)
if (TryFindEdgeRootOnSegment(signedField,
        coordinates[n1, 0], coordinates[n1, 1], coordinates[n1, 2],
        coordinates[n2, 0], coordinates[n2, 1], coordinates[n2, 2],
        out double tRoot,
        out double xRoot, out double yRoot, out double zRoot,
        out double phiRoot))
{
    totalCrossingEdges++;

    if (regionField != null)
    {
        // Filter based on the intersection point (NOT only endpoints)
        double r = regionField(xRoot, yRoot, zRoot);
        if (r <= 0)
            edgesToRefine.Add((n1, n2));
        else
            regionFilteredEdges++;
    }
    else
    {
        edgesToRefine.Add((n1, n2));
    }
}
        }
        
        Console.WriteLine($"  → Edges crossing crack surface: {totalCrossingEdges}");
        if (regionField != null)
            Console.WriteLine($"  → Filtered by region: {regionFilteredEdges}");
        Console.WriteLine($"  → Edges to refine: {edgesToRefine.Count}");
        
        if (edgesToRefine.Count == 0)
        {
            Console.WriteLine($"  ⚠️  No edges to refine - returning original mesh");
            return (mesh, coordinates);
        }
        
        // CHECK INITIAL MESH JACOBIANS (before refinement)
        Console.WriteLine($"  → Checking initial mesh quality...");
        int initialNegative = 0;
        for (int i = 0; i < mesh.Count<Tet4>(); i++)
        {
            var nodes = mesh.NodesOf<Tet4, Node>(i);
            double jac = ComputeTetrahedronJacobian(coordinates, nodes[0], nodes[1], nodes[2], nodes[3]);
            if (jac <= 0) initialNegative++;
        }
        if (initialNegative > 0)
            Console.WriteLine($"  ⚠️  Initial mesh has {initialNegative}/{mesh.Count<Tet4>()} elements with non-positive Jacobian!");
        else
            Console.WriteLine($"  ✓ Initial mesh: all elements have positive Jacobians");
        
        // Store original node count before refinement
        int originalNodeCount = mesh.Count<Node>();
        
        // Use MeshRefinement.Refine to split edges AND subdivide tetrahedra
        Console.WriteLine($"  → Calling MeshRefinement.Refine with closure enforcement...");
        var (refinedMesh, _) = MeshRefinement.Refine(mesh, edgesToRefine, 
            enforceClosureForTets: false,
            validateTopology: true,
            inputCoordinates: coordinates);
        
        Console.WriteLine($"  → Refined mesh: {refinedMesh.Count<Node>()} nodes, {refinedMesh.Count<Tet4>()} tetrahedra");
        
        // Use MeshRefinement's own coordinate interpolation
        Console.WriteLine($"  → Using MeshRefinement.InterpolateCoordinates...");
        var refinedCoords = MeshRefinement.InterpolateCoordinates(refinedMesh, coordinates);
        
        // Save intermediate refined mesh for visualization
        Console.WriteLine($"  → Saving refined mesh before snapping...");
        MeshIO.SaveGiD(refinedMesh, refinedCoords, "refined_crack_mesh.post.msh");
        
        // Snap new nodes to exact crack surface (surface = 0) ONLY IF INSIDE REGION
        Console.WriteLine($"  → Snapping crack nodes to exact zero crossing...");
        int snappedCount = 0;
        int skippedTooClose = 0;
        int skippedWouldInvert = 0;
        int skippedOutsideRegion = 0;
        double maxSnapError = 0.0;
        
        for (int i = originalNodeCount; i < refinedMesh.Count<Node>(); i++)
        {
            var parents = refinedMesh.Get<Node, ParentNodes>(i);
            
            if (parents.Parent1 != parents.Parent2)
            {
                int p1 = parents.Parent1, p2 = parents.Parent2;
                
                double x1 = refinedCoords[p1, 0], y1 = refinedCoords[p1, 1], z1 = refinedCoords[p1, 2];
                double x2 = refinedCoords[p2, 0], y2 = refinedCoords[p2, 1], z2 = refinedCoords[p2, 2];
                
                double f1 = signedField(x1, y1, z1);
                double f2 = signedField(x2, y2, z2);
                
                // Snap to zero-crossing if edge crosses crack
                if (Math.Abs(f2 - f1) > 1e-10 && f1 * f2 <= 0)
                {
                    // Current interpolated position
                    double xCurr = refinedCoords[i, 0];
                    double yCurr = refinedCoords[i, 1];
                    double zCurr = refinedCoords[i, 2];
                    double fCurr = signedField(xCurr, yCurr, zCurr);
                    
                    // Don't snap if already very close to surface
                    double edgeLength = Math.Sqrt((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1) + (z2-z1)*(z2-z1));
                    double snapTolerance = Math.Max(1e-6, 1e-5 * edgeLength); // Much tighter: 0.001% of edge
                    
                    if (Math.Abs(fCurr) < snapTolerance)
                    {
                        skippedTooClose++;
                        continue;
                    }
                    
                    // Use bisection for nonlinear fields
                    double tMin = 0.0, tMax = 1.0;
                    double t = 0.5;
                    
                    for (int iter = 0; iter < 30; iter++)  // More iterations for curved surfaces
                    {
                        t = (tMin + tMax) / 2.0;
                        double xm = x1 + t * (x2 - x1);
                        double ym = y1 + t * (y2 - y1);
                        double zm = z1 + t * (z2 - z1);
                        double fm = signedField(xm, ym, zm);
                        
                        if (Math.Abs(fm) < 1e-12) break;  // Tighter convergence
                        
                        if (fm * f1 < 0)
                        {
                            tMax = t;
                            f2 = fm;
                        }
                        else
                        {
                            tMin = t;
                            f1 = fm;
                        }
                    }
                    
                    // Check if snap distance is significant
                    double xNew = x1 + t * (x2 - x1);
                    double yNew = y1 + t * (y2 - y1);
                    double zNew = z1 + t * (z2 - z1);
                    
                    // CRITICAL FIX: Only snap if the new position is INSIDE the crack region
                    if (regionField != null)
                    {
                        double regionValue = regionField(xNew, yNew, zNew);
                        if (regionValue > 0)
                        {
                            // Outside the crack region (e.g., outside the ellipse)
                            // Do NOT snap - leave the node at its interpolated position
                            skippedOutsideRegion++;
                            continue;
                        }
                    }
                    
                    double snapDist = Math.Sqrt((xNew-xCurr)*(xNew-xCurr) + (yNew-yCurr)*(yNew-yCurr) + (zNew-zCurr)*(zNew-zCurr));
                    
                    if (snapDist > snapTolerance)
                    {
                        // Validate: check if snapping would invert any adjacent tet
                        bool wouldInvert = false;
                        
                        // TEMPORARILY DISABLED - diagnose snapping issues
                        /*
                        var incidentTets = refinedMesh.ElementsAt<Tet4, Node>(i);
                        {
                            var tempCoords = (double[,])refinedCoords.Clone();
                            tempCoords[i, 0] = xNew;
                            tempCoords[i, 1] = yNew;
                            tempCoords[i, 2] = zNew;
                            
                            foreach (var tetIdx in incidentTets)
                            {
                                var tetNodes = refinedMesh.NodesOf<Tet4, Node>(tetIdx);
                                double jac = ComputeTetrahedronJacobian(tempCoords, tetNodes[0], tetNodes[1], tetNodes[2], tetNodes[3]);
                                if (jac <= 0)
                                {
                                    wouldInvert = true;
                                    break;
                                }
                            }
                        }
                        */
                        
                        if (!wouldInvert)
                        {
                            refinedCoords[i, 0] = xNew;
                            refinedCoords[i, 1] = yNew;
                            refinedCoords[i, 2] = zNew;
                            
                            double finalError = Math.Abs(signedField(refinedCoords[i, 0], refinedCoords[i, 1], refinedCoords[i, 2]));
                            maxSnapError = Math.Max(maxSnapError, finalError);
                            snappedCount++;
                        }
                        else
                        {
                            skippedWouldInvert++;
                        }
                    }
                    else
                    {
                        skippedTooClose++;
                    }
                }
            }
        }
        
        Console.WriteLine($"  → Snapped {snappedCount} nodes to surface=0 (max error: {maxSnapError:E3})");
        if (skippedTooClose > 0)
            Console.WriteLine($"  → Skipped {skippedTooClose} nodes already close enough to surface");
        if (skippedWouldInvert > 0)
            Console.WriteLine($"  → Skipped {skippedWouldInvert} nodes that would invert tets");
        if (skippedOutsideRegion > 0)
            Console.WriteLine($"  → Skipped {skippedOutsideRegion} nodes outside crack region (tip area)");

        MeshRefinement.CheckJacobians(refinedMesh, refinedCoords, "After snapping");
        MeshRefinement.FixNegativeJacobians(refinedMesh, refinedCoords);
        MeshRefinement.CheckJacobians(refinedMesh, refinedCoords, "Final mesh");

        // ============================================================
        // NODE DUPLICATION - Element-based approach (matching 2D)
        // ============================================================
        Console.WriteLine($"  → Starting element-based crack node duplication...");
        
        // Step 1: Pre-compute signed field values for ALL nodes
        var surfaceValues = new double[refinedMesh.Count<Node>()];
        var regionValues = new double[refinedMesh.Count<Node>()];
        
        for (int i = 0; i < refinedMesh.Count<Node>(); i++)
        {
            surfaceValues[i] = signedField(refinedCoords[i, 0], refinedCoords[i, 1], refinedCoords[i, 2]);
            regionValues[i] = regionField != null 
                ? regionField(refinedCoords[i, 0], refinedCoords[i, 1], refinedCoords[i, 2]) 
                : -1.0; // Default: all nodes in active region
        }
        
        // Compute average edge length for adaptive tolerances
        double avgEdgeLen = 0;
        int numEdgesCount = 0;
        for (int t = 0; t < refinedMesh.Count<Tet4>(); t++)
        {
            var n = refinedMesh.NodesOf<Tet4, Node>(t);
            for (int i = 0; i < 4; i++)
            {
                for (int j = i + 1; j < 4; j++)
                {
                    double dx = refinedCoords[n[j], 0] - refinedCoords[n[i], 0];
                    double dy = refinedCoords[n[j], 1] - refinedCoords[n[i], 1];
                    double dz = refinedCoords[n[j], 2] - refinedCoords[n[i], 2];
                    avgEdgeLen += Math.Sqrt(dx*dx + dy*dy + dz*dz);
                    numEdgesCount++;
                }
            }
        }
        avgEdgeLen /= Math.Max(1, numEdgesCount);
        Console.WriteLine($"  → Average edge length: {avgEdgeLen:F4}");
        
        // Step 2: Identify crack nodes by SIGN CHANGE (not tolerance)
        // A node is a crack node if it was created by edge refinement (Parent1 != Parent2)
        // AND both parents have opposite signs
        
        var crackNodes = new HashSet<int>();
        
        for (int i = originalNodeCount; i < refinedMesh.Count<Node>(); i++)
        {
            var parents = refinedMesh.Get<Node, ParentNodes>(i);
            
            if (parents.Parent1 != parents.Parent2)
            {
                int p1 = parents.Parent1, p2 = parents.Parent2;
                
                double f1 = signedField(coordinates[p1, 0], coordinates[p1, 1], coordinates[p1, 2]);
                double f2 = signedField(coordinates[p2, 0], coordinates[p2, 1], coordinates[p2, 2]);
                
                // Sign change = edge crosses surface
                if (f1 * f2 < 0)
                {
                    // Check if inside region
                    if (regionField == null || regionField(refinedCoords[i, 0], refinedCoords[i, 1], refinedCoords[i, 2]) <= 0)
                    {
                        crackNodes.Add(i);
                    }
                }
            }
        }
        
        Console.WriteLine($"  → Found {crackNodes.Count} crack nodes (by sign change)");
        
        if (crackNodes.Count == 0)
        {
            Console.WriteLine($"  ⚠️  No crack nodes found - returning refined mesh");
            return (refinedMesh, refinedCoords);
        }
        
        // Step 3: Identify boundary nodes (to exclude from tip detection)
        var boundaryNodes3D = FindBoundaryNodes3D(refinedMesh);
        Console.WriteLine($"  → Found {boundaryNodes3D.Count} boundary nodes");
        
        // Step 4: Identify tip nodes (crack nodes with neighbors outside crack region)
        var tipNodes = new HashSet<int>();
        
        if (regionField != null)
        {
            // Build node-to-node connectivity from tets
            var nodeNeighbors = BuildNodeNeighborsTet(refinedMesh);
            
            // Threshold for "near region boundary" - should be wider than regionTolerance
            double regionBoundaryThreshold = 0.1 * avgEdgeLen;  // 10% of edge length
            
            // Check each crack node for:
            // 1. Neighbors outside active region (classic tip detection)
            // 2. The node itself being near the region boundary
            int tipByNeighbor = 0;
            int tipByBoundary = 0;
            
            foreach (int crackNode in crackNodes)
            {
                // Check 1: Is this node itself near the region boundary?
                if (regionValues[crackNode] > -regionBoundaryThreshold)
                {
                    // Node is near the crack front (boundary of ellipse, etc.)
                    if (!boundaryNodes3D.Contains(crackNode))
                    {
                        tipNodes.Add(crackNode);
                        tipByBoundary++;
                        continue;
                    }
                }
                
                // Check 2: Does this node have neighbors outside the region?
                bool hasFarNeighbor = false;
                foreach (int neighbor in nodeNeighbors[crackNode])
                {
                    if (regionValues[neighbor] > 0)
                    {
                        hasFarNeighbor = true;
                        break;
                    }
                }
                
                // If tip AND not on mesh boundary, don't duplicate
                if (hasFarNeighbor && !boundaryNodes3D.Contains(crackNode))
                {
                    tipNodes.Add(crackNode);
                    tipByNeighbor++;
                }
            }
            
            Console.WriteLine($"  → Tip detection: {tipByBoundary} by region boundary, {tipByNeighbor} by neighbor check");
        }
        
        Console.WriteLine($"  → Found {tipNodes.Count} tip nodes (will NOT be duplicated)");
        
        // Step 5: Nodes to duplicate = crack nodes - tip nodes
        var nodesToDuplicate = new HashSet<int>(crackNodes);
        nodesToDuplicate.ExceptWith(tipNodes);
        
        Console.WriteLine($"  → Will duplicate {nodesToDuplicate.Count} nodes");
        
        // DIAGNOSTIC: Print bounding box of crack nodes and nodes to duplicate
        if (nodesToDuplicate.Count > 0)
        {
            double minX = double.MaxValue, maxX = double.MinValue;
            double minY = double.MaxValue, maxY = double.MinValue;
            double minZ = double.MaxValue, maxZ = double.MinValue;
            double minRegion = double.MaxValue, maxRegion = double.MinValue;
            
            foreach (int nodeId in nodesToDuplicate)
            {
                minX = Math.Min(minX, refinedCoords[nodeId, 0]);
                maxX = Math.Max(maxX, refinedCoords[nodeId, 0]);
                minY = Math.Min(minY, refinedCoords[nodeId, 1]);
                maxY = Math.Max(maxY, refinedCoords[nodeId, 1]);
                minZ = Math.Min(minZ, refinedCoords[nodeId, 2]);
                maxZ = Math.Max(maxZ, refinedCoords[nodeId, 2]);
                minRegion = Math.Min(minRegion, regionValues[nodeId]);
                maxRegion = Math.Max(maxRegion, regionValues[nodeId]);
            }
            
            Console.WriteLine($"  → Crack bbox: X=[{minX:F2},{maxX:F2}], Y=[{minY:F2},{maxY:F2}], Z=[{minZ:F2},{maxZ:F2}]");
            Console.WriteLine($"  → Region values of crack nodes: [{minRegion:F3},{maxRegion:F3}]");
        }
        
        if (nodesToDuplicate.Count == 0)
        {
            Console.WriteLine($"  → No nodes to duplicate - returning refined mesh");
            return (refinedMesh, refinedCoords);
        }
        
        // Step 6: Create new mesh with duplicated nodes
        int totalNodes = refinedMesh.Count<Node>();
        int finalNodeCount = totalNodes + nodesToDuplicate.Count;
        var crackedMesh = new SimplexMesh();
        var crackedCoords = new double[finalNodeCount, 3];
        
        var originalMap = new Dictionary<int, int>();
        var duplicateMap = new Dictionary<int, int>();
        
        // Copy all original nodes
        for (int i = 0; i < totalNodes; i++)
        {
            int newId = crackedMesh.Add<Node>();
            originalMap[i] = newId;
            
            crackedCoords[newId, 0] = refinedCoords[i, 0];
            crackedCoords[newId, 1] = refinedCoords[i, 1];
            crackedCoords[newId, 2] = refinedCoords[i, 2];
            
            var parents = refinedMesh.Get<Node, ParentNodes>(i);
            crackedMesh.Set<Node, ParentNodes>(newId, parents);
        }

        // Create duplicates
        foreach (int nodeId in nodesToDuplicate)
        {
            int dupId = crackedMesh.Add<Node>();
            duplicateMap[nodeId] = dupId;

            crackedCoords[dupId, 0] = refinedCoords[nodeId, 0];
            crackedCoords[dupId, 1] = refinedCoords[nodeId, 1];
            crackedCoords[dupId, 2] = refinedCoords[nodeId, 2];

            var parents = refinedMesh.Get<Node, ParentNodes>(nodeId);
            crackedMesh.Set<Node, ParentNodes>(dupId, parents);
        }
        
        Console.WriteLine($"  → Created {totalNodes} original + {nodesToDuplicate.Count} duplicate nodes");
        
        // Step 7: Recreate elements - determine side based on nodes CLEARLY off the crack surface
        // CRITICAL: Avoid using nodes near the crack plane for side classification
        int elementsCreated = 0;
        int positiveSideElements = 0;
        int negativeSideElements = 0;
        int duplicatesUsed = 0;
        int classifiedByClearNode = 0;
        int classifiedByCentroid = 0;
        
        // Threshold for "clearly off the crack surface" - use 10% of average edge length
        double clearThreshold = 0.1 * avgEdgeLen;
        
        for (int t = 0; t < refinedMesh.Count<Tet4>(); t++)
        {
            var nodes = refinedMesh.NodesOf<Tet4, Node>(t);
            var newNodes = new int[4];
            
            // Count how many nodes are crack nodes to be duplicated
            int crackNodeCount = 0;
            for (int j = 0; j < 4; j++)
            {
                if (nodesToDuplicate.Contains(nodes[j]))
                    crackNodeCount++;
            }
            
            // If element has NO crack nodes, just use originals for all
            if (crackNodeCount == 0)
            {
                for (int j = 0; j < 4; j++)
                    newNodes[j] = originalMap[nodes[j]];
                crackedMesh.Add<Tet4, Node>(newNodes[0], newNodes[1], newNodes[2], newNodes[3]);
                elementsCreated++;
                continue;
            }
            
            // VOTING-BASED ELEMENT ASSIGNMENT
            // Count positive vs negative non-crack nodes
            int positiveVotes = 0;
            int negativeVotes = 0;
            
            for (int j = 0; j < 4; j++)
            {
                int nodeId = nodes[j];
                if (!nodesToDuplicate.Contains(nodeId))
                {
                    // Non-crack node - check which side
                    if (surfaceValues[nodeId] > 0)
                        positiveVotes++;
                    else
                        negativeVotes++;
                }
            }
            
            // Assign element to majority side
            bool usePositiveSide;
            if (positiveVotes != negativeVotes)
            {
                usePositiveSide = positiveVotes > negativeVotes;
                classifiedByClearNode++;
            }
            else
            {
                // Tie or all nodes are crack nodes - use centroid
                double cx = 0, cy = 0, cz = 0;
                for (int j = 0; j < 4; j++)
                {
                    cx += refinedCoords[nodes[j], 0];
                    cy += refinedCoords[nodes[j], 1];
                    cz += refinedCoords[nodes[j], 2];
                }
                cx /= 4; cy /= 4; cz /= 4;
                usePositiveSide = signedField(cx, cy, cz) > 0;
                classifiedByCentroid++;
            }
            
            if (usePositiveSide)
                positiveSideElements++;
            else
                negativeSideElements++;
            
            // Assign nodes
            for (int j = 0; j < 4; j++)
            {
                if (!nodesToDuplicate.Contains(nodes[j]))
                {
                    // Non-crack node - use original
                    newNodes[j] = originalMap[nodes[j]];
                }
                else
                {
                    // Crack node - use duplicate for positive, original for negative
                    if (usePositiveSide)
                    {
                        newNodes[j] = duplicateMap[nodes[j]];
                        duplicatesUsed++;
                    }
                    else
                    {
                        newNodes[j] = originalMap[nodes[j]];
                    }
                }
            }
            
            crackedMesh.Add<Tet4, Node>(newNodes[0], newNodes[1], newNodes[2], newNodes[3]);
            elementsCreated++;
        }
        
        Console.WriteLine($"  → Created {elementsCreated} tetrahedra");
        Console.WriteLine($"  → Positive side: {positiveSideElements}, Negative side: {negativeSideElements}");
        Console.WriteLine($"  → Classification: {classifiedByClearNode} by clear node, {classifiedByCentroid} by centroid");
        Console.WriteLine($"  → Duplicate nodes used {duplicatesUsed} times");
        
        refinedMesh = crackedMesh;
        refinedCoords = crackedCoords;
        
        // Step 8: Validate mesh quality
        Console.WriteLine($"  → Validating cracked mesh...");
        int negativeCount = 0;
        for (int i = 0; i < refinedMesh.Count<Tet4>(); i++)
        {
            var nodes = refinedMesh.NodesOf<Tet4, Node>(i);
            double jac = ComputeTetrahedronJacobian(refinedCoords, nodes[0], nodes[1], nodes[2], nodes[3]);
            if (jac <= 0) negativeCount++;
        }
        
        if (negativeCount > 0)
            Console.WriteLine($"  ⚠️  WARNING: {negativeCount} elements with non-positive Jacobian");
        else
            Console.WriteLine($"  ✓ All elements have positive Jacobians");
        
        // OPTIONAL: 3D SMOOTHING (with boundary and crack nodes fixed)
        if (enableSmoothing)
        {
            Console.WriteLine($"  → Applying 3D smoothing (all boundaries fixed)...");
            
            var boundaryNodes = FindBoundaryNodes3D(refinedMesh);
            Console.WriteLine($"  → Fixed {boundaryNodes.Count} boundary nodes");
            
            // Build neighbor map from tetrahedra
            var nodeNeighbors = BuildNodeNeighborsTet(refinedMesh);
            
            // Conservative Laplacian smoothing
            var smoothedCoords = (double[,])refinedCoords.Clone();
            int iterations = 3;
            double relaxation = 0.25;
            
            for (int iter = 0; iter < iterations; iter++)
            {
                var newCoords = (double[,])smoothedCoords.Clone();
                int smoothedCount = 0;
                int rejectedCount = 0;
                
                for (int nodeId = 0; nodeId < refinedMesh.Count<Node>(); nodeId++)
                {
                    if (boundaryNodes.Contains(nodeId)) continue;
                    
                    var neighbors = nodeNeighbors[nodeId];
                    if (neighbors.Count == 0) continue;
                    
                    // Compute centroid
                    double cx = 0, cy = 0, cz = 0;
                    foreach (var nbr in neighbors)
                    {
                        cx += smoothedCoords[nbr, 0];
                        cy += smoothedCoords[nbr, 1];
                        cz += smoothedCoords[nbr, 2];
                    }
                    cx /= neighbors.Count;
                    cy /= neighbors.Count;
                    cz /= neighbors.Count;
                    
                    // Conservative move
                    double newX = smoothedCoords[nodeId, 0] + relaxation * (cx - smoothedCoords[nodeId, 0]);
                    double newY = smoothedCoords[nodeId, 1] + relaxation * (cy - smoothedCoords[nodeId, 1]);
                    double newZ = smoothedCoords[nodeId, 2] + relaxation * (cz - smoothedCoords[nodeId, 2]);
                    
                    // Quality check: reject if move inverts any adjacent tet
                    bool valid = true;
                    foreach (var tetId in refinedMesh.ElementsAt<Tet4, Node>(nodeId))
                    {
                        var tn = refinedMesh.NodesOf<Tet4, Node>(tetId);
                        
                        // Temporarily update coords for this node
                        double oldX = smoothedCoords[nodeId, 0];
                        double oldY = smoothedCoords[nodeId, 1];
                        double oldZ = smoothedCoords[nodeId, 2];
                        
                        smoothedCoords[nodeId, 0] = newX;
                        smoothedCoords[nodeId, 1] = newY;
                        smoothedCoords[nodeId, 2] = newZ;
                        
                        double jac = ComputeTetrahedronJacobian(smoothedCoords, tn[0], tn[1], tn[2], tn[3]);
                        
                        // Restore
                        smoothedCoords[nodeId, 0] = oldX;
                        smoothedCoords[nodeId, 1] = oldY;
                        smoothedCoords[nodeId, 2] = oldZ;
                        
                        if (jac <= 0)
                        {
                            valid = false;
                            rejectedCount++;
                            break;
                        }
                    }
                    
                    if (valid)
                    {
                        newCoords[nodeId, 0] = newX;
                        newCoords[nodeId, 1] = newY;
                        newCoords[nodeId, 2] = newZ;
                        smoothedCount++;
                    }
                }
                
                smoothedCoords = newCoords;
                Console.WriteLine($"     Iteration {iter + 1}/{iterations}: smoothed {smoothedCount} nodes, rejected {rejectedCount}");
            }
            
            refinedCoords = smoothedCoords;
            Console.WriteLine($"  ✓ 3D smoothing complete");
        }
        
        return (refinedMesh, refinedCoords);
    }
    public static HashSet<int> FindBoundaryNodes3D(SimplexMesh mesh)
    {
        var boundaryNodes = new HashSet<int>();
        
        // Count how many tetrahedra share each face
        var faceCount = new Dictionary<(int, int, int), int>();
        
        for (int i = 0; i < mesh.Count<Tet4>(); i++)
        {
            var nodes = mesh.NodesOf<Tet4, Node>(i);
            
            // Four faces per tetrahedron
            var faces = new[]
            {
                (nodes[0], nodes[1], nodes[2]),
                (nodes[0], nodes[1], nodes[3]),
                (nodes[0], nodes[2], nodes[3]),
                (nodes[1], nodes[2], nodes[3])
            };
            
            foreach (var face in faces)
            {
                // Sort to get canonical form
                var sorted = new[] { face.Item1, face.Item2, face.Item3 };
                Array.Sort(sorted);
                var key = (sorted[0], sorted[1], sorted[2]);
                
                if (!faceCount.ContainsKey(key))
                    faceCount[key] = 0;
                faceCount[key]++;
            }
        }
        
        // Boundary faces have count == 1
        foreach (var (face, count) in faceCount)
        {
            if (count == 1)
            {
                boundaryNodes.Add(face.Item1);
                boundaryNodes.Add(face.Item2);
                boundaryNodes.Add(face.Item3);
            }
        }
        
        return boundaryNodes;
    }
    
    #endregion
    
    #region Node Connectivity Helpers
    
    /// <summary>
    /// Build node-to-node connectivity from triangles.
    /// Returns a dictionary mapping each node to its set of topological neighbors.
    /// </summary>
    private static Dictionary<int, HashSet<int>> BuildNodeNeighborsTri(SimplexMesh mesh)
    {
        var neighbors = new Dictionary<int, HashSet<int>>();
        for (int i = 0; i < mesh.Count<Node>(); i++)
            neighbors[i] = new HashSet<int>();
        
        for (int elemId = 0; elemId < mesh.Count<Tri3>(); elemId++)
        {
            var n = mesh.NodesOf<Tri3, Node>(elemId);
            neighbors[n[0]].Add(n[1]); neighbors[n[0]].Add(n[2]);
            neighbors[n[1]].Add(n[0]); neighbors[n[1]].Add(n[2]);
            neighbors[n[2]].Add(n[0]); neighbors[n[2]].Add(n[1]);
        }
        return neighbors;
    }
    
    /// <summary>
    /// Build node-to-node connectivity from tetrahedra.
    /// Returns a dictionary mapping each node to its set of topological neighbors.
    /// </summary>
    private static Dictionary<int, HashSet<int>> BuildNodeNeighborsTet(SimplexMesh mesh)
    {
        var neighbors = new Dictionary<int, HashSet<int>>();
        for (int i = 0; i < mesh.Count<Node>(); i++)
            neighbors[i] = new HashSet<int>();
        
        for (int elemId = 0; elemId < mesh.Count<Tet4>(); elemId++)
        {
            var n = mesh.NodesOf<Tet4, Node>(elemId);
            for (int i = 0; i < 4; i++)
                for (int j = i + 1; j < 4; j++)
                {
                    neighbors[n[i]].Add(n[j]);
                    neighbors[n[j]].Add(n[i]);
                }
        }
        return neighbors;
    }
    
    #endregion

    #region Orientation Correction

    /// <summary>Correct triangle orientations in-place to ensure CCW (positive signed area)</summary>
    private static void CorrectTriangleOrientations(SimplexMesh mesh, double[,] coords)
    {
        int flipped = 0;
        
        for (int i = 0; i < mesh.Count<Tri3>(); i++)
        {
            var n = mesh.NodesOf<Tri3, Node>(i);
            double area2 = (coords[n[1],0] - coords[n[0],0]) * (coords[n[2],1] - coords[n[0],1]) -
                          (coords[n[1],1] - coords[n[0],1]) * (coords[n[2],0] - coords[n[0],0]);
            
            const double tol = 1e-14;
            if (area2 < -tol)
            {
                // CW → CCW: swap nodes 1 and 2 (in-place)
                mesh.ReplaceElementNodes<Tri3, Node>(i, n[0], n[2], n[1]);
                flipped++;
            }
        }
        
        if (flipped > 0)
            Console.WriteLine($"  → Corrected {flipped} CW→CCW triangles");
    }

    /// <summary>Correct quad orientations in-place to ensure positive Jacobians</summary>
    private static void CorrectQuadOrientations(SimplexMesh mesh, double[,] coords)
    {
        int flipped = 0;
        
        for (int i = 0; i < mesh.Count<Quad4>(); i++)
        {
            var n = mesh.NodesOf<Quad4, Node>(i);
            
            double x1 = coords[n[0],0], y1 = coords[n[0],1];
            double x2 = coords[n[1],0], y2 = coords[n[1],1];
            double x3 = coords[n[2],0], y3 = coords[n[2],1];
            double x4 = coords[n[3],0], y4 = coords[n[3],1];
            
            double dxdxi = 0.25 * (-x1 + x2 + x3 - x4);
            double dydxi = 0.25 * (-y1 + y2 + y3 - y4);
            double dxdeta = 0.25 * (-x1 - x2 + x3 + x4);
            double dydeta = 0.25 * (-y1 - y2 + y3 + y4);
            
            double jac = dxdxi * dydeta - dydxi * dxdeta;
            
            if (jac < 0)
            {
                mesh.ReplaceElementNodes<Quad4, Node>(i, n[0], n[3], n[2], n[1]);
                flipped++;
            }
        }
        
        if (flipped > 0)
            Console.WriteLine($"  → Corrected {flipped} inverted quads");
    }

    /// <summary>Correct tetrahedron orientations in-place to ensure positive volumes</summary>
    private static void CorrectTetOrientations(SimplexMesh mesh, double[,] coords)
    {
        int flipped = 0;
        
        for (int i = 0; i < mesh.Count<Tet4>(); i++)
        {
            var n = mesh.NodesOf<Tet4, Node>(i);
            
            double ax = coords[n[1],0] - coords[n[0],0];
            double ay = coords[n[1],1] - coords[n[0],1];
            double az = coords[n[1],2] - coords[n[0],2];
            double bx = coords[n[2],0] - coords[n[0],0];
            double by = coords[n[2],1] - coords[n[0],1];
            double bz = coords[n[2],2] - coords[n[0],2];
            double cx = coords[n[3],0] - coords[n[0],0];
            double cy = coords[n[3],1] - coords[n[0],1];
            double cz = coords[n[3],2] - coords[n[0],2];
            
            double vol6 = ax*(by*cz - bz*cy) - ay*(bx*cz - bz*cx) + az*(bx*cy - by*cx);
            
            if (vol6 < 0)
            {
                // Swap nodes 0 and 1 to fix orientation (in-place)
                mesh.ReplaceElementNodes<Tet4, Node>(i, n[1], n[0], n[2], n[3]);
                flipped++;
            }
        }
        
        if (flipped > 0)
            Console.WriteLine($"  → Corrected {flipped} inverted tetrahedra");
    }
    
    private static (int, int, int) CanonicalFace(int n1, int n2, int n3)
    {
        var sorted = new[] { n1, n2, n3 };
        Array.Sort(sorted);
        return (sorted[0], sorted[1], sorted[2]);
    }
    
    #endregion

    // ═══════════════════════════════════════════════════════════════════════════
    // Crack Duplication (was CrackDuplication)
    // ═══════════════════════════════════════════════════════════════════════════

    public static (SimplexMesh mesh, double[,] coords) CreateCrack(
        SimplexMesh mesh, double[,] coords,
        List<(int, int)> crackEdges,
        Func<double, double, double> levelSetFunction,
        int smoothingIterations = 5)
    {
        var originalNodeCount = mesh.Count<Node>();

        Console.WriteLine($"[CreateCrack] Refining {crackEdges.Count} crack edges...");
        var (refinedMesh, _) = MeshRefinement.Refine(mesh, crackEdges.ToList());
        var refinedCoords = MeshRefinement.InterpolateCoordinates(refinedMesh, coords);

        Console.WriteLine($"[CreateCrack] After refinement: {refinedMesh.Count<Node>()} nodes");

        var newNodes = IdentifyCrackMidpointNodes(refinedMesh, originalNodeCount);
        Console.WriteLine($"[CreateCrack] Found {newNodes.Count} new midpoint nodes");

        if (smoothingIterations > 0)
        {
            Console.WriteLine($"[CreateCrack] Smoothing mesh ({smoothingIterations} iterations)...");
            refinedCoords = MeshSmoothing.LaplacianSmoothing(refinedMesh, refinedCoords, smoothingIterations, newNodes);
        }

        var tipNodes = IdentifyCrackTipNodes(refinedMesh, newNodes, crackEdges);
        Console.WriteLine($"[CreateCrack] Identified {tipNodes.Count} tip nodes (will NOT be duplicated)");

        var nodesToDuplicate = new HashSet<int>(newNodes);
        nodesToDuplicate.ExceptWith(tipNodes);

        Console.WriteLine($"[CreateCrack] Duplicating {nodesToDuplicate.Count} interior crack nodes...");

        var (crackedMesh, crackedCoords) = DuplicateCrackNodesAndAssignSides(
            refinedMesh, refinedCoords, nodesToDuplicate, newNodes, levelSetFunction);

        return RenumberCrackedMesh(crackedMesh, crackedCoords);
    }

    public static (SimplexMesh mesh, double[,] coords) CreateCrackFromRefinedMesh(
        SimplexMesh refinedMesh, double[,] refinedCoords,
        int originalNodeCount,
        List<(int, int)> crackEdges,
        Func<double, double, double> levelSetFunction,
        int smoothingIterations = 5)
    {
        Console.WriteLine("[CreateCrackFromRefinedMesh] Using pre-refined mesh with exact geometry");

        var newNodes = IdentifyCrackMidpointNodes(refinedMesh, originalNodeCount);
        Console.WriteLine($"[CreateCrackFromRefinedMesh] Found {newNodes.Count} crack nodes");

        DiagnoseCrackZeroAreaTriangles(refinedMesh, refinedCoords);

        var nodeMapping = new Dictionary<int, int>();
        (refinedMesh, refinedCoords, nodeMapping) = MergeCrackDuplicateNodes(refinedMesh, refinedCoords, newNodes);

        if (nodeMapping.Count > 0)
        {
            var updatedNewNodes = new HashSet<int>();
            foreach (var node in newNodes)
            {
                var canonical = node;
                while (nodeMapping.ContainsKey(canonical)) canonical = nodeMapping[canonical];
                updatedNewNodes.Add(canonical);
            }
            newNodes = updatedNewNodes;
        }

        (refinedMesh, refinedCoords) = MeshSmoothing.RemoveDegenerateTriangles(refinedMesh, refinedCoords);

        if (smoothingIterations > 0)
        {
            refinedCoords = MeshSmoothing.LaplacianSmoothing(refinedMesh, refinedCoords, smoothingIterations, newNodes);
        }

        var tipNodes = IdentifyCrackTipNodes(refinedMesh, newNodes, crackEdges);

        var nodesToDuplicate = new HashSet<int>(newNodes);
        nodesToDuplicate.ExceptWith(tipNodes);

        var (crackedMesh, crackedCoords) = DuplicateCrackNodesAndAssignSides(
            refinedMesh, refinedCoords, nodesToDuplicate, newNodes, levelSetFunction);

        return RenumberCrackedMesh(crackedMesh, crackedCoords);
    }

    private static HashSet<int> IdentifyCrackMidpointNodes(SimplexMesh mesh, int originalNodeCount)
    {
        var midpointNodes = new HashSet<int>();
        for (var i = originalNodeCount; i < mesh.Count<Node>(); i++)
        {
            var parents = mesh.Get<Node, ParentNodes>(i);
            if (parents.Parent1 != parents.Parent2) midpointNodes.Add(i);
        }
        return midpointNodes;
    }

    private static HashSet<int> IdentifyCrackTipNodes(
        SimplexMesh mesh, HashSet<int> newNodes, List<(int, int)> originalCrackEdges)
    {
        var tipNodes = new HashSet<int>();
        var refinedCrackEdges = new HashSet<(int, int)>();

        foreach (var (n0, n1) in originalCrackEdges)
        {
            var nodesOnEdge = new List<int>();
            foreach (var nodeId in newNodes)
            {
                var parents = mesh.Get<Node, ParentNodes>(nodeId);
                if ((parents.Parent1 == n0 && parents.Parent2 == n1) ||
                    (parents.Parent1 == n1 && parents.Parent2 == n0))
                    nodesOnEdge.Add(nodeId);
            }

            if (nodesOnEdge.Count > 0)
            {
                nodesOnEdge.Sort();
                for (var i = 0; i < nodesOnEdge.Count - 1; i++)
                {
                    var edge = (Math.Min(nodesOnEdge[i], nodesOnEdge[i + 1]),
                        Math.Max(nodesOnEdge[i], nodesOnEdge[i + 1]));
                    refinedCrackEdges.Add(edge);
                }
            }
        }

        var edgeCount = new Dictionary<int, int>();
        foreach (var (n0, n1) in refinedCrackEdges)
        {
            if (newNodes.Contains(n0)) edgeCount[n0] = edgeCount.GetValueOrDefault(n0, 0) + 1;
            if (newNodes.Contains(n1)) edgeCount[n1] = edgeCount.GetValueOrDefault(n1, 0) + 1;
        }

        foreach (var (nodeId, count) in edgeCount)
            if (count == 1) tipNodes.Add(nodeId);

        return tipNodes;
    }

    private static (SimplexMesh mesh, double[,] coords) DuplicateCrackNodesAndAssignSides(
        SimplexMesh mesh, double[,] coords,
        HashSet<int> nodesToDuplicate, HashSet<int> allCrackNodes,
        Func<double, double, double> levelSetFunction)
    {
        var nNodes = mesh.Count<Node>();
        var nNewNodes = nNodes + nodesToDuplicate.Count;
        var newCoords = new double[nNewNodes, 3];

        for (var i = 0; i < nNodes; i++)
        {
            newCoords[i, 0] = coords[i, 0]; newCoords[i, 1] = coords[i, 1]; newCoords[i, 2] = coords[i, 2];
        }

        var nodeDuplicates = new Dictionary<int, int>();
        var nextId = nNodes;

        foreach (var nodeId in nodesToDuplicate)
        {
            var dupId = nextId++;
            nodeDuplicates[nodeId] = dupId;
            newCoords[dupId, 0] = coords[nodeId, 0]; newCoords[dupId, 1] = coords[nodeId, 1]; newCoords[dupId, 2] = coords[nodeId, 2];
        }

        Console.WriteLine($"[DuplicateNodes] Created {nodeDuplicates.Count} duplicate nodes, total: {nNodes} -> {nNewNodes}");

        var newMesh = new SimplexMesh();
        newMesh.WithBatch(() =>
        {
            for (var i = 0; i < nNewNodes; i++) newMesh.AddNode(i);
            CopyCrackElementsWithSideAssignment(mesh, newMesh, allCrackNodes, nodeDuplicates, newCoords, levelSetFunction);
        });

        return (newMesh, newCoords);
    }

    private static void CopyCrackElementsWithSideAssignment(
        SimplexMesh mesh, SimplexMesh newMesh,
        HashSet<int> crackNodes, Dictionary<int, int> nodeDuplicates,
        double[,] coords, Func<double, double, double> levelSet)
    {
        int RemapNode(int nodeId, bool useDup)
        {
            if (useDup && nodeDuplicates.ContainsKey(nodeId)) return nodeDuplicates[nodeId];
            return nodeId;
        }

        bool ShouldUseDuplicates(IReadOnlyList<int> nodes)
        {
            for (var i = 0; i < nodes.Count; i++)
            {
                var nodeId = nodes[i];
                if (!crackNodes.Contains(nodeId))
                {
                    var phi = levelSet(coords[nodeId, 0], coords[nodeId, 1]);
                    if (phi <= 0) return false;
                }
            }
            return true;
        }

        for (var i = 0; i < mesh.Count<Point>(); i++)
        {
            var nodes = mesh.NodesOf<Point, Node>(i);
            var idx = newMesh.AddPoint(nodes[0]);
            newMesh.Set<Point, OriginalElement>(idx, mesh.Get<Point, OriginalElement>(i));
        }

        for (var i = 0; i < mesh.Count<Tri3>(); i++)
        {
            var nodes = mesh.NodesOf<Tri3, Node>(i);
            var useDup = ShouldUseDuplicates(nodes);
            var idx = newMesh.AddTriangle(RemapNode(nodes[0], useDup), RemapNode(nodes[1], useDup), RemapNode(nodes[2], useDup));
            newMesh.Set<Tri3, OriginalElement>(idx, mesh.Get<Tri3, OriginalElement>(i));
        }

        for (var i = 0; i < mesh.Count<Tet4>(); i++)
        {
            var nodes = mesh.NodesOf<Tet4, Node>(i);
            var useDup = ShouldUseDuplicates(nodes);
            var idx = newMesh.AddTetrahedron(RemapNode(nodes[0], useDup), RemapNode(nodes[1], useDup),
                RemapNode(nodes[2], useDup), RemapNode(nodes[3], useDup));
            newMesh.Set<Tet4, OriginalElement>(idx, mesh.Get<Tet4, OriginalElement>(i));
        }
    }

    private static (SimplexMesh mesh, double[,] coords) RenumberCrackedMesh(SimplexMesh mesh, double[,] coords)
    {
        var nNodes = mesh.Count<Node>();
        var usedNodes = new HashSet<int>();

        for (var i = 0; i < mesh.Count<Tri3>(); i++) foreach (var n in mesh.NodesOf<Tri3, Node>(i)) usedNodes.Add(n);
        for (var i = 0; i < mesh.Count<Tet4>(); i++) foreach (var n in mesh.NodesOf<Tet4, Node>(i)) usedNodes.Add(n);
        for (var i = 0; i < mesh.Count<Bar2>(); i++) foreach (var n in mesh.NodesOf<Bar2, Node>(i)) usedNodes.Add(n);
        for (var i = 0; i < mesh.Count<Point>(); i++) foreach (var n in mesh.NodesOf<Point, Node>(i)) usedNodes.Add(n);

        var sortedNodes = usedNodes.OrderBy(x => x).ToList();
        var needsRenumbering = sortedNodes.Count != nNodes;

        if (!needsRenumbering)
        {
            for (int i = 0; i < sortedNodes.Count; i++)
                if (sortedNodes[i] != i) { needsRenumbering = true; break; }
        }

        if (!needsRenumbering) return (mesh, coords);

        for (int i = mesh.Count<Edge>() - 1; i >= 0; i--) mesh.Remove<Edge>(i);
        for (int i = 0; i < nNodes; i++) if (!usedNodes.Contains(i)) mesh.Remove<Node>(i);
        mesh.Compress();

        var newCoords = new double[sortedNodes.Count, 3];
        for (int i = 0; i < sortedNodes.Count; i++)
        {
            var oldId = sortedNodes[i];
            newCoords[i, 0] = coords[oldId, 0]; newCoords[i, 1] = coords[oldId, 1]; newCoords[i, 2] = coords[oldId, 2];
        }

        Console.WriteLine($"[RenumberMesh] Renumbered: {nNodes} -> {sortedNodes.Count} nodes");
        return (mesh, newCoords);
    }

    private static (SimplexMesh mesh, double[,] coords, Dictionary<int, int> mapping)
        MergeCrackDuplicateNodes(SimplexMesh mesh, double[,] coords, HashSet<int> crackNodes, double tolerance = 1e-12)
    {
        var nNodes = mesh.Count<Node>();
        var nodeMapping = new Dictionary<int, int>();
        var spatialBuckets = new Dictionary<(int, int, int), List<int>>();
        var bucketSize = Math.Max(tolerance * 100, 1e-6);

        for (var i = 0; i < nNodes; i++)
        {
            var key = ((int)Math.Floor(coords[i, 0] / bucketSize),
                       (int)Math.Floor(coords[i, 1] / bucketSize),
                       (int)Math.Floor(coords[i, 2] / bucketSize));
            if (!spatialBuckets.ContainsKey(key)) spatialBuckets[key] = new List<int>();
            spatialBuckets[key].Add(i);
        }

        var merged = new HashSet<int>();
        var mergeCount = 0;

        foreach (var bucket in spatialBuckets.Values)
        {
            if (bucket.Count < 2) continue;
            for (var i = 0; i < bucket.Count; i++)
            {
                var id1 = bucket[i];
                if (merged.Contains(id1)) continue;
                for (var j = i + 1; j < bucket.Count; j++)
                {
                    var id2 = bucket[j];
                    if (merged.Contains(id2)) continue;
                    var dx = coords[id1, 0] - coords[id2, 0];
                    var dy = coords[id1, 1] - coords[id2, 1];
                    var dz = coords[id1, 2] - coords[id2, 2];
                    var dist = Math.Sqrt(dx * dx + dy * dy + dz * dz);
                    if (dist < tolerance)
                    {
                        if (!(crackNodes.Contains(id1) && crackNodes.Contains(id2)))
                        {
                            var canonicalId = Math.Min(id1, id2);
                            var mergedId = Math.Max(id1, id2);
                            nodeMapping[mergedId] = canonicalId;
                            merged.Add(mergedId);
                            mergeCount++;
                        }
                    }
                }
            }
        }

        if (mergeCount == 0) return (mesh, coords, nodeMapping);

        var GetCanonicalNode = (int nodeId) =>
        {
            while (nodeMapping.ContainsKey(nodeId)) nodeId = nodeMapping[nodeId];
            return nodeId;
        };

        for (var i = 0; i < mesh.Count<Tri3>(); i++)
        {
            var nodes = mesh.NodesOf<Tri3, Node>(i);
            var c0 = GetCanonicalNode(nodes[0]); var c1 = GetCanonicalNode(nodes[1]); var c2 = GetCanonicalNode(nodes[2]);
            if (c0 != nodes[0] || c1 != nodes[1] || c2 != nodes[2])
                mesh.ReplaceElementNodes<Tri3, Node>(i, c0, c1, c2);
        }

        for (var i = 0; i < mesh.Count<Tet4>(); i++)
        {
            var nodes = mesh.NodesOf<Tet4, Node>(i);
            var c0 = GetCanonicalNode(nodes[0]); var c1 = GetCanonicalNode(nodes[1]);
            var c2 = GetCanonicalNode(nodes[2]); var c3 = GetCanonicalNode(nodes[3]);
            if (c0 != nodes[0] || c1 != nodes[1] || c2 != nodes[2] || c3 != nodes[3])
                mesh.ReplaceElementNodes<Tet4, Node>(i, c0, c1, c2, c3);
        }

        var usedNodes = new HashSet<int>();
        for (var i = 0; i < mesh.Count<Tri3>(); i++) foreach (var n in mesh.NodesOf<Tri3, Node>(i)) usedNodes.Add(n);
        for (var i = 0; i < mesh.Count<Tet4>(); i++) foreach (var n in mesh.NodesOf<Tet4, Node>(i)) usedNodes.Add(n);

        for (int i = mesh.Count<Edge>() - 1; i >= 0; i--) mesh.Remove<Edge>(i);
        for (int i = 0; i < nNodes; i++) if (!usedNodes.Contains(i)) mesh.Remove<Node>(i);

        var sortedUsed = usedNodes.OrderBy(x => x).ToList();
        mesh.Compress();

        var newCoords = new double[sortedUsed.Count, 3];
        for (int i = 0; i < sortedUsed.Count; i++)
        {
            var oldId = sortedUsed[i];
            newCoords[i, 0] = coords[oldId, 0]; newCoords[i, 1] = coords[oldId, 1]; newCoords[i, 2] = coords[oldId, 2];
        }

        var compactMapping = new Dictionary<int, int>();
        for (int i = 0; i < sortedUsed.Count; i++) compactMapping[sortedUsed[i]] = i;

        var finalMapping = new Dictionary<int, int>();
        foreach (var (oldId, _) in nodeMapping)
        {
            var finalCanonical = GetCanonicalNode(oldId);
            if (compactMapping.ContainsKey(finalCanonical))
                finalMapping[oldId] = compactMapping[finalCanonical];
        }

        Console.WriteLine($"[MergeDuplicateNodes] Merged {mergeCount} nodes: {nNodes} -> {mesh.Count<Node>()} nodes");
        return (mesh, newCoords, finalMapping);
    }

    private static void DiagnoseCrackZeroAreaTriangles(SimplexMesh mesh, double[,] coords, double tolerance = 1e-10)
    {
        var zeroAreaCount = 0;
        for (var i = 0; i < mesh.Count<Tri3>(); i++)
        {
            var nodes = mesh.NodesOf<Tri3, Node>(i);
            if (IsTriangleDegenerate(coords, nodes[0], nodes[1], nodes[2], tolerance))
                zeroAreaCount++;
        }

        if (zeroAreaCount > 0)
            Console.WriteLine($"[Diagnostic] Found {zeroAreaCount} zero-area triangles!");
        else
            Console.WriteLine("[Diagnostic] No zero-area triangles found");
    }
}
}
