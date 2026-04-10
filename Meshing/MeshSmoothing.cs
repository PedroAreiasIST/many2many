// MeshSmoothing.cs
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

using System;
using System.Collections.Generic;
using System.Linq;
using static Numerical.MeshGeometry;
using static Numerical.MeshConstants;

namespace Numerical;

public static class MeshSmoothing
{
    public static double[,] LaplacianSmoothing(
        SimplexMesh mesh, double[,] coords,
        int iterations = 5, HashSet<int>? fixedNodes = null,
        double relaxation = 1.0, bool keepBoundaryFixed = true)
    {
        var nNodes = coords.GetLength(0);
        var newCoords = (double[,])coords.Clone();

        var boundaryNodes = keepBoundaryFixed ? IdentifyBoundaryNodes(mesh) : new HashSet<int>();
        var allFixedNodes = new HashSet<int>(boundaryNodes);
        if (fixedNodes != null) allFixedNodes.UnionWith(fixedNodes);

        Console.WriteLine($"[LaplacianSmoothing] Starting {iterations} iterations...");
        Console.WriteLine($"[LaplacianSmoothing] Fixed nodes: {allFixedNodes.Count}/{nNodes}");

        for (var iter = 0; iter < iterations; iter++)
        {
            var neighbors = BuildNodeNeighbors(mesh, nNodes);
            var smoothedCount = 0;

            for (var nodeId = 0; nodeId < nNodes; nodeId++)
            {
                if (allFixedNodes.Contains(nodeId)) continue;

                var nodeNeighbors = neighbors[nodeId];
                if (nodeNeighbors.Count == 0) continue;

                double avgX = 0, avgY = 0, avgZ = 0;
                foreach (var neighborId in nodeNeighbors)
                {
                    avgX += newCoords[neighborId, 0];
                    avgY += newCoords[neighborId, 1];
                    avgZ += newCoords[neighborId, 2];
                }

                var count = nodeNeighbors.Count;
                avgX /= count;
                avgY /= count;
                avgZ /= count;

                newCoords[nodeId, 0] += relaxation * (avgX - newCoords[nodeId, 0]);
                newCoords[nodeId, 1] += relaxation * (avgY - newCoords[nodeId, 1]);
                newCoords[nodeId, 2] += relaxation * (avgZ - newCoords[nodeId, 2]);

                smoothedCount++;
            }

            if ((iter + 1) % Math.Max(1, iterations / 5) == 0 || iter == iterations - 1)
                Console.WriteLine($"[LaplacianSmoothing] Iteration {iter + 1}/{iterations}: smoothed {smoothedCount} nodes");
        }

        Console.WriteLine("[LaplacianSmoothing] Complete");
        return newCoords;
    }

    public static double[,] CVTSmoothing(
        SimplexMesh mesh, double[,] coords,
        int iterations = 5, HashSet<int>? fixedNodes = null, double relaxation = 1.0)
    {
        var nNodes = coords.GetLength(0);
        var newCoords = (double[,])coords.Clone();

        var boundaryNodes = IdentifyBoundaryNodes(mesh);
        var allFixedNodes = new HashSet<int>(boundaryNodes);
        if (fixedNodes != null) allFixedNodes.UnionWith(fixedNodes);

        Console.WriteLine($"[CVTSmoothing] Starting {iterations} iterations...");

        for (var iter = 0; iter < iterations; iter++)
        {
            var nodeTris = BuildNodeToTriangles(mesh, nNodes);
            var smoothedCount = 0;

            for (var nodeId = 0; nodeId < nNodes; nodeId++)
            {
                if (allFixedNodes.Contains(nodeId)) continue;

                var nodeTriangles = nodeTris[nodeId];
                if (nodeTriangles.Count == 0) continue;

                double totalArea = 0;
                double centroidX = 0, centroidY = 0;

                foreach (var triId in nodeTriangles)
                {
                    var nodes = mesh.NodesOf<Tri3, Node>(triId);

                    var cx = (newCoords[nodes[0], 0] + newCoords[nodes[1], 0] + newCoords[nodes[2], 0]) / 3.0;
                    var cy = (newCoords[nodes[0], 1] + newCoords[nodes[1], 1] + newCoords[nodes[2], 1]) / 3.0;

                    var area = ComputeTriangleArea(newCoords, nodes[0], nodes[1], nodes[2]);

                    totalArea += area;
                    centroidX += area * cx;
                    centroidY += area * cy;
                }

                if (totalArea > Epsilon)
                {
                    centroidX /= totalArea;
                    centroidY /= totalArea;

                    newCoords[nodeId, 0] += relaxation * (centroidX - newCoords[nodeId, 0]);
                    newCoords[nodeId, 1] += relaxation * (centroidY - newCoords[nodeId, 1]);

                    smoothedCount++;
                }
            }

            if ((iter + 1) % Math.Max(1, iterations / 5) == 0 || iter == iterations - 1)
                Console.WriteLine($"[CVTSmoothing] Iteration {iter + 1}/{iterations}: smoothed {smoothedCount} nodes");
        }

        Console.WriteLine("[CVTSmoothing] Complete");
        return newCoords;
    }

    public static (SimplexMesh mesh, double[,] coords) RemoveDegenerateTriangles(
        SimplexMesh mesh, double[,] coords, double tolerance = Epsilon)
    {
        Console.WriteLine($"[RemoveDegenerateTriangles] Checking {mesh.Count<Tri3>()} triangles...");

        var degenerateTris = new HashSet<int>();

        for (var i = 0; i < mesh.Count<Tri3>(); i++)
        {
            var nodes = mesh.NodesOf<Tri3, Node>(i);
            if (IsTriangleDegenerate(coords, nodes[0], nodes[1], nodes[2], tolerance))
                degenerateTris.Add(i);
        }

        Console.WriteLine($"[RemoveDegenerateTriangles] Found {degenerateTris.Count} degenerate triangles");

        if (degenerateTris.Count == 0)
            return (mesh, coords);

        return RebuildMeshWithoutElements(mesh, coords, degenerateTris, new HashSet<int>());
    }

    public static (SimplexMesh mesh, double[,] coords) RemoveDegenerateTetrahedra(
        SimplexMesh mesh, double[,] coords, double tolerance = Epsilon)
    {
        Console.WriteLine($"[RemoveDegenerateTetrahedra] Checking {mesh.Count<Tet4>()} tetrahedra...");

        var degenerateTets = new HashSet<int>();

        for (var i = 0; i < mesh.Count<Tet4>(); i++)
        {
            var nodes = mesh.NodesOf<Tet4, Node>(i);
            if (IsTetrahedronDegenerate(coords, nodes[0], nodes[1], nodes[2], nodes[3], tolerance))
                degenerateTets.Add(i);
        }

        Console.WriteLine($"[RemoveDegenerateTetrahedra] Found {degenerateTets.Count} degenerate tetrahedra");

        if (degenerateTets.Count == 0)
            return (mesh, coords);

        return RebuildMeshWithoutElements(mesh, coords, new HashSet<int>(), degenerateTets);
    }

    public static HashSet<int> IdentifyBoundaryNodes(SimplexMesh mesh)
    {
        var boundaryNodes = new HashSet<int>();

        if (mesh.Count<Tri3>() > 0)
        {
            var edgeCount = new Dictionary<(int, int), int>();

            for (var i = 0; i < mesh.Count<Tri3>(); i++)
            {
                var nodes = mesh.NodesOf<Tri3, Node>(i);
                IncrementEdgeCount(edgeCount, nodes[0], nodes[1]);
                IncrementEdgeCount(edgeCount, nodes[1], nodes[2]);
                IncrementEdgeCount(edgeCount, nodes[2], nodes[0]);
            }

            foreach (var (edge, count) in edgeCount)
            {
                if (count == 1)
                {
                    boundaryNodes.Add(edge.Item1);
                    boundaryNodes.Add(edge.Item2);
                }
            }
        }

        if (mesh.Count<Tet4>() > 0)
        {
            var faceCount = new Dictionary<(int, int, int), int>();

            for (var i = 0; i < mesh.Count<Tet4>(); i++)
            {
                var nodes = mesh.NodesOf<Tet4, Node>(i);
                IncrementFaceCount(faceCount, nodes[0], nodes[1], nodes[2]);
                IncrementFaceCount(faceCount, nodes[0], nodes[1], nodes[3]);
                IncrementFaceCount(faceCount, nodes[0], nodes[2], nodes[3]);
                IncrementFaceCount(faceCount, nodes[1], nodes[2], nodes[3]);
            }

            foreach (var (face, count) in faceCount)
            {
                if (count == 1)
                {
                    boundaryNodes.Add(face.Item1);
                    boundaryNodes.Add(face.Item2);
                    boundaryNodes.Add(face.Item3);
                }
            }
        }

        return boundaryNodes;
    }

    private static List<HashSet<int>> BuildNodeNeighbors(SimplexMesh mesh, int nNodes)
    {
        var neighbors = new List<HashSet<int>>(nNodes);
        for (var i = 0; i < nNodes; i++) neighbors.Add(new HashSet<int>());

        for (var i = 0; i < mesh.Count<Tri3>(); i++)
        {
            var nodes = mesh.NodesOf<Tri3, Node>(i);
            neighbors[nodes[0]].Add(nodes[1]);
            neighbors[nodes[0]].Add(nodes[2]);
            neighbors[nodes[1]].Add(nodes[0]);
            neighbors[nodes[1]].Add(nodes[2]);
            neighbors[nodes[2]].Add(nodes[0]);
            neighbors[nodes[2]].Add(nodes[1]);
        }

        for (var i = 0; i < mesh.Count<Tet4>(); i++)
        {
            var nodes = mesh.NodesOf<Tet4, Node>(i);
            for (var j = 0; j < 4; j++)
                for (var k = 0; k < 4; k++)
                    if (j != k) neighbors[nodes[j]].Add(nodes[k]);
        }

        return neighbors;
    }

    private static List<HashSet<int>> BuildNodeToTriangles(SimplexMesh mesh, int nNodes)
    {
        var nodeTris = new List<HashSet<int>>(nNodes);
        for (var i = 0; i < nNodes; i++) nodeTris.Add(new HashSet<int>());

        for (var i = 0; i < mesh.Count<Tri3>(); i++)
        {
            var nodes = mesh.NodesOf<Tri3, Node>(i);
            nodeTris[nodes[0]].Add(i);
            nodeTris[nodes[1]].Add(i);
            nodeTris[nodes[2]].Add(i);
        }

        return nodeTris;
    }

    private static void IncrementEdgeCount(Dictionary<(int, int), int> dict, int n0, int n1)
    {
        var edge = n0 < n1 ? (n0, n1) : (n1, n0);
        dict[edge] = dict.GetValueOrDefault(edge, 0) + 1;
    }

    private static void IncrementFaceCount(Dictionary<(int, int, int), int> dict, int n0, int n1, int n2)
    {
        var sorted = new[] { n0, n1, n2 }.OrderBy(x => x).ToArray();
        var face = (sorted[0], sorted[1], sorted[2]);
        dict[face] = dict.GetValueOrDefault(face, 0) + 1;
    }

    private static (SimplexMesh mesh, double[,] coords) RebuildMeshWithoutElements(
        SimplexMesh mesh, double[,] coords,
        HashSet<int> excludeTriangles, HashSet<int> excludeTetrahedra)
    {
        var usedNodes = new HashSet<int>();

        for (var i = 0; i < mesh.Count<Tri3>(); i++)
        {
            if (excludeTriangles.Contains(i)) continue;
            var nodes = mesh.NodesOf<Tri3, Node>(i);
            usedNodes.Add(nodes[0]); usedNodes.Add(nodes[1]); usedNodes.Add(nodes[2]);
        }

        for (var i = 0; i < mesh.Count<Tet4>(); i++)
        {
            if (excludeTetrahedra.Contains(i)) continue;
            var nodes = mesh.NodesOf<Tet4, Node>(i);
            usedNodes.Add(nodes[0]); usedNodes.Add(nodes[1]); usedNodes.Add(nodes[2]); usedNodes.Add(nodes[3]);
        }

        for (var i = 0; i < mesh.Count<Bar2>(); i++)
        {
            var nodes = mesh.NodesOf<Bar2, Node>(i);
            usedNodes.Add(nodes[0]); usedNodes.Add(nodes[1]);
        }

        for (var i = 0; i < mesh.Count<Point>(); i++)
        {
            var nodes = mesh.NodesOf<Point, Node>(i);
            usedNodes.Add(nodes[0]);
        }

        var nodeMap = new Dictionary<int, int>();
        var newCoordsList = new List<double[]>();

        foreach (var oldId in usedNodes.OrderBy(x => x))
        {
            nodeMap[oldId] = newCoordsList.Count;
            newCoordsList.Add(new[] { coords[oldId, 0], coords[oldId, 1], coords[oldId, 2] });
        }

        var newCoords = new double[newCoordsList.Count, 3];
        for (var i = 0; i < newCoordsList.Count; i++)
        {
            newCoords[i, 0] = newCoordsList[i][0];
            newCoords[i, 1] = newCoordsList[i][1];
            newCoords[i, 2] = newCoordsList[i][2];
        }

        var newMesh = new SimplexMesh();

        newMesh.WithBatch(() =>
        {
            for (var i = 0; i < newCoordsList.Count; i++) newMesh.AddNode(i);

            for (var i = 0; i < mesh.Count<Tri3>(); i++)
            {
                if (excludeTriangles.Contains(i)) continue;
                var nodes = mesh.NodesOf<Tri3, Node>(i);
                var idx = newMesh.AddTriangle(nodeMap[nodes[0]], nodeMap[nodes[1]], nodeMap[nodes[2]]);
                var orig = mesh.Get<Tri3, OriginalElement>(i);
                newMesh.Set<Tri3, OriginalElement>(idx, orig);
            }

            for (var i = 0; i < mesh.Count<Tet4>(); i++)
            {
                if (excludeTetrahedra.Contains(i)) continue;
                var nodes = mesh.NodesOf<Tet4, Node>(i);
                var idx = newMesh.AddTetrahedron(nodeMap[nodes[0]], nodeMap[nodes[1]], nodeMap[nodes[2]], nodeMap[nodes[3]]);
                var orig = mesh.Get<Tet4, OriginalElement>(i);
                newMesh.Set<Tet4, OriginalElement>(idx, orig);
            }

            for (var i = 0; i < mesh.Count<Bar2>(); i++)
            {
                var nodes = mesh.NodesOf<Bar2, Node>(i);
                var idx = newMesh.AddBar(nodeMap[nodes[0]], nodeMap[nodes[1]]);
                var orig = mesh.Get<Bar2, OriginalElement>(i);
                newMesh.Set<Bar2, OriginalElement>(idx, orig);
            }

            for (var i = 0; i < mesh.Count<Point>(); i++)
            {
                var nodes = mesh.NodesOf<Point, Node>(i);
                var idx = newMesh.AddPoint(nodeMap[nodes[0]]);
                var orig = mesh.Get<Point, OriginalElement>(i);
                newMesh.Set<Point, OriginalElement>(idx, orig);
            }
        });

        var removedCount = excludeTriangles.Count + excludeTetrahedra.Count;
        Console.WriteLine($"[RebuildMesh] Removed {removedCount} degenerate elements");

        return (newMesh, newCoords);
    }
}
