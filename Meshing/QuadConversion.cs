// QuadConversion.cs
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

using static Numerical.MeshGeometry;

namespace Numerical;

public static class QuadConversion
{
    public static (SimplexMesh mesh, double[,] coords) ConvertToQuads(
        SimplexMesh mesh, double[,] coords,
        int passes = 2, double minQualityThreshold = 0.3)
    {
        Console.WriteLine("[QuadConversion] Starting triangle-to-quad conversion...");
        Console.WriteLine($"  Input: {mesh.Count<Tri3>()} triangles");

        var adjacency = BuildTriangleAdjacency(mesh);
        var converted = new HashSet<int>();
        var quads = new List<(int n0, int n1, int n2, int n3)>();

        for (var pass = 0; pass < passes; pass++)
        {
            Console.WriteLine($"  Pass {pass + 1}/{passes}...");
            var quadsThisPass = 0;

            quadsThisPass += PairByQuality(mesh, coords, adjacency, converted, quads, minQualityThreshold);
            quadsThisPass += PairByValence(mesh, coords, adjacency, converted, quads, minQualityThreshold);
            quadsThisPass += PairByGeometry(mesh, coords, adjacency, converted, quads, minQualityThreshold);

            Console.WriteLine($"    Created {quadsThisPass} quads in pass {pass + 1}");
            if (quadsThisPass == 0) break;
        }

        Console.WriteLine($"  Total quads created: {quads.Count}");

        var newMesh = BuildQuadMesh(mesh, quads, converted);
        return (newMesh, coords);
    }

    private static Dictionary<int, List<int>> BuildTriangleAdjacency(SimplexMesh mesh)
    {
        var edgeToTriangles = new Dictionary<(int, int), List<int>>();

        for (var i = 0; i < mesh.Count<Tri3>(); i++)
        {
            var nodes = mesh.NodesOf<Tri3, Node>(i);
            var edges = new[]
            {
                MakeEdge(nodes[0], nodes[1]),
                MakeEdge(nodes[1], nodes[2]),
                MakeEdge(nodes[2], nodes[0])
            };

            foreach (var edge in edges)
            {
                if (!edgeToTriangles.ContainsKey(edge))
                    edgeToTriangles[edge] = new List<int>();
                edgeToTriangles[edge].Add(i);
            }
        }

        var adjacency = new Dictionary<int, List<int>>();

        for (var i = 0; i < mesh.Count<Tri3>(); i++)
        {
            adjacency[i] = new List<int>();
            var nodes = mesh.NodesOf<Tri3, Node>(i);
            var edges = new[]
            {
                MakeEdge(nodes[0], nodes[1]),
                MakeEdge(nodes[1], nodes[2]),
                MakeEdge(nodes[2], nodes[0])
            };

            foreach (var edge in edges)
                foreach (var neighbor in edgeToTriangles[edge])
                    if (neighbor != i && !adjacency[i].Contains(neighbor))
                        adjacency[i].Add(neighbor);
        }

        return adjacency;
    }

    private static (int, int) MakeEdge(int v0, int v1) => v0 < v1 ? (v0, v1) : (v1, v0);

    private static int PairByQuality(SimplexMesh mesh, double[,] coords,
        Dictionary<int, List<int>> adjacency, HashSet<int> converted,
        List<(int, int, int, int)> quads, double minQuality)
    {
        var count = 0;
        for (var i = 0; i < mesh.Count<Tri3>(); i++)
        {
            if (converted.Contains(i)) continue;
            var nodesI = mesh.NodesOf<Tri3, Node>(i);
            var bestQuality = minQuality;
            var bestNeighbor = -1;
            var bestQuad = (0, 0, 0, 0);
            foreach (var j in adjacency[i])
            {
                if (converted.Contains(j)) continue;
                var nodesJ = mesh.NodesOf<Tri3, Node>(j);
                var quadNodes = TryFormQuad(nodesI, nodesJ);
                if (!quadNodes.HasValue) continue;
                var quality = ComputeQuadQuality(coords, quadNodes.Value.n0, quadNodes.Value.n1, quadNodes.Value.n2, quadNodes.Value.n3);
                if (quality > bestQuality) { bestQuality = quality; bestNeighbor = j; bestQuad = quadNodes.Value; }
            }
            if (bestNeighbor >= 0) { quads.Add(bestQuad); converted.Add(i); converted.Add(bestNeighbor); count++; }
        }
        return count;
    }

    private static int PairByValence(SimplexMesh mesh, double[,] coords,
        Dictionary<int, List<int>> adjacency, HashSet<int> converted,
        List<(int, int, int, int)> quads, double minQuality)
    {
        var valence = ComputeNodeValence(mesh, converted);
        var count = 0;
        for (var i = 0; i < mesh.Count<Tri3>(); i++)
        {
            if (converted.Contains(i)) continue;
            var nodesI = mesh.NodesOf<Tri3, Node>(i);
            var bestNeighbor = -1;
            var bestScore = double.MinValue;
            var bestQuad = (0, 0, 0, 0);
            foreach (var j in adjacency[i])
            {
                if (converted.Contains(j)) continue;
                var nodesJ = mesh.NodesOf<Tri3, Node>(j);
                var quadNodes = TryFormQuad(nodesI, nodesJ);
                if (!quadNodes.HasValue) continue;
                var quality = ComputeQuadQuality(coords, quadNodes.Value.n0, quadNodes.Value.n1, quadNodes.Value.n2, quadNodes.Value.n3);
                if (quality < minQuality) continue;
                double valenceScore = 0;
                var qn = new[] { quadNodes.Value.n0, quadNodes.Value.n1, quadNodes.Value.n2, quadNodes.Value.n3 };
                foreach (var node in qn) { var v = valence.GetValueOrDefault(node, 0); if (v == 3 || v == 5) valenceScore += 1.0; }
                var score = quality + 0.2 * valenceScore;
                if (score > bestScore) { bestScore = score; bestNeighbor = j; bestQuad = quadNodes.Value; }
            }
            if (bestNeighbor >= 0) { quads.Add(bestQuad); converted.Add(i); converted.Add(bestNeighbor); count++; }
        }
        return count;
    }

    private static int PairByGeometry(SimplexMesh mesh, double[,] coords,
        Dictionary<int, List<int>> adjacency, HashSet<int> converted,
        List<(int, int, int, int)> quads, double minQuality)
    {
        var count = 0;
        for (var i = 0; i < mesh.Count<Tri3>(); i++)
        {
            if (converted.Contains(i)) continue;
            var nodesI = mesh.NodesOf<Tri3, Node>(i);
            foreach (var j in adjacency[i])
            {
                if (converted.Contains(j)) continue;
                var nodesJ = mesh.NodesOf<Tri3, Node>(j);
                var quadNodes = TryFormQuad(nodesI, nodesJ);
                if (!quadNodes.HasValue) continue;
                var quality = ComputeQuadQuality(coords, quadNodes.Value.n0, quadNodes.Value.n1, quadNodes.Value.n2, quadNodes.Value.n3);
                if (quality >= minQuality) { quads.Add(quadNodes.Value); converted.Add(i); converted.Add(j); count++; break; }
            }
        }
        return count;
    }

    private static (int n0, int n1, int n2, int n3)? TryFormQuad(IReadOnlyList<int> tri1, IReadOnlyList<int> tri2)
    {
        var shared = FindSharedEdge(tri1, tri2);
        if (!shared.HasValue) return null;
        var (s0, s1) = shared.Value;
        int unique1 = -1, unique2 = -1;
        foreach (var v in tri1) if (v != s0 && v != s1) { unique1 = v; break; }
        foreach (var v in tri2) if (v != s0 && v != s1) { unique2 = v; break; }
        if (unique1 < 0 || unique2 < 0) return null;
        return (s0, unique1, s1, unique2);
    }

    private static (int, int)? FindSharedEdge(IReadOnlyList<int> tri1, IReadOnlyList<int> tri2)
    {
        var edges1 = new[] { MakeEdge(tri1[0], tri1[1]), MakeEdge(tri1[1], tri1[2]), MakeEdge(tri1[2], tri1[0]) };
        var edges2 = new[] { MakeEdge(tri2[0], tri2[1]), MakeEdge(tri2[1], tri2[2]), MakeEdge(tri2[2], tri2[0]) };
        foreach (var e1 in edges1) foreach (var e2 in edges2) if (e1 == e2) return e1;
        return null;
    }

    private static double ComputeQuadQuality(double[,] coords, int n0, int n1, int n2, int n3)
    {
        if (!IsQuadConvex(coords, n0, n1, n2, n3)) return 0.0;
        var e0 = EdgeLength2D(coords, n0, n1);
        var e1 = EdgeLength2D(coords, n1, n2);
        var e2 = EdgeLength2D(coords, n2, n3);
        var e3 = EdgeLength2D(coords, n3, n0);
        var minEdge = Math.Min(Math.Min(e0, e1), Math.Min(e2, e3));
        var maxEdge = Math.Max(Math.Max(e0, e1), Math.Max(e2, e3));
        if (maxEdge < Epsilon) return 0.0;
        var aspectRatio = minEdge / maxEdge;
        var angles = new[] { ComputeAngle(coords, n3, n0, n1), ComputeAngle(coords, n0, n1, n2), ComputeAngle(coords, n1, n2, n3), ComputeAngle(coords, n2, n3, n0) };
        double angleQuality = 0;
        foreach (var angle in angles) angleQuality += Math.Max(0, 1.0 - Math.Abs(angle - 90.0) / 90.0);
        angleQuality /= 4.0;
        return 0.5 * aspectRatio + 0.5 * angleQuality;
    }

    private static double ComputeAngle(double[,] coords, int a, int center, int b)
    {
        var ax = coords[a, 0] - coords[center, 0]; var ay = coords[a, 1] - coords[center, 1];
        var bx = coords[b, 0] - coords[center, 0]; var by = coords[b, 1] - coords[center, 1];
        var dot = ax * bx + ay * by;
        var lenA = Math.Sqrt(ax * ax + ay * ay); var lenB = Math.Sqrt(bx * bx + by * by);
        if (lenA < Epsilon || lenB < Epsilon) return 90.0;
        var cosAngle = Math.Clamp(dot / (lenA * lenB), -1.0, 1.0);
        return Math.Acos(cosAngle) * 180.0 / Math.PI;
    }

    private static Dictionary<int, int> ComputeNodeValence(SimplexMesh mesh, HashSet<int> excludeTriangles)
    {
        var valence = new Dictionary<int, int>();
        for (var i = 0; i < mesh.Count<Tri3>(); i++)
        {
            if (excludeTriangles.Contains(i)) continue;
            var nodes = mesh.NodesOf<Tri3, Node>(i);
            foreach (var node in nodes) valence[node] = valence.GetValueOrDefault(node, 0) + 1;
        }
        return valence;
    }

    private static SimplexMesh BuildQuadMesh(SimplexMesh oldMesh, List<(int n0, int n1, int n2, int n3)> quads, HashSet<int> convertedTriangles)
    {
        var newMesh = new SimplexMesh();
        newMesh.WithBatch(() =>
        {
            for (var i = 0; i < oldMesh.Count<Node>(); i++) newMesh.AddNode(i);
            foreach (var (n0, n1, n2, n3) in quads) newMesh.AddQuad(n0, n1, n2, n3);
            for (var i = 0; i < oldMesh.Count<Tri3>(); i++)
            {
                if (convertedTriangles.Contains(i)) continue;
                var nodes = oldMesh.NodesOf<Tri3, Node>(i);
                newMesh.AddTriangle(nodes[0], nodes[1], nodes[2]);
            }
        });
        Console.WriteLine($"[QuadConversion] Built mesh: {newMesh.Count<Quad4>()} quads, {newMesh.Count<Tri3>()} triangles");
        return newMesh;
    }
}
