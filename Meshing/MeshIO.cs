// MeshIO.cs
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

// MeshIO.cs - Mesh I/O: Gmsh MSH, GiD/CIMNE, ASCII formats
// License: GPLv3

using System.Globalization;

namespace Numerical;

/// <summary>
///     Mesh I/O operations: Gmsh MSH 2.2, GiD/CIMNE, ASCII debug format.
/// </summary>
public static class MeshIO
{
    public static void SaveMSH(SimplexMesh mesh, double[,] coordinates, string path)
    {
        ArgumentNullException.ThrowIfNull(mesh);
        ArgumentNullException.ThrowIfNull(coordinates);
        ArgumentNullException.ThrowIfNull(path);
        int numNodes = mesh.Count<Node>();
        int numPoints = mesh.Count<Point>();
        int numBars = mesh.Count<Bar2>();
        int numTris = mesh.Count<Tri3>();
        int numTets = mesh.Count<Tet4>();
        int numElements = numPoints + numBars + numTris + numTets;
        using var writer = new StreamWriter(path);
        writer.WriteLine("$MeshFormat");
        writer.WriteLine("2.2 0 8");
        writer.WriteLine("$EndMeshFormat");
        writer.WriteLine("$Nodes");
        writer.WriteLine(numNodes);
        for (int i = 0; i < numNodes; i++)
            writer.WriteLine(string.Format(CultureInfo.InvariantCulture,
                "{0} {1} {2} {3}", i + 1, coordinates[i, 0], coordinates[i, 1], coordinates[i, 2]));
        writer.WriteLine("$EndNodes");
        writer.WriteLine("$Elements");
        writer.WriteLine(numElements);
        int elemId = 1;
        for (int i = 0; i < numPoints; i++)
        {
            var n = mesh.NodesOf<Point, Node>(i);
            writer.WriteLine($"{elemId++} 15 2 0 0 {n[0] + 1}");
        }
        for (int i = 0; i < numBars; i++)
        {
            var n = mesh.NodesOf<Bar2, Node>(i);
            writer.WriteLine($"{elemId++} 1 2 0 0 {n[0] + 1} {n[1] + 1}");
        }
        for (int i = 0; i < numTris; i++)
        {
            var n = mesh.NodesOf<Tri3, Node>(i);
            writer.WriteLine($"{elemId++} 2 2 0 0 {n[0] + 1} {n[1] + 1} {n[2] + 1}");
        }
        for (int i = 0; i < numTets; i++)
        {
            var n = mesh.NodesOf<Tet4, Node>(i);
            writer.WriteLine($"{elemId++} 4 2 0 0 {n[0] + 1} {n[1] + 1} {n[2] + 1} {n[3] + 1}");
        }
        writer.WriteLine("$EndElements");
    }

    public static void SaveMSH(SimplexMesh mesh, double[,] coordinates, string path,
        Dictionary<int, (int Dimension, string Name)>? physicalNames = null,
        int[]? pointTags = null, int[]? barTags = null,
        int[]? triTags = null, int[]? tetTags = null)
    {
        ArgumentNullException.ThrowIfNull(mesh);
        ArgumentNullException.ThrowIfNull(coordinates);
        ArgumentNullException.ThrowIfNull(path);
        int numNodes = mesh.Count<Node>();
        int numPoints = mesh.Count<Point>();
        int numBars = mesh.Count<Bar2>();
        int numTris = mesh.Count<Tri3>();
        int numTets = mesh.Count<Tet4>();
        int numElements = numPoints + numBars + numTris + numTets;
        if (pointTags != null && pointTags.Length != numPoints)
            throw new ArgumentException($"pointTags length ({pointTags.Length}) must match Point count ({numPoints})");
        if (barTags != null && barTags.Length != numBars)
            throw new ArgumentException($"barTags length ({barTags.Length}) must match Bar2 count ({numBars})");
        if (triTags != null && triTags.Length != numTris)
            throw new ArgumentException($"triTags length ({triTags.Length}) must match Tri3 count ({numTris})");
        if (tetTags != null && tetTags.Length != numTets)
            throw new ArgumentException($"tetTags length ({tetTags.Length}) must match Tet4 count ({numTets})");
        using var writer = new StreamWriter(path);
        writer.WriteLine("$MeshFormat");
        writer.WriteLine("2.2 0 8");
        writer.WriteLine("$EndMeshFormat");
        if (physicalNames != null && physicalNames.Count > 0)
        {
            writer.WriteLine("$PhysicalNames");
            writer.WriteLine(physicalNames.Count);
            foreach (var (id, (dim, name)) in physicalNames.OrderBy(kv => kv.Key))
                writer.WriteLine($"{dim} {id} \"{name}\"");
            writer.WriteLine("$EndPhysicalNames");
        }
        writer.WriteLine("$Nodes");
        writer.WriteLine(numNodes);
        for (int i = 0; i < numNodes; i++)
            writer.WriteLine(string.Format(CultureInfo.InvariantCulture,
                "{0} {1} {2} {3}", i + 1, coordinates[i, 0], coordinates[i, 1], coordinates[i, 2]));
        writer.WriteLine("$EndNodes");
        writer.WriteLine("$Elements");
        writer.WriteLine(numElements);
        int elemId = 1;
        for (int i = 0; i < numPoints; i++)
        {
            var n = mesh.NodesOf<Point, Node>(i);
            int physTag = pointTags?[i] ?? 0;
            writer.WriteLine($"{elemId++} 15 2 {physTag} 0 {n[0] + 1}");
        }
        for (int i = 0; i < numBars; i++)
        {
            var n = mesh.NodesOf<Bar2, Node>(i);
            int physTag = barTags?[i] ?? 0;
            writer.WriteLine($"{elemId++} 1 2 {physTag} 0 {n[0] + 1} {n[1] + 1}");
        }
        for (int i = 0; i < numTris; i++)
        {
            var n = mesh.NodesOf<Tri3, Node>(i);
            int physTag = triTags?[i] ?? 0;
            writer.WriteLine($"{elemId++} 2 2 {physTag} 0 {n[0] + 1} {n[1] + 1} {n[2] + 1}");
        }
        for (int i = 0; i < numTets; i++)
        {
            var n = mesh.NodesOf<Tet4, Node>(i);
            int physTag = tetTags?[i] ?? 0;
            writer.WriteLine($"{elemId++} 4 2 {physTag} 0 {n[0] + 1} {n[1] + 1} {n[2] + 1} {n[3] + 1}");
        }
        writer.WriteLine("$EndElements");
    }

    public static void SaveMSHWithCrackGroups(SimplexMesh mesh, double[,] coordinates, string path,
        HashSet<int>? crackedTriangles = null, HashSet<int>? crackedTetrahedra = null,
        int intactGroupId = 1, int crackedGroupId = 2)
    {
        ArgumentNullException.ThrowIfNull(mesh);
        ArgumentNullException.ThrowIfNull(coordinates);
        ArgumentNullException.ThrowIfNull(path);
        int numTris = mesh.Count<Tri3>();
        int numTets = mesh.Count<Tet4>();
        int[]? triTags = null;
        int[]? tetTags = null;
        if (crackedTriangles != null || numTris > 0)
        {
            triTags = new int[numTris];
            for (int i = 0; i < numTris; i++)
                triTags[i] = (crackedTriangles?.Contains(i) == true) ? crackedGroupId : intactGroupId;
        }
        if (crackedTetrahedra != null || numTets > 0)
        {
            tetTags = new int[numTets];
            for (int i = 0; i < numTets; i++)
                tetTags[i] = (crackedTetrahedra?.Contains(i) == true) ? crackedGroupId : intactGroupId;
        }
        int dim = numTets > 0 ? 3 : 2;
        var physicalNames = new Dictionary<int, (int Dimension, string Name)>
        {
            { intactGroupId, (dim, "NonCracked") },
            { crackedGroupId, (dim, "Cracked") }
        };
        SaveMSH(mesh, coordinates, path, physicalNames,
            pointTags: null, barTags: null, triTags: triTags, tetTags: tetTags);
    }

    public static void SaveASCII(SimplexMesh mesh, double[,] coordinates, string path)
    {
        ArgumentNullException.ThrowIfNull(mesh);
        ArgumentNullException.ThrowIfNull(coordinates);
        ArgumentNullException.ThrowIfNull(path);
        int numNodes = mesh.Count<Node>();
        int numTris = mesh.Count<Tri3>();
        int numTets = mesh.Count<Tet4>();
        using var writer = new StreamWriter(path);
        writer.WriteLine($"# SimplexMesh ASCII format");
        writer.WriteLine($"# Nodes: {numNodes}");
        writer.WriteLine($"# Tri3: {numTris}");
        writer.WriteLine($"# Tet4: {numTets}");
        writer.WriteLine();
        writer.WriteLine("COORDINATES");
        writer.WriteLine(numNodes);
        for (int i = 0; i < numNodes; i++)
            writer.WriteLine(string.Format(CultureInfo.InvariantCulture,
                "{0} {1} {2}", coordinates[i, 0], coordinates[i, 1], coordinates[i, 2]));
        writer.WriteLine();
        if (numTris > 0)
        {
            writer.WriteLine("TRIANGLES");
            writer.WriteLine(numTris);
            for (int i = 0; i < numTris; i++)
            {
                var n = mesh.NodesOf<Tri3, Node>(i);
                writer.WriteLine($"{n[0]} {n[1]} {n[2]}");
            }
            writer.WriteLine();
        }
        if (numTets > 0)
        {
            writer.WriteLine("TETRAHEDRA");
            writer.WriteLine(numTets);
            for (int i = 0; i < numTets; i++)
            {
                var n = mesh.NodesOf<Tet4, Node>(i);
                writer.WriteLine($"{n[0]} {n[1]} {n[2]} {n[3]}");
            }
        }
    }

    public static void PrintStats(SimplexMesh mesh, string label = "Mesh")
    {
        Console.WriteLine($"=== {label} ===");
        Console.WriteLine($"  Nodes: {mesh.Count<Node>()}");
        Console.WriteLine($"  Edges: {mesh.Count<Edge>()}");
        Console.WriteLine($"  Points: {mesh.Count<Point>()}");
        Console.WriteLine($"  Bar2: {mesh.Count<Bar2>()}");
        Console.WriteLine($"  Tri3: {mesh.Count<Tri3>()}");
        Console.WriteLine($"  Tet4: {mesh.Count<Tet4>()}");
    }

    public static (SimplexMesh Mesh, double[,] Coordinates) LoadMSH(string path)
    {
        ArgumentNullException.ThrowIfNull(path);
        if (!File.Exists(path)) throw new FileNotFoundException($"MSH file not found: {path}");
        using var reader = new StreamReader(path);
        return ParseMSH(reader);
    }

    public static (SimplexMesh Mesh, double[,] Coordinates,
        Dictionary<int, (int Dimension, string Name)> PhysicalNames,
        int[] PointTags, int[] BarTags, int[] TriTags, int[] TetTags) LoadMSHWithTags(string path)
    {
        ArgumentNullException.ThrowIfNull(path);
        if (!File.Exists(path)) throw new FileNotFoundException($"MSH file not found: {path}");
        using var reader = new StreamReader(path);
        return ParseMSHWithTags(reader);
    }

    private static (SimplexMesh Mesh, double[,] Coordinates) ParseMSH(StreamReader reader)
    {
        var (mesh, coords, _, _, _, _, _) = ParseMSHWithTags(reader);
        return (mesh, coords);
    }

    private static (SimplexMesh Mesh, double[,] Coordinates,
        Dictionary<int, (int Dimension, string Name)> PhysicalNames,
        int[] PointTags, int[] BarTags, int[] TriTags, int[] TetTags) ParseMSHWithTags(StreamReader reader)
    {
        var physicalNames = new Dictionary<int, (int Dimension, string Name)>();
        double[,]? coordinates = null;
        var elements = new List<(int ElemType, int PhysTag, int[] Nodes)>();
        string? line;
        while ((line = reader.ReadLine()) != null)
        {
            line = line.Trim();
            if (line == "$MeshFormat")
            {
                line = reader.ReadLine()?.Trim();
                if (line != null)
                {
                    var parts = line.Split(' ', StringSplitOptions.RemoveEmptyEntries);
                    double version = double.Parse(parts[0], CultureInfo.InvariantCulture);
                    if (version < 2.0 || version >= 3.0)
                        Console.WriteLine($"Warning: Expected Gmsh 2.x format, got {version}");
                }
                SkipToEndSection(reader, "$EndMeshFormat");
            }
            else if (line == "$PhysicalNames")
            {
                line = reader.ReadLine()?.Trim();
                int numNames = int.Parse(line!, CultureInfo.InvariantCulture);
                for (int i = 0; i < numNames; i++)
                {
                    line = reader.ReadLine()?.Trim();
                    if (line == null) break;
                    var parts = line.Split(' ', StringSplitOptions.RemoveEmptyEntries);
                    int dim = int.Parse(parts[0], CultureInfo.InvariantCulture);
                    int id = int.Parse(parts[1], CultureInfo.InvariantCulture);
                    string name = parts[2].Trim('"');
                    physicalNames[id] = (dim, name);
                }
                SkipToEndSection(reader, "$EndPhysicalNames");
            }
            else if (line == "$Nodes")
            {
                line = reader.ReadLine()?.Trim();
                int nodeCount = int.Parse(line!, CultureInfo.InvariantCulture);
                coordinates = new double[nodeCount, 3];
                for (int i = 0; i < nodeCount; i++)
                {
                    line = reader.ReadLine()?.Trim();
                    if (line == null) break;
                    var parts = line.Split(' ', StringSplitOptions.RemoveEmptyEntries);
                    int nodeId = int.Parse(parts[0], CultureInfo.InvariantCulture) - 1;
                    coordinates[nodeId, 0] = double.Parse(parts[1], CultureInfo.InvariantCulture);
                    coordinates[nodeId, 1] = double.Parse(parts[2], CultureInfo.InvariantCulture);
                    coordinates[nodeId, 2] = double.Parse(parts[3], CultureInfo.InvariantCulture);
                }
                SkipToEndSection(reader, "$EndNodes");
            }
            else if (line == "$Elements")
            {
                line = reader.ReadLine()?.Trim();
                int numElements = int.Parse(line!, CultureInfo.InvariantCulture);
                for (int i = 0; i < numElements; i++)
                {
                    line = reader.ReadLine()?.Trim();
                    if (line == null) break;
                    var parts = line.Split(' ', StringSplitOptions.RemoveEmptyEntries);
                    int elemType = int.Parse(parts[1], CultureInfo.InvariantCulture);
                    int numTags = int.Parse(parts[2], CultureInfo.InvariantCulture);
                    int physTag = numTags > 0 ? int.Parse(parts[3], CultureInfo.InvariantCulture) : 0;
                    int nodeStart = 3 + numTags;
                    var nodes = new int[parts.Length - nodeStart];
                    for (int j = 0; j < nodes.Length; j++)
                        nodes[j] = int.Parse(parts[nodeStart + j], CultureInfo.InvariantCulture) - 1;
                    elements.Add((elemType, physTag, nodes));
                }
                SkipToEndSection(reader, "$EndElements");
            }
            else if (line.StartsWith("$"))
            {
                string endMarker = "$End" + line.Substring(1);
                SkipToEndSection(reader, endMarker);
            }
        }
        if (coordinates == null) throw new FormatException("MSH file missing $Nodes section");
        var pointList = new List<(int PhysTag, int[] Nodes)>();
        var barList = new List<(int PhysTag, int[] Nodes)>();
        var triList = new List<(int PhysTag, int[] Nodes)>();
        var tetList = new List<(int PhysTag, int[] Nodes)>();
        foreach (var (elemType, physTag, nodes) in elements)
        {
            switch (elemType)
            {
                case 15: pointList.Add((physTag, nodes)); break;
                case 1: case 8: barList.Add((physTag, new[] { nodes[0], nodes[1] })); break;
                case 2: case 9: triList.Add((physTag, new[] { nodes[0], nodes[1], nodes[2] })); break;
                case 4: case 11: tetList.Add((physTag, new[] { nodes[0], nodes[1], nodes[2], nodes[3] })); break;
            }
        }
        var mesh = new SimplexMesh();
        int numNodes = coordinates.GetLength(0);
        mesh.WithBatch(() =>
        {
            for (int i = 0; i < numNodes; i++) { mesh.Add<Node>(); mesh.Set<Node, ParentNodes>(i, new ParentNodes(i, i)); }
            foreach (var (_, nodes) in pointList) { int pi = mesh.Add<Point, Node>(nodes[0]); mesh.Set<Point, OriginalElement>(pi, new OriginalElement(pi)); }
            foreach (var (_, nodes) in barList) { int bi = mesh.Add<Bar2, Node>(nodes[0], nodes[1]); mesh.Set<Bar2, OriginalElement>(bi, new OriginalElement(bi)); }
            foreach (var (_, nodes) in triList) { int ti = mesh.Add<Tri3, Node>(nodes[0], nodes[1], nodes[2]); mesh.Set<Tri3, OriginalElement>(ti, new OriginalElement(ti)); }
            foreach (var (_, nodes) in tetList) { int ei = mesh.Add<Tet4, Node>(nodes[0], nodes[1], nodes[2], nodes[3]); mesh.Set<Tet4, OriginalElement>(ei, new OriginalElement(ei)); }
        });
        return (mesh, coordinates, physicalNames,
            pointList.Select(x => x.PhysTag).ToArray(), barList.Select(x => x.PhysTag).ToArray(),
            triList.Select(x => x.PhysTag).ToArray(), tetList.Select(x => x.PhysTag).ToArray());
    }

    private static void SkipToEndSection(StreamReader reader, string endMarker)
    {
        string? line;
        while ((line = reader.ReadLine()) != null) { if (line.Trim() == endMarker) break; }
    }

    public static (SimplexMesh Mesh, double[,] Coordinates) LoadGiD(string path)
    {
        ArgumentNullException.ThrowIfNull(path);
        if (!File.Exists(path)) throw new FileNotFoundException($"GiD mesh file not found: {path}");
        using var reader = new StreamReader(path);
        return ParseGiD(reader);
    }

    public static (SimplexMesh Mesh, double[,] Coordinates,
        int[] PointMaterials, int[] BarMaterials, int[] TriMaterials, int[] TetMaterials) LoadGiDWithMaterials(string path)
    {
        ArgumentNullException.ThrowIfNull(path);
        if (!File.Exists(path)) throw new FileNotFoundException($"GiD mesh file not found: {path}");
        using var reader = new StreamReader(path);
        return ParseGiDWithMaterials(reader);
    }

    private static (SimplexMesh Mesh, double[,] Coordinates) ParseGiD(StreamReader reader)
    {
        var (mesh, coords, _, _, _, _) = ParseGiDWithMaterials(reader);
        return (mesh, coords);
    }

    private static (SimplexMesh Mesh, double[,] Coordinates,
        int[] PointMaterials, int[] BarMaterials, int[] TriMaterials, int[] TetMaterials) ParseGiDWithMaterials(StreamReader reader)
    {
        var nodeCoords = new Dictionary<int, (double X, double Y, double Z)>();
        var pointList = new List<(int Material, int[] Nodes)>();
        var barList = new List<(int Material, int[] Nodes)>();
        var triList = new List<(int Material, int[] Nodes)>();
        var tetList = new List<(int Material, int[] Nodes)>();
        int dimension = 3; string currentElemType = ""; int currentNnode = 0;
        bool inCoordinates = false; bool inElements = false;
        string? line;
        while ((line = reader.ReadLine()) != null)
        {
            line = line.Trim();
            if (string.IsNullOrEmpty(line) || line.StartsWith("#")) continue;
            var upperLine = line.ToUpperInvariant();
            if (upperLine.StartsWith("MESH"))
            {
                var parts = line.Split(' ', StringSplitOptions.RemoveEmptyEntries);
                for (int i = 0; i < parts.Length - 1; i++)
                {
                    var key = parts[i].ToUpperInvariant();
                    if (key == "DIMENSION" && i + 1 < parts.Length) dimension = int.Parse(parts[i + 1], CultureInfo.InvariantCulture);
                    else if (key == "ELEMTYPE" && i + 1 < parts.Length) currentElemType = parts[i + 1].ToUpperInvariant();
                    else if (key == "NNODE" && i + 1 < parts.Length) currentNnode = int.Parse(parts[i + 1], CultureInfo.InvariantCulture);
                }
                continue;
            }
            if (upperLine == "COORDINATES") { inCoordinates = true; inElements = false; continue; }
            if (upperLine == "END COORDINATES") { inCoordinates = false; continue; }
            if (upperLine == "ELEMENTS") { inElements = true; inCoordinates = false; continue; }
            if (upperLine == "END ELEMENTS") { inElements = false; continue; }
            if (inCoordinates)
            {
                var parts = line.Split(' ', StringSplitOptions.RemoveEmptyEntries);
                if (parts.Length >= 3)
                {
                    int nodeId = int.Parse(parts[0], CultureInfo.InvariantCulture);
                    double x = double.Parse(parts[1], CultureInfo.InvariantCulture);
                    double y = double.Parse(parts[2], CultureInfo.InvariantCulture);
                    double z = parts.Length > 3 ? double.Parse(parts[3], CultureInfo.InvariantCulture) : 0.0;
                    nodeCoords[nodeId] = (x, y, z);
                }
            }
            else if (inElements)
            {
                var parts = line.Split(' ', StringSplitOptions.RemoveEmptyEntries);
                if (parts.Length < 2) continue;
                int nNodes = currentNnode > 0 ? currentNnode : GuessNodeCount(currentElemType);
                if (parts.Length < 1 + nNodes) continue;
                var nodes = new int[nNodes];
                for (int j = 0; j < nNodes; j++) nodes[j] = int.Parse(parts[1 + j], CultureInfo.InvariantCulture);
                int material = parts.Length > 1 + nNodes ? int.Parse(parts[1 + nNodes], CultureInfo.InvariantCulture) : 0;
                switch (currentElemType)
                {
                    case "POINT": pointList.Add((material, nodes)); break;
                    case "LINEAR": case "LINE": barList.Add((material, new[] { nodes[0], nodes[1] })); break;
                    case "TRIANGLE": triList.Add((material, new[] { nodes[0], nodes[1], nodes[2] })); break;
                    case "TETRAHEDRA": case "TETRAHEDRON": tetList.Add((material, new[] { nodes[0], nodes[1], nodes[2], nodes[3] })); break;
                    default:
                        if (nNodes == 1) pointList.Add((material, nodes));
                        else if (nNodes == 2) barList.Add((material, nodes));
                        else if (nNodes == 3) triList.Add((material, nodes));
                        else if (nNodes == 4) tetList.Add((material, nodes));
                        break;
                }
            }
        }
        if (nodeCoords.Count == 0) throw new FormatException("GiD mesh file has no coordinates");
        int numNodes = nodeCoords.Count;
        var coordinates = new double[numNodes, 3];
        var nodeIdMap = new Dictionary<int, int>();
        int idx = 0;
        foreach (var nodeId in nodeCoords.Keys.OrderBy(x => x))
        {
            var (x, y, z) = nodeCoords[nodeId];
            coordinates[idx, 0] = x; coordinates[idx, 1] = y; coordinates[idx, 2] = z;
            nodeIdMap[nodeId] = idx; idx++;
        }
        void RemapNodes(int[] nodes) { for (int i = 0; i < nodes.Length; i++) nodes[i] = nodeIdMap[nodes[i]]; }
        foreach (var (_, nodes) in pointList) RemapNodes(nodes);
        foreach (var (_, nodes) in barList) RemapNodes(nodes);
        foreach (var (_, nodes) in triList) RemapNodes(nodes);
        foreach (var (_, nodes) in tetList) RemapNodes(nodes);
        var mesh = new SimplexMesh();
        mesh.WithBatch(() =>
        {
            for (int i = 0; i < numNodes; i++) { mesh.Add<Node>(); mesh.Set<Node, ParentNodes>(i, new ParentNodes(i, i)); }
            foreach (var (_, nodes) in pointList) { int pi = mesh.Add<Point, Node>(nodes[0]); mesh.Set<Point, OriginalElement>(pi, new OriginalElement(pi)); }
            foreach (var (_, nodes) in barList) { int bi = mesh.Add<Bar2, Node>(nodes[0], nodes[1]); mesh.Set<Bar2, OriginalElement>(bi, new OriginalElement(bi)); }
            foreach (var (_, nodes) in triList) { int ti = mesh.Add<Tri3, Node>(nodes[0], nodes[1], nodes[2]); mesh.Set<Tri3, OriginalElement>(ti, new OriginalElement(ti)); }
            foreach (var (_, nodes) in tetList) { int ei = mesh.Add<Tet4, Node>(nodes[0], nodes[1], nodes[2], nodes[3]); mesh.Set<Tet4, OriginalElement>(ei, new OriginalElement(ei)); }
        });
        return (mesh, coordinates, pointList.Select(x => x.Material).ToArray(),
            barList.Select(x => x.Material).ToArray(), triList.Select(x => x.Material).ToArray(),
            tetList.Select(x => x.Material).ToArray());
    }

    private static int GuessNodeCount(string elemType) => elemType switch
    {
        "POINT" => 1, "LINEAR" or "LINE" => 2, "TRIANGLE" => 3, "QUADRILATERAL" => 4,
        "TETRAHEDRA" or "TETRAHEDRON" => 4, "HEXAHEDRA" or "HEXAHEDRON" => 8,
        "PRISM" => 6, "PYRAMID" => 5, _ => 0
    };

    public static void SaveGiD(SimplexMesh mesh, double[,] coordinates, string path)
    {
        SaveGiD(mesh, coordinates, path, null, null, null, null, null);
    }

    public static void SaveGiD(SimplexMesh mesh, double[,] coordinates, string path,
        int[]? pointMaterials, int[]? barMaterials, int[]? triMaterials,
        int[]? quadMaterials, int[]? tetMaterials, string? meshName = null)
    {
        ArgumentNullException.ThrowIfNull(mesh);
        ArgumentNullException.ThrowIfNull(coordinates);
        ArgumentNullException.ThrowIfNull(path);
        int numPoints = mesh.Count<Point>(); int numBars = mesh.Count<Bar2>();
        int numTris = mesh.Count<Tri3>(); int numQuads = mesh.Count<Quad4>();
        int numTets = mesh.Count<Tet4>();
        int numElements = numPoints + numBars + numTris + numQuads + numTets;
        var activeNodes = new HashSet<int>();
        for (int i = 0; i < numPoints; i++) { var n = mesh.NodesOf<Point, Node>(i); activeNodes.Add(n[0]); }
        for (int i = 0; i < numBars; i++) { var n = mesh.NodesOf<Bar2, Node>(i); activeNodes.Add(n[0]); activeNodes.Add(n[1]); }
        for (int i = 0; i < numTris; i++) { var n = mesh.NodesOf<Tri3, Node>(i); activeNodes.Add(n[0]); activeNodes.Add(n[1]); activeNodes.Add(n[2]); }
        for (int i = 0; i < numQuads; i++) { var n = mesh.NodesOf<Quad4, Node>(i); activeNodes.Add(n[0]); activeNodes.Add(n[1]); activeNodes.Add(n[2]); activeNodes.Add(n[3]); }
        for (int i = 0; i < numTets; i++) { var n = mesh.NodesOf<Tet4, Node>(i); activeNodes.Add(n[0]); activeNodes.Add(n[1]); activeNodes.Add(n[2]); activeNodes.Add(n[3]); }
        var nodeMap = new Dictionary<int, int>();
        var sortedNodes = activeNodes.OrderBy(n => n).ToArray();
        for (int i = 0; i < sortedNodes.Length; i++) nodeMap[sortedNodes[i]] = i + 1;
        int numActiveNodes = sortedNodes.Length;
        int dimension = numTets > 0 ? 3 : (numTris > 0 ? 2 : 1);
        if (!path.EndsWith(".msh", StringComparison.OrdinalIgnoreCase)) path = path + ".msh";
        using var writer = new StreamWriter(path);
        writer.WriteLine("# GiD/CIMNE Mesh File");
        writer.WriteLine($"# Generated by MeshIO with continuous numbering");
        if (!string.IsNullOrEmpty(meshName)) writer.WriteLine($"# Mesh name: {meshName}");
        writer.WriteLine($"# Dimension: {dimension}");
        writer.WriteLine($"# Nodes: {numActiveNodes} (continuous 1-based)");
        writer.WriteLine($"# Elements: {numElements} total");
        writer.WriteLine();
        int currentElementId = 1;
        if (numTets > 0)
        {
            writer.WriteLine($"MESH dimension {dimension} ElemType Tetrahedra Nnode 4");
            WriteGiDCoords(writer, coordinates, sortedNodes, dimension);
            writer.WriteLine("Elements");
            for (int i = 0; i < numTets; i++)
            {
                var n = mesh.NodesOf<Tet4, Node>(i); int mat = tetMaterials?[i] ?? 0;
                writer.WriteLine(string.Format(CultureInfo.InvariantCulture, "  {0}  {1}  {2}  {3}  {4}  {5}",
                    currentElementId++, nodeMap[n[0]], nodeMap[n[1]], nodeMap[n[2]], nodeMap[n[3]], mat));
            }
            writer.WriteLine("End Elements"); writer.WriteLine();
        }
        if (numQuads > 0)
        {
            writer.WriteLine($"MESH dimension {dimension} ElemType Quadrilateral Nnode 4");
            WriteGiDCoords(writer, coordinates, sortedNodes, dimension);
            writer.WriteLine("Elements");
            for (int i = 0; i < numQuads; i++)
            {
                var n = mesh.NodesOf<Quad4, Node>(i); int mat = quadMaterials?[i] ?? 0;
                writer.WriteLine(string.Format(CultureInfo.InvariantCulture, "  {0}  {1}  {2}  {3}  {4}  {5}",
                    currentElementId++, nodeMap[n[0]], nodeMap[n[1]], nodeMap[n[2]], nodeMap[n[3]], mat));
            }
            writer.WriteLine("End Elements"); writer.WriteLine();
        }
        if (numTris > 0)
        {
            writer.WriteLine($"MESH dimension {dimension} ElemType Triangle Nnode 3");
            WriteGiDCoords(writer, coordinates, sortedNodes, dimension);
            writer.WriteLine("Elements");
            for (int i = 0; i < numTris; i++)
            {
                var n = mesh.NodesOf<Tri3, Node>(i); int mat = triMaterials?[i] ?? 0;
                writer.WriteLine(string.Format(CultureInfo.InvariantCulture, "  {0}  {1}  {2}  {3}  {4}",
                    currentElementId++, nodeMap[n[0]], nodeMap[n[1]], nodeMap[n[2]], mat));
            }
            writer.WriteLine("End Elements"); writer.WriteLine();
        }
        if (numBars > 0)
        {
            writer.WriteLine($"MESH dimension {dimension} ElemType Linear Nnode 2");
            WriteGiDCoords(writer, coordinates, sortedNodes, dimension);
            writer.WriteLine("Elements");
            for (int i = 0; i < numBars; i++)
            {
                var n = mesh.NodesOf<Bar2, Node>(i); int mat = barMaterials?[i] ?? 0;
                writer.WriteLine(string.Format(CultureInfo.InvariantCulture, "  {0}  {1}  {2}  {3}",
                    currentElementId++, nodeMap[n[0]], nodeMap[n[1]], mat));
            }
            writer.WriteLine("End Elements"); writer.WriteLine();
        }
        if (numPoints > 0)
        {
            writer.WriteLine($"MESH dimension {dimension} ElemType Point Nnode 1");
            WriteGiDCoords(writer, coordinates, sortedNodes, dimension);
            writer.WriteLine("Elements");
            for (int i = 0; i < numPoints; i++)
            {
                var n = mesh.NodesOf<Point, Node>(i); int mat = pointMaterials?[i] ?? 0;
                writer.WriteLine(string.Format(CultureInfo.InvariantCulture, "  {0}  {1}  {2}",
                    currentElementId++, nodeMap[n[0]], mat));
            }
            writer.WriteLine("End Elements"); writer.WriteLine();
        }
    }

    private static void WriteGiDCoords(StreamWriter writer, double[,] coordinates, int[] sortedNodes, int dimension)
    {
        writer.WriteLine("Coordinates");
        for (int i = 0; i < sortedNodes.Length; i++)
        {
            int oldNodeId = sortedNodes[i]; int newNodeId = i + 1;
            if (dimension == 2)
                writer.WriteLine(string.Format(CultureInfo.InvariantCulture,
                    "  {0}  {1}  {2}", newNodeId, coordinates[oldNodeId, 0], coordinates[oldNodeId, 1]));
            else
                writer.WriteLine(string.Format(CultureInfo.InvariantCulture,
                    "  {0}  {1}  {2}  {3}", newNodeId, coordinates[oldNodeId, 0], coordinates[oldNodeId, 1], coordinates[oldNodeId, 2]));
        }
        writer.WriteLine("End Coordinates"); writer.WriteLine();
    }
}
