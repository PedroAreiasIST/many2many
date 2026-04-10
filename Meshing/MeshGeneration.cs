// MeshGeneration.cs
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
using static Numerical.MeshGeometry;

namespace Numerical;

/// <summary>
///     Structured mesh generation: rectangular, box, and unit meshes.
/// </summary>
public static class MeshGeneration
{
    public static (SimplexMesh Mesh, double[,] Coordinates) CreateRectangularMesh(
        int nx, int ny, double xMin, double xMax, double yMin, double yMax)
    {
        if (nx <= 0)
            throw new ArgumentOutOfRangeException(nameof(nx), nx, "nx must be > 0.");
        if (ny <= 0)
            throw new ArgumentOutOfRangeException(nameof(ny), ny, "ny must be > 0.");
        if (xMax <= xMin)
            throw new ArgumentException("xMax must be greater than xMin.", nameof(xMax));
        if (yMax <= yMin)
            throw new ArgumentException("yMax must be greater than yMin.", nameof(yMax));

        var mesh = new SimplexMesh();
        var coords = new double[(nx + 1) * (ny + 1), 3];

        double dx = (xMax - xMin) / nx;
        double dy = (yMax - yMin) / ny;

        mesh.WithBatch(() =>
        {
            for (int j = 0; j <= ny; j++)
            {
                for (int i = 0; i <= nx; i++)
                {
                    int idx = j * (nx + 1) + i;
                    coords[idx, 0] = xMin + i * dx;
                    coords[idx, 1] = yMin + j * dy;
                    coords[idx, 2] = 0.0;
                    mesh.AddNode(idx);
                }
            }

            for (int j = 0; j < ny; j++)
            {
                for (int i = 0; i < nx; i++)
                {
                    int n0 = j * (nx + 1) + i;
                    int n1 = n0 + 1;
                    int n2 = n0 + (nx + 1);
                    int n3 = n2 + 1;

                    mesh.AddTriangle(n0, n1, n2);
                    mesh.AddTriangle(n1, n3, n2);
                }
            }
        });

        return (mesh, coords);
    }

    public static (SimplexMesh Mesh, double[,] Coordinates) CreateRectangularMesh(int nx, int ny)
        => CreateRectangularMesh(nx, ny, 0, 1, 0, 1);

    public static (SimplexMesh Mesh, double[,] Coordinates) CreateRectangularQuadMesh(
        int nx, int ny, double xMin, double xMax, double yMin, double yMax)
    {
        if (nx <= 0)
            throw new ArgumentOutOfRangeException(nameof(nx), nx, "nx must be > 0.");
        if (ny <= 0)
            throw new ArgumentOutOfRangeException(nameof(ny), ny, "ny must be > 0.");
        if (xMax <= xMin)
            throw new ArgumentException("xMax must be greater than xMin.", nameof(xMax));
        if (yMax <= yMin)
            throw new ArgumentException("yMax must be greater than yMin.", nameof(yMax));

        var mesh = new SimplexMesh();
        var coords = new double[(nx + 1) * (ny + 1), 3];

        double dx = (xMax - xMin) / nx;
        double dy = (yMax - yMin) / ny;

        mesh.WithBatch(() =>
        {
            for (int j = 0; j <= ny; j++)
            {
                for (int i = 0; i <= nx; i++)
                {
                    int idx = j * (nx + 1) + i;
                    coords[idx, 0] = xMin + i * dx;
                    coords[idx, 1] = yMin + j * dy;
                    coords[idx, 2] = 0.0;
                    mesh.AddNode(idx);
                }
            }

            for (int j = 0; j < ny; j++)
            {
                for (int i = 0; i < nx; i++)
                {
                    int n0 = j * (nx + 1) + i;
                    int n1 = n0 + 1;
                    int n2 = n0 + (nx + 1);
                    int n3 = n2 + 1;

                    mesh.AddQuad(n0, n1, n3, n2);
                }
            }
        });

        return (mesh, coords);
    }

    public static (SimplexMesh Mesh, double[,] Coordinates) CreateUnitSquareMesh(int n)
        => CreateRectangularMesh(n, n, 0.0, 1.0, 0.0, 1.0);

    public static (SimplexMesh Mesh, double[,] Coordinates) CreateBoxMesh(
        int nx, int ny, int nz,
        double xMin, double xMax, double yMin, double yMax, double zMin, double zMax)
    {
        if (nx <= 0)
            throw new ArgumentOutOfRangeException(nameof(nx), nx, "nx must be > 0.");
        if (ny <= 0)
            throw new ArgumentOutOfRangeException(nameof(ny), ny, "ny must be > 0.");
        if (nz <= 0)
            throw new ArgumentOutOfRangeException(nameof(nz), nz, "nz must be > 0.");
        if (xMax <= xMin)
            throw new ArgumentException("xMax must be greater than xMin.", nameof(xMax));
        if (yMax <= yMin)
            throw new ArgumentException("yMax must be greater than yMin.", nameof(yMax));
        if (zMax <= zMin)
            throw new ArgumentException("zMax must be greater than zMin.", nameof(zMax));

        var mesh = new SimplexMesh();
        int nodeCount = (nx + 1) * (ny + 1) * (nz + 1);
        var coords = new double[nodeCount, 3];

        double dx = (xMax - xMin) / nx;
        double dy = (yMax - yMin) / ny;
        double dz = (zMax - zMin) / nz;

        int NodeIndex(int i, int j, int k) => k * (nx + 1) * (ny + 1) + j * (nx + 1) + i;

        mesh.WithBatch(() =>
        {
            for (int k = 0; k <= nz; k++)
                for (int j = 0; j <= ny; j++)
                    for (int i = 0; i <= nx; i++)
                    {
                        int idx = NodeIndex(i, j, k);
                        coords[idx, 0] = xMin + i * dx;
                        coords[idx, 1] = yMin + j * dy;
                        coords[idx, 2] = zMin + k * dz;
                        mesh.AddNode(idx);
                    }

            for (int k = 0; k < nz; k++)
                for (int j = 0; j < ny; j++)
                    for (int i = 0; i < nx; i++)
                    {
                        int n0 = NodeIndex(i, j, k);
                        int n1 = NodeIndex(i + 1, j, k);
                        int n2 = NodeIndex(i, j + 1, k);
                        int n3 = NodeIndex(i + 1, j + 1, k);
                        int n4 = NodeIndex(i, j, k + 1);
                        int n5 = NodeIndex(i + 1, j, k + 1);
                        int n6 = NodeIndex(i, j + 1, k + 1);
                        int n7 = NodeIndex(i + 1, j + 1, k + 1);

                        mesh.AddTetrahedron(n0, n1, n3, n7);
                        mesh.AddTetrahedron(n0, n3, n2, n7);
                        mesh.AddTetrahedron(n0, n2, n6, n7);
                        mesh.AddTetrahedron(n0, n6, n4, n7);
                        mesh.AddTetrahedron(n0, n4, n5, n7);
                        mesh.AddTetrahedron(n0, n5, n1, n7);
                    }
        });

        FixInvertedTetrahedra(mesh, coords);
        Console.WriteLine($"[CreateBoxMesh] Created {mesh.Count<Node>()} nodes, {mesh.Count<Tet4>()} tetrahedra");

        return (mesh, coords);
    }

    public static (SimplexMesh Mesh, double[,] Coordinates) CreateBoxMesh(int nx, int ny, int nz)
        => CreateBoxMesh(nx, ny, nz, 0, 1, 0, 1, 0, 1);

    public static (SimplexMesh Mesh, double[,] Coordinates) CreateUnitCubeMesh(int n)
        => CreateBoxMesh(n, n, n, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0);

    private static void FixInvertedTetrahedra(SimplexMesh mesh, double[,] coords)
    {
        var tetsToFix = new List<(int index, int[] nodes)>();

        for (int i = 0; i < mesh.Count<Tet4>(); i++)
        {
            var nodes = mesh.NodesOf<Tet4, Node>(i);
            double jac = ComputeTetrahedronJacobian(coords, nodes[0], nodes[1], nodes[2], nodes[3]);

            if (jac <= 0)
                tetsToFix.Add((i, new[] { nodes[1], nodes[0], nodes[2], nodes[3] }));
        }

        tetsToFix.Sort((a, b) => b.index.CompareTo(a.index));

        foreach (var (index, swappedNodes) in tetsToFix)
        {
            mesh.Remove<Tet4>(index);
            mesh.AddTetrahedron(swappedNodes[0], swappedNodes[1], swappedNodes[2], swappedNodes[3]);
        }

        if (tetsToFix.Count > 0)
            Console.WriteLine($"[CreateBoxMesh] Fixed {tetsToFix.Count} inverted tetrahedra");
    }
}
