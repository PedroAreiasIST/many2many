#ifndef MESHQUALITY_HPP
#define MESHQUALITY_HPP
// ======================================================================
// meshquality.hpp — Post-split tet mesh quality improvement
//
// Uses m2m relational algebra:
//   - getelementswithnodes() for face-neighbor detection (boundary nodes)
//   - getnodeneighbours()   for Laplacian smoothing stencil
//   - efromn (node→element) for adjacent-tet quality checks
//
// Pedro Areias / IST
// ======================================================================

#include "remesh.hpp"
#include <array>
#include <cmath>
#include <vector>

using Vec3 = std::array<double,3>;

inline Vec3 cross(Vec3 const &u, Vec3 const &v) {
    return {u[1]*v[2]-u[2]*v[1], u[2]*v[0]-u[0]*v[2], u[0]*v[1]-u[1]*v[0]};
}
inline double dot(Vec3 const &a, Vec3 const &b) {
    return a[0]*b[0]+a[1]*b[1]+a[2]*b[2];
}
inline double norm3(Vec3 const &v) { return std::sqrt(dot(v,v)); }
inline Vec3 operator-(Vec3 const &a, Vec3 const &b) { return {a[0]-b[0],a[1]-b[1],a[2]-b[2]}; }
inline Vec3 operator+(Vec3 const &a, Vec3 const &b) { return {a[0]+b[0],a[1]+b[1],a[2]+b[2]}; }
inline Vec3 operator*(double s, Vec3 const &v) { return {s*v[0],s*v[1],s*v[2]}; }

double signedVolumeTet(Vec3 const (&x)[4]);
inline double volumeTet(Vec3 const (&x)[4]) { return std::abs(signedVolumeTet(x)); }
double insphereRadius(Vec3 const (&x)[4]);
double circumsphereRadius(Vec3 const (&x)[4]);
double tetQuality(Vec3 const (&x)[4]);

int validateMeshPostSplit(RenewedMesh &nm, std::vector<Vec3> const &coords);
void improveMeshQuality(RenewedMesh &nm, std::vector<Vec3> &coords, int maxSmoothIter = 10);

#endif
