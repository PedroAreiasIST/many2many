// ======================================================================
// remesh_main.cpp — Main remeshing program
//
// Reads original.malha + cuts.txt, splits via m2m-based splitmesh,
// writes remeshed.malha.  Node groups stored as o2m (node→group-indices).
//
// Pedro Areias / IST — GPLv3
// ======================================================================

#include "remesh.hpp"
#include "meshquality.hpp"
#include <cmath>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

static int nnosc(int ity)
{
    switch (ity) {
    case 1: return 1; case 2: return 2; case 3: return 3;
    case 4: return 4; case 5: return 4; case 6: return 8; case 7: return 6;
    default: throw std::runtime_error("Wrong request to nnosc");
    }
}

static int ityFromNnodes(int nn)
{
    switch (nn) {
    case 1: return 1; case 2: return 2; case 3: return 3; case 4: return 5;
    default: throw std::runtime_error("Error in mesh output");
    }
}

int main()
{
    constexpr int NL = 13;

    // -------------------------------------------------------
    // Read original mesh
    // -------------------------------------------------------
    std::ifstream fmesh("original.malha");
    if (!fmesh) { std::cerr << "Cannot open original.malha" << std::endl; return 1; }

    std::string line;
    for (int i = 0; i < NL; ++i) std::getline(fmesh, line);

    OriginalMesh om;
    fmesh >> om.nno;
    std::getline(fmesh, line);

    std::vector<Vec3> noco(om.nno);
    for (int in = 0; in < om.nno; ++in) {
        int ino;
        fmesh >> ino >> noco[ino-1][0] >> noco[ino-1][1] >> noco[ino-1][2];
        if (ino != in+1) { std::cerr << "Error in coordinate reading" << std::endl; return 1; }
    }
    std::getline(fmesh, line);

    // Elements — first pass: topology types
    std::getline(fmesh, line); // "ELEMENTS ..."
    fmesh >> om.nel;
    std::getline(fmesh, line);

    std::vector<std::string> elementgroup(om.nel);
    std::vector<std::streampos> elemPos(om.nel);
    for (int iel = 0; iel < om.nel; ++iel) {
        elemPos[iel] = fmesh.tellg();
        int jel, ity; fmesh >> jel >> ity;
        if (jel != iel+1) { std::cerr << "Wrong element ordering" << std::endl; return 1; }
        std::getline(fmesh, line);
    }

    // Second pass: fill m2m elem_nodes
    setnumberofelements(om.elem_nodes, om.nel);
    fmesh.seekg(elemPos[0]);

    for (int iel = 0; iel < om.nel; ++iel) {
        int jel, ity; fmesh >> jel >> ity;
        int nn = nnosc(ity);
        seque<int> nodes;
        setsize(nodes, nn);
        for (int k = 0; k < nn; ++k) { int nd; fmesh >> nd; nodes[k] = nd-1; }
        std::getline(fmesh, line);
        auto s = line.find_first_not_of(" \t");
        elementgroup[jel-1] = (s != std::string::npos) ? line.substr(s) : "";
        om.elem_nodes.nfrome.lnods[jel-1] = nodes;
    }
    om.elem_nodes.nfrome.nelem = om.nel;
    for (int iel = 0; iel < om.nel; ++iel)
        for (int k = 0; k < om.elem_nodes.nfrome.nnodes(iel); ++k)
            om.elem_nodes.nfrome.maxnode = std::max(om.elem_nodes.nfrome.maxnode,
                                                     om.elem_nodes.nfrome[iel][k]);
    om.elem_nodes.isupdated = false;
    synchronize(om.elem_nodes); // builds node→element for later queries

    // Node groups: stored as o2m  nodeGroups[node] = {group-index, ...}
    std::getline(fmesh, line); // "Groups"
    int ngroupno; fmesh >> ngroupno; std::getline(fmesh, line);
    std::cout << "ngroupno=" << ngroupno << std::endl;

    std::vector<int> nodelist(ngroupno);
    std::vector<std::string> nodegroup(ngroupno);
    for (int i = 0; i < ngroupno; ++i) {
        fmesh >> nodelist[i];
        std::getline(fmesh, line);
        auto s = line.find_first_not_of(" \t");
        nodegroup[i] = (s != std::string::npos) ? line.substr(s) : "";
    }
    fmesh.close();

    // Build o2m: node → group-indices  (node is 0-based)
    o2m nodeGroupMap;
    setsize(nodeGroupMap, om.nno);
    for (int i = 0; i < ngroupno; ++i) {
        int node = nodelist[i] - 1;
        if (node >= 0 && node < om.nno) {
            int sz = getsize(nodeGroupMap[node]);
            setsize(nodeGroupMap.lnods[node], sz + 1);
            nodeGroupMap[node][sz] = i;
        }
    }

    // -------------------------------------------------------
    // Read cut requests
    // -------------------------------------------------------
    om.nmarkednodepairs = 0;
    std::vector<double> xi;
    std::ifstream fcut("cuts.txt");
    if (fcut) {
        fcut >> om.nmarkednodepairs;
        if (fcut.good() && om.nmarkednodepairs > 0) {
            setsize(om.markednodepairs, om.nmarkednodepairs);
            xi.resize(om.nmarkednodepairs);
            for (int i = 0; i < om.nmarkednodepairs; ++i) {
                int n1, n2; fcut >> n1 >> n2 >> xi[i];
                om.markednodepairs[i] = {n1-1, n2-1};
            }
        } else om.nmarkednodepairs = 0;
        fcut.close();
    }

    // -------------------------------------------------------
    // Split mesh
    // -------------------------------------------------------
    std::cout << "Split begins" << std::endl;
    RenewedMesh nm;
    splitmesh(om, nm);
    std::cout << "Split ends" << std::endl;

    // -------------------------------------------------------
    // New coordinates
    // -------------------------------------------------------
    std::vector<Vec3> noco2(nm.nno);
    for (int in = 0; in < nm.nno; ++in) {
        int in1 = nm.parentnodes[in].first;
        int in2 = nm.parentnodes[in].second;
        if (in >= om.nno) {
            double xiv = 0.0;
            for (int i = 0; i < om.nmarkednodepairs; ++i) {
                int p1 = om.markednodepairs[i].first, p2 = om.markednodepairs[i].second;
                if ((p1==in1 && p2==in2) || (p1==in2 && p2==in1)) { xiv = xi[i]; break; }
            }
            if (in1 == in2) { std::cerr << "Problems in remesh - part I" << std::endl; return 1; }
            for (int d = 0; d < 3; ++d)
                noco2[in][d] = 0.5*(1.0-xiv)*noco[in1][d] + 0.5*(1.0+xiv)*noco[in2][d];
        } else {
            if (in1 != in2) { std::cerr << "Problems in remesh - part II" << std::endl; return 1; }
            noco2[in] = noco[in1];
        }
    }

    // -------------------------------------------------------
    // Validate (smoothing OFF, matching Fortran behavior)
    // -------------------------------------------------------
    int ninverted = validateMeshPostSplit(nm, noco2);
    if (ninverted > 0)
        std::cout << "WARNING: " << ninverted << " tets remain inverted." << std::endl;

    // -------------------------------------------------------
    // Write new mesh
    // -------------------------------------------------------
    std::ofstream fout("remeshed.malha");
    for (int i = 0; i < NL; ++i) fout << "Remeshed at least once" << std::endl;
    fout << " " << nm.nno << std::endl;
    for (int in = 0; in < nm.nno; ++in)
        fout << std::setw(9) << (in+1)
             << std::scientific << std::setprecision(8)
             << std::setw(16) << noco2[in][0]
             << std::setw(16) << noco2[in][1]
             << std::setw(16) << noco2[in][2] << std::endl;

    fout << " ELEMENTS (ELEMENT NUMBER, TOPOLOGY TYPE, CONNECTIVITIES, MAYBE NORMAL VECTOR)" << std::endl;
    fout << std::setw(9) << nm.nel << " -> THIS IS THE TOTAL NUMBER OF ELEMENTS" << std::endl;

    for (int iel = 0; iel < nm.nel; ++iel) {
        int total = nm.elem_nodes.nfrome.nnodes(iel);
        int nn = total - 1;
        int ity = ityFromNnodes(nn);
        int ieo = nm.elem_nodes.nfrome[iel][total-1]; // parent (0-based)
        fout << std::setw(9) << (iel+1) << std::setw(9) << ity;
        for (int k = 0; k < nn; ++k)
            fout << std::setw(9) << (nm.elem_nodes.nfrome[iel][k]+1);
        fout << "   " << elementgroup[ieo] << std::endl;
    }

    // -------------------------------------------------------
    // Node groups (using o2m nodeGroupMap for parent lookups)
    // -------------------------------------------------------
    int ngroupno2 = 0;
    for (int in = 0; in < nm.nno; ++in) {
        int ifather = nm.parentnodes[in].first;
        int imother = nm.parentnodes[in].second;
        if (ifather == imother) {
            ngroupno2 += nodeGroupMap.nnodes(imother);
        } else {
            ngroupno2 += nodeGroupMap.nnodes(imother);
            for (int kf = 0; kf < nodeGroupMap.nnodes(ifather); ++kf) {
                int gf = nodeGroupMap[ifather][kf];
                bool dup = false;
                for (int km = 0; km < nodeGroupMap.nnodes(imother); ++km)
                    if (nodegroup[nodeGroupMap[imother][km]] == nodegroup[gf]) { dup = true; break; }
                if (!dup) ngroupno2++;
            }
        }
    }

    fout << " Groups " << std::endl;
    fout << " " << ngroupno2 << std::endl;

    for (int in = 0; in < nm.nno; ++in) {
        int ifather = nm.parentnodes[in].first;
        int imother = nm.parentnodes[in].second;
        // Mother's groups
        for (int km = 0; km < nodeGroupMap.nnodes(imother); ++km)
            fout << std::setw(8) << (in+1) << "   " << nodegroup[nodeGroupMap[imother][km]] << std::endl;
        // Father's unique groups (only for midpoint nodes)
        if (ifather != imother) {
            for (int kf = 0; kf < nodeGroupMap.nnodes(ifather); ++kf) {
                int gf = nodeGroupMap[ifather][kf];
                bool dup = false;
                for (int km = 0; km < nodeGroupMap.nnodes(imother); ++km)
                    if (nodegroup[nodeGroupMap[imother][km]] == nodegroup[gf]) { dup = true; break; }
                if (!dup)
                    fout << std::setw(8) << (in+1) << "   " << nodegroup[gf] << std::endl;
            }
        }
    }

    for (int i = 0; i < 10; ++i) fout << std::endl;
    fout.close();
    std::cout << "Remeshed mesh written to remeshed.malha" << std::endl;
    return 0;
}
