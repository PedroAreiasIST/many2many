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
#include <chrono>
#include <charconv>
#include <cstdio>
#include <cstring>
#include <unordered_map>
static double tnow(){return std::chrono::duration<double>(std::chrono::steady_clock::now().time_since_epoch()).count();}

static bool readwholefile(const char *name, std::vector<char> &buf)
{
    FILE *f = std::fopen(name, "rb");
    if (!f) return false;
    std::fseek(f, 0, SEEK_END);
    long n = std::ftell(f);
    std::fseek(f, 0, SEEK_SET);
    buf.resize(n > 0 ? (size_t) n : 0);
    size_t got = n > 0 ? std::fread(buf.data(), 1, (size_t) n, f) : 0;
    std::fclose(f);
    buf.resize(got);
    return true;
}

/// Sequential scanner over an in-memory text (whitespace-separated numbers, lines).
struct scanner
{
    const char *p, *e;
    scanner(const char *b, const char *end) : p(b), e(end) {}
    void skipspaces() { while (p < e && (*p == ' ' || *p == '\t' || *p == '\r')) ++p; }
    void skipws() { while (p < e && (*p == ' ' || *p == '\t' || *p == '\r' || *p == '\n')) ++p; }
    int readint()
    {
        skipws();
        int v = 0;
        auto r = std::from_chars(p, e, v);
        if (r.ec != std::errc()) throw std::runtime_error("integer expected in the mesh file");
        p = r.ptr;
        return v;
    }
    double readdouble()
    {
        skipws();
        double v = 0;
        auto r = std::from_chars(p, e, v);
        if (r.ec != std::errc()) throw std::runtime_error("number expected in the mesh file");
        p = r.ptr;
        return v;
    }
    void skipline() { while (p < e && *p != '\n') ++p; if (p < e) ++p; }
    /// Rest of the current line without leading blanks and trailing CR/blanks; consumes the newline.
    std::string readrestofline()
    {
        skipspaces();
        const char *b = p;
        while (p < e && *p != '\n') ++p;
        const char *q = p;
        while (q > b && (q[-1] == ' ' || q[-1] == '\t' || q[-1] == '\r')) --q;
        std::string s(b, q);
        if (p < e) ++p;
        return s;
    }
};

/// Buffered text writer with fixed-width numeric fields (right-aligned, as Fortran formats).
struct writer
{
    std::string buf;
    void reserve(size_t n) { buf.reserve(n); }
    void text(const char *s) { buf.append(s); }
    void text(std::string const &s) { buf.append(s); }
    void integer(long long v, int width)
    {
        char tmp[24];
        auto r = std::to_chars(tmp, tmp + sizeof tmp, v);
        int len = (int) (r.ptr - tmp);
        for (int i = len; i < width; ++i) buf.push_back(' ');
        buf.append(tmp, (size_t) len);
    }
    void scientific(double v, int precision, int width)
    {
        char tmp[48];
        auto r = std::to_chars(tmp, tmp + sizeof tmp, v, std::chars_format::scientific, precision);
        int len = (int) (r.ptr - tmp);
        for (int i = len; i < width; ++i) buf.push_back(' ');
        buf.append(tmp, (size_t) len);
    }
    void flushto(const char *name)
    {
        FILE *f = std::fopen(name, "wb");
        if (!f) throw std::runtime_error("cannot write the mesh file");
        std::fwrite(buf.data(), 1, buf.size(), f);
        std::fclose(f);
    }
};

static inline unsigned long long pairkey(int a, int b)
{
    unsigned int lo = (unsigned int) std::min(a, b), hi = (unsigned int) std::max(a, b);
    return ((unsigned long long) hi << 32) | lo;
}

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
    const double T0 = tnow();

    // -------------------------------------------------------
    // Read original mesh (whole file in memory, hand-written scanner)
    // -------------------------------------------------------
    std::vector<char> text;
    if (!readwholefile("original.malha", text)) { std::cerr << "Cannot open original.malha" << std::endl; return 1; }
    scanner sc(text.data(), text.data() + text.size());
    for (int i = 0; i < NL; ++i) sc.skipline();

    OriginalMesh om;
    om.nno = sc.readint();
    sc.skipline();

    std::vector<Vec3> noco(om.nno);
    for (int in = 0; in < om.nno; ++in) {
        int ino = sc.readint();
        if (ino != in+1) { std::cerr << "Error in coordinate reading" << std::endl; return 1; }
        noco[in][0] = sc.readdouble(); noco[in][1] = sc.readdouble(); noco[in][2] = sc.readdouble();
        sc.skipline();
    }

    sc.skipline(); // "ELEMENTS ..."
    om.nel = sc.readint();
    sc.skipline();

    std::vector<std::string> elementgroup(om.nel);
    setnumberofelements(om.elem_nodes, om.nel);
    {
        o2m &rel = om.elem_nodes.nfrome;
        reserve(rel, om.nel, 4L * om.nel);
        int nodes[8];
        for (int iel = 0; iel < om.nel; ++iel) {
            int jel = sc.readint(), ity = sc.readint();
            if (jel != iel+1) { std::cerr << "Wrong element ordering" << std::endl; return 1; }
            int nn = nnosc(ity);
            for (int k = 0; k < nn; ++k) nodes[k] = sc.readint() - 1;
            elementgroup[iel] = sc.readrestofline();
            setrow(rel, iel, crowview(nodes, nn));
        }
    }
    updatemaxnode(om.elem_nodes.nfrome);
    om.elem_nodes.isupdated = false;
    synchronize(om.elem_nodes); // builds node->element for later queries

    // Node groups: group i -> node, then its transpose node -> groups
    sc.skipline(); // "Groups"
    int ngroupno = sc.readint(); sc.skipline();
    std::cout << "ngroupno=" << ngroupno << std::endl;

    std::vector<std::string> nodegroup(ngroupno);
    o2m groupToNode;
    setsize(groupToNode, ngroupno);
    for (int i = 0; i < ngroupno; ++i) {
        int node = sc.readint() - 1;
        nodegroup[i] = sc.readrestofline();
        if (node >= 0 && node < om.nno)
            setrow(groupToNode, i, {node});
    }
    groupToNode.maxnode = om.nno - 1;
    o2m nodeGroupMap = Tr(groupToNode); // node -> group-indices (0-based node)
    setsize(nodeGroupMap, om.nno);

    // -------------------------------------------------------
    // Read cut requests
    // -------------------------------------------------------
    om.nmarkednodepairs = 0;
    std::vector<double> xi;
    std::unordered_map<unsigned long long, double> xiofpair;
    {
        std::vector<char> ctext;
        if (readwholefile("cuts.txt", ctext) && !ctext.empty()) {
            scanner cs(ctext.data(), ctext.data() + ctext.size());
            om.nmarkednodepairs = cs.readint();
            if (om.nmarkednodepairs > 0) {
                setsize(om.markednodepairs, om.nmarkednodepairs);
                xi.resize(om.nmarkednodepairs);
                for (int i = 0; i < om.nmarkednodepairs; ++i) {
                    int n1 = cs.readint() - 1, n2 = cs.readint() - 1; xi[i] = cs.readdouble();
                    om.markednodepairs[i] = {n1, n2};
                    xiofpair.emplace(pairkey(n1, n2), xi[i]);
                }
            } else om.nmarkednodepairs = 0;
        }
    }

    // -------------------------------------------------------
    // Split mesh
    // -------------------------------------------------------
    const double T1 = tnow(); std::cout << "TIMER parse " << T1 - T0 << std::endl;
    std::cout << "Split begins" << std::endl;
    RenewedMesh nm;
    splitmesh(om, nm);
    const double T2 = tnow(); std::cout << "Split ends" << std::endl; std::cout << "TIMER split " << T2 - T1 << std::endl;

    // -------------------------------------------------------
    // New coordinates
    // -------------------------------------------------------
    std::vector<Vec3> noco2(nm.nno);
    for (int in = 0; in < nm.nno; ++in) {
        int in1 = nm.parentnodes[in].first;
        int in2 = nm.parentnodes[in].second;
        if (in >= om.nno) {
            double xiv = 0.0;
            {
                auto it = xiofpair.find(pairkey(in1, in2));
                if (it != xiofpair.end()) xiv = it->second;
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
    const double T3 = tnow(); std::cout << "TIMER coords+validate " << T3 - T2 << std::endl;
    writer out;
    out.reserve(64 + 60 * (size_t) nm.nno + 60 * (size_t) nm.nel);
    for (int i = 0; i < NL; ++i) out.text("Remeshed at least once\n");
    out.text(" "); out.integer(nm.nno, 0); out.text("\n");
    for (int in = 0; in < nm.nno; ++in) {
        out.integer(in+1, 9);
        out.scientific(noco2[in][0], 8, 16);
        out.scientific(noco2[in][1], 8, 16);
        out.scientific(noco2[in][2], 8, 16);
        out.text("\n");
    }

    out.text(" ELEMENTS (ELEMENT NUMBER, TOPOLOGY TYPE, CONNECTIVITIES, MAYBE NORMAL VECTOR)\n");
    out.integer(nm.nel, 9); out.text(" -> THIS IS THE TOTAL NUMBER OF ELEMENTS\n");

    for (int iel = 0; iel < nm.nel; ++iel) {
        int total = nm.elem_nodes.nfrome.nnodes(iel);
        int nn = total - 1;
        int ity = ityFromNnodes(nn);
        int ieo = nm.elem_nodes.nfrome[iel][total-1] - om.nno; // parent (0-based), parent slot offset by nno
        out.integer(iel+1, 9); out.integer(ity, 9);
        for (int k = 0; k < nn; ++k)
            out.integer(nm.elem_nodes.nfrome[iel][k]+1, 9);
        out.text("   "); out.text(elementgroup[ieo]); out.text("\n");
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

    out.text(" Groups \n");
    out.text(" "); out.integer(ngroupno2, 0); out.text("\n");

    for (int in = 0; in < nm.nno; ++in) {
        int ifather = nm.parentnodes[in].first;
        int imother = nm.parentnodes[in].second;
        // Mother's groups
        for (int km = 0; km < nodeGroupMap.nnodes(imother); ++km) {
            out.integer(in+1, 8); out.text("   "); out.text(nodegroup[nodeGroupMap[imother][km]]); out.text("\n");
        }
        // Father's unique groups (only for midpoint nodes)
        if (ifather != imother) {
            for (int kf = 0; kf < nodeGroupMap.nnodes(ifather); ++kf) {
                int gf = nodeGroupMap[ifather][kf];
                bool dup = false;
                for (int km = 0; km < nodeGroupMap.nnodes(imother); ++km)
                    if (nodegroup[nodeGroupMap[imother][km]] == nodegroup[gf]) { dup = true; break; }
                if (!dup) {
                    out.integer(in+1, 8); out.text("   "); out.text(nodegroup[gf]); out.text("\n");
                }
            }
        }
    }

    for (int i = 0; i < 10; ++i) out.text("\n");
    out.flushto("remeshed.malha");
    std::cout << "Remeshed mesh written to remeshed.malha" << std::endl;
    const double T4 = tnow(); std::cout << "TIMER write " << T4 - T3 << std::endl << "TIMER total " << T4 - T0 << std::endl;
    return 0;
}
