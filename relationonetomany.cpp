#include "relationonetomany.hpp"
#include <algorithm>
#include <cassert>
#include <queue>
#include <vector>
#include <stdexcept>

void setnelem(relationonetomany &rel, size_t nelem) {
    rel.nelems = nelem;
    setsize(rel.lnods, nelem);
}

size_t appendelement(relationonetomany &rel, const hidden::lst &nodes) {
    rel.nelems++;
    setsize(rel.lnods, rel.nelems);
    assert(getsize(rel.lnods) == rel.nelems);
    rel.lnods[rel.nelems - 1] = nodes;
    for (size_t node : nodes) {
        rel.maxnodenumber = std::max(rel.maxnodenumber, node);
    }
    return rel.nelems - 1;
}

void transpose(const relationonetomany &rel, relationonetomany &relt) {
    const size_t numNodes = rel.maxnodenumber + 1;
    std::vector<size_t> counts(numNodes, 0);
    for (const auto &nodes : rel.lnods) {
        for (size_t node : nodes) {
            counts[node]++;
        }
    }
    setnelem(relt, numNodes);
    for (size_t i = 0; i < numNodes; ++i) {
        setsize(relt.lnods[i], counts[i]);
    }
    std::vector<size_t> genMarker(numNodes, 0);
    size_t currentGeneration = 1;
    for (size_t e = 0; e < rel.nelems; ++e) {
        for (size_t node : rel.lnods[e]) {
            if (genMarker[node] != currentGeneration) {
                counts[node] = 0;
                genMarker[node] = currentGeneration;
            }
            relt.lnods[node][counts[node]++] = e;
            relt.maxnodenumber = std::max(relt.maxnodenumber, e);
        }
    }
}

void times(const relationonetomany &rela, const relationonetomany &relb, relationonetomany &relc) {
    setnelem(relc, rela.nelems);
    relc.maxnodenumber = relb.maxnodenumber;
    std::vector<size_t> marker(relc.maxnodenumber + 1, 0);
    size_t currentGeneration = 1;
    for (size_t r = 0; r < rela.nelems; ++r) {
        size_t count = 0;
        for (size_t aCol : rela.lnods[r]) {
            for (size_t bCol : relb.lnods[aCol]) {
                if (marker[bCol] != currentGeneration) {
                    marker[bCol] = currentGeneration;
                    ++count;
                }
            }
        }
        setsize(relc.lnods[r], count);
        size_t idx = 0;
        for (size_t aCol : rela.lnods[r]) {
            for (size_t bCol : relb.lnods[aCol]) {
                if (marker[bCol] == currentGeneration) {
                    marker[bCol] = currentGeneration + 1;
                    relc.lnods[r][idx++] = bCol;
                }
            }
        }
        currentGeneration += 2;
    }
}

void plusunion(const relationonetomany &rela, const relationonetomany &relb, relationonetomany &relc) {
    size_t maxelem = std::max(rela.nelems, relb.nelems);
    setnelem(relc, maxelem);
    relc.maxnodenumber = std::max(rela.maxnodenumber, relb.maxnodenumber);
    std::vector<size_t> marker(relc.maxnodenumber + 1, 0);
    for (size_t row = 0; row < maxelem; ++row) {
        size_t len = 0;
        if (row < rela.nelems) {
            for (size_t node : rela.lnods[row]) {
                len++;
                marker[node] = len;
            }
        }
        if (row < relb.nelems) {
            for (size_t node : relb.lnods[row]) {
                if (marker[node] == 0) {
                    len++;
                    marker[node] = len;
                }
            }
        }
        if (row < rela.nelems) {
            for (size_t node : rela.lnods[row]) {
                marker[node] = 0;
            }
        }
        if (row < relb.nelems) {
            for (size_t node : relb.lnods[row]) {
                marker[node] = 0;
            }
        }
        setsize(relc.lnods[row], len);
    }
    for (size_t row = 0; row < maxelem; ++row) {
        size_t len = 0;
        if (row < rela.nelems) {
            for (size_t node : rela.lnods[row]) {
                relc.lnods[row][len] = node;
                len++;
                marker[node] = len;
            }
        }
        if (row < relb.nelems) {
            for (size_t node : relb.lnods[row]) {
                if (marker[node] == 0) {
                    relc.lnods[row][len] = node;
                    len++;
                    marker[node] = len;
                }
            }
        }
        if (row < rela.nelems) {
            for (size_t node : rela.lnods[row]) {
                marker[node] = 0;
            }
        }
        if (row < relb.nelems) {
            for (size_t node : relb.lnods[row]) {
                marker[node] = 0;
            }
        }
    }
}

void intersection(const relationonetomany &a, const relationonetomany &b, relationonetomany &c) {
    const size_t nRows = std::min(a.nelems, b.nelems);
    setnelem(c, nRows);
    c.maxnodenumber = std::max(a.maxnodenumber, b.maxnodenumber);
    std::vector<size_t> marker(c.maxnodenumber + 1, 0);
    size_t currentGeneration = 1;
    for (size_t r = 0; r < nRows; ++r) {
        for (size_t node : b.lnods[r]) {
            marker[node] = currentGeneration;
        }
        size_t count = 0;
        for (size_t node : a.lnods[r]) {
            if (marker[node] == currentGeneration) {
                ++count;
            }
        }
        setsize(c.lnods[r], count);
        size_t idx = 0;
        for (size_t node : a.lnods[r]) {
            if (marker[node] == currentGeneration) {
                c.lnods[r][idx++] = node;
            }
        }
        ++currentGeneration;
    }
}

void difference(const relationonetomany &rela, const relationonetomany &relb, relationonetomany &relc) {
    const size_t nRows = std::min(rela.nelems, relb.nelems);
    setnelem(relc, nRows);
    relc.maxnodenumber = std::max(rela.maxnodenumber, relb.maxnodenumber);
    std::vector<size_t> marker(relc.maxnodenumber + 1, 0);
    size_t currentGeneration = 1;
    for (size_t r = 0; r < nRows; ++r) {
        for (size_t node : relb.lnods[r]) {
            marker[node] = currentGeneration;
        }
        size_t count = 0;
        for (size_t node : rela.lnods[r]) {
            if (marker[node] != currentGeneration) {
                ++count;
            }
        }
        setsize(relc.lnods[r], count);
        size_t idx = 0;
        for (size_t node : rela.lnods[r]) {
            if (marker[node] != currentGeneration) {
                relc.lnods[r][idx++] = node;
            }
        }
        ++currentGeneration;
    }
}

void toporder(const relationonetomany &rel, hidden::lst &order) {
    setsize(order, 0);
    std::vector<size_t> inDegree(rel.nelems, 0);
    for (const auto &neighbors : rel.lnods) {
        for (size_t node : neighbors) {
            ++inDegree[node];
        }
    }
    std::queue<size_t> q;
    for (size_t i = 0; i < inDegree.size(); ++i) {
        if (inDegree[i] == 0) {
            q.push(i);
        }
    }
    while (!q.empty()) {
        size_t cur = q.front();
        q.pop();
        append(order, cur);
        for (size_t nbr : rel.lnods[cur]) {
            if (--inDegree[nbr] == 0) {
                q.push(nbr);
            }
        }
    }
    if (getsize(order) != getsize(rel.lnods)) {
        throw std::runtime_error("The relation contains cycles, topological sort not possible.");
    }
}

void lexiorder(const relationonetomany &rel, hidden::lst &orderofelements) {
    orderofelements = getorder(rel.lnods);
}

void indicesfromorder(const relationonetomany &rel, const hidden::lst &elemOrder, hidden::lst &oldFromNew, hidden::lst &newFromOld) {
    indicesfromorder(rel.lnods, elemOrder, oldFromNew, newFromOld);
}

void compresselements(relationonetomany &rel, const hidden::lst &oldelementfromnew) {
    rel.lnods = rel.lnods(oldelementfromnew);
    rel.nelems = getsize(oldelementfromnew);
    rel.maxnodenumber = 0;
    for (const auto &nodes : rel.lnods) {
        for (size_t node : nodes) {
            rel.maxnodenumber = std::max(rel.maxnodenumber, node);
        }
    }
}

void compressnodes(relationonetomany &rel, const hidden::lst &newnodefromold) {
    for (auto &nodesList : rel.lnods) {
        nodesList = newnodefromold(nodesList);
    }
    rel.maxnodenumber = 0;
    for (const auto &nodesList : rel.lnods) {
        for (size_t node : nodesList) {
            rel.maxnodenumber = std::max(rel.maxnodenumber, node);
        }
    }
}
