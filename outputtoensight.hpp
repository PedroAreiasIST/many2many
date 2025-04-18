//
// Created by pedro on 13-04-2025.
//

#ifndef OUTPUTTOENSIGHT_HPP
#define OUTPUTTOENSIGHT_HPP
#include "mm2m.hpp"
#include <array>
#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
// This function writes simulation output in Ensight Gold format.
// Note: For simplicity the nodal coordinate array 'x' is assumed to be a vector
//       of std::array<double,3> (one per node); similarly, the connectivity and
//       field arrays are assumed to be provided in flattened form in row-major
//       order.
void ensightoutput(
    int number,                  // Number of time steps (should be <= 9999)
    const std::string &filename, // Base filename for output files
    int nnoe,                    // Number of nodes
    const std::vector<std::array<double, 3>>
        &x,   // Node coordinates: one std::array<double,3> per node
    int nele, // Number of elements
    const std::vector<std::string> &names, const std::vector<int> &nelpr,
    const std::vector<int> &ncnos, const std::vector<int> &el_ni,
    const std::vector<int> &el_no) {
  const int mtp = 7; // maximum number of parts (element types)
  // -------------------------
  // Preliminary bounds checking
  // -------------------------
  if (nnoe > 99999999 || nele > 99999999) {
    std::cerr << "Too large problem, more than 99,999,999 elements or nodes"
              << std::endl;
    std::exit(1);
  }
  if (number > 9999) {
    std::cerr << "Too many output files, please limit them to 9999 steps"
              << std::endl;
    std::exit(1);
  }

  // -------------------------
  // Determine padded step number string.
  // This produces a string such that a number like 1 becomes "0001", 12 becomes
  // "0012", etc.
  // -------------------------
  std::ostringstream oss;
  if (number <= 9)
    oss << "000";
  else if (number <= 99)
    oss << "00";
  else if (number <= 999)
    oss << "0";
  // else no prefix is needed
  oss << number;
  std::string ext = oss.str();

  // -------------------------
  // Write the geometry file (.geo)
  // -------------------------
  {
    std::string geoFilename = filename + ".geo" + ext;
    std::ofstream geoFile(geoFilename);
    if (!geoFile) {
      std::cerr << "Error opening file: " << geoFilename << std::endl;
      std::exit(1);
    }
    geoFile << "Linha 1" << "\n"
            << "Linha 2" << "\n"
            << "node id given" << "\n"
            << "element id given" << "\n"
            << "coordinates" << "\n"
            << std::setw(8) << nnoe << "\n";

    geoFile << std::scientific << std::setprecision(5);
    // Write node data: For each node, print node id (1-indexed) and the three
    // coordinates.
    for (int node = 0; node < nnoe; node++) {
      geoFile << std::setw(8) << (node + 1);
      // In Fortran the loop was over i=1,3 for x(i, node). Here x[node][0..2]
      // holds the coordinates.
      for (int comp = 0; comp < 3; comp++) {
        geoFile << std::setw(12) << x[node][comp];
      }
      geoFile << "\n";
    }

    geoFile << "part 1" << "\n"
            << "Only one part" << "\n";

    // Group elements by part (element “type”) using a temporary vector.
    // (Note: nelpr is assumed to have values 1..mtp.)
    for (int part = 1; part <= mtp; part++) {
      std::vector<int> el_list;
      for (int i = 0; i < nele; i++) {
        if (nelpr[i] == part) {
          el_list.push_back(i);
        }
      }
      if (!el_list.empty()) {
        geoFile << " " << "\n";
        // names vector is assumed to be 0-indexed and correspond to
        // parts 1..mtp.
        geoFile << names[part - 1] << "\n";
        geoFile << std::setw(8) << el_list.size() << "\n";
        int ilimit = ncnos[part - 1];
        // For each element in the current part, write the element id and
        // connectivity.
        for (size_t idx = 0; idx < el_list.size(); idx++) {
          int elem = el_list[idx];
          geoFile << std::setw(8)
                  << (elem + 1); // output element id (1-indexed)
          int start =
              el_ni[elem]; // starting connectivity index for this element
          int end = start + ilimit;
          for (int kk = start; kk < end; kk++) {
            geoFile << std::setw(8) << el_no[kk];
          }
          geoFile << "\n";
        }
      }
    }
    geoFile.close();
  }

  // -------------------------
  // Write the case file (.case)
  // -------------------------
  {
    std::string caseFilename = filename + ".case";
    std::ofstream caseFile(caseFilename);
    if (!caseFile) {
      std::cerr << "Error opening file: " << caseFilename << std::endl;
      std::exit(1);
    }
    caseFile << "FORMAT" << "\n"
             << "type:   ensight" << "\n"
             << "GEOMETRY" << "\n"
             << "model: 1 " << filename << ".geo****" << "\n"
             << "VARIABLE" << "\n";
    // Write nodal variable definitions.
    caseFile << "TIME" << "\n";
    caseFile << "time set: " << std::setw(8) << 1 << "\n";
    caseFile << "number of steps: " << std::setw(8) << (number + 1) << "\n";
    caseFile << "filename start number: " << std::setw(8) << 0 << "\n";
    caseFile << "filename increment: " << std::setw(8) << 1 << "\n";
    caseFile << "time values:" << "\n";
    int total_steps = number + 1;
    // Print time values (simply 0,1,2,...,number), with 5 per line.
    for (int i = 0; i < total_steps; i++) {
      caseFile << std::setw(8) << i;
      if ((i + 1) % 5 == 0)
        caseFile << "\n";
    }
    if (total_steps % 5 != 0)
      caseFile << "\n";
    caseFile.close();
  }
}

void printmesh(mm2m &m, int number,
               const std::vector<std::array<double, 3>> &coordinates) {
  std::vector<std::string> nomes{"point",  "bar2",  "tria3", "quad4",
                                 "tetra4", "hexa8", "penta6"};
  std::vector<int> ncnos{1, 2, 3, 4, 4, 8, 6};
  enum typenumbers : int {
    isanelement = 0,
    node,
    point,
    edge,
    tri,
    quad,
    tet,
    wedge,
    hex
  };
  std::string filename = "output";
  int nnoe = m(node, node).efromn.nelem;
  auto aaa = getallelements(m, isanelement);
  int nele = getsize(aaa);
  std::cout << aaa << std::endl;
  std::vector<int> nelpr(getsize(aaa));
  std::vector<int> elni(1000, 0), elno(1000, 0);
  nele = 0;
  for (int i = 0; i < getsize(aaa); ++i) {
    int etype = aaa[i].first;
    int e = aaa[i].second;
    m2m temp;
    switch (etype) {
    case point:
      temp = m(etype, node);
      if (temp.nfrome[e].size > 0) {
        nelpr[nele] = 1;
        int ss = temp.nfrome[e].size;
        elni[nele + 1] = elni[nele] + ss;
        for (int j = 0; j < ss; ++j) {
          elno[elni[nele] + j] = temp.nfrome[e][j] + 1;
        }
        nele++;
      }
      break;
    case edge:
      temp = m(edge, node);
      if (temp.nfrome[e].size > 0) {
        nelpr[nele] = 2;
        int ss = temp.nfrome[e].size;
        elni[nele + 1] = elni[nele] + ss;
        for (int j = 0; j < ss; ++j) {
          elno[elni[nele] + j] = temp.nfrome[e][j] + 1;
        }
        nele++;
      }
      break;
    case tri:
      temp = m(tri, node);
      if (temp.nfrome[e].size > 0) {
        nelpr[nele] = 3;
        int ss = temp.nfrome[e].size;
        elni[nele + 1] = elni[nele] + ss;
        for (int j = 0; j < ss; ++j) {
          elno[elni[nele] + j] = temp.nfrome[e][j] + 1;
        }
        nele++;
      }
      break;
    case quad:
      temp = m(quad, node);
      if (temp.nfrome[e].size > 0) {
        nelpr[nele] = 4;
        int ss = temp.nfrome[e].size;
        elni[nele + 1] = elni[nele] + ss;
        for (int j = 0; j < ss; ++j) {
          elno[elni[nele] + j] = temp.nfrome[e][j] + 1;
        }
        nele++;
      }
      break;
    case tet:
      temp = m(tet, node);
      if (temp.nfrome[e].size > 0) {
        nelpr[nele] = 5;
        int ss = temp.nfrome[e].size;
        elni[nele + 1] = elni[nele] + ss;
        for (int j = 0; j < ss; ++j) {
          elno[elni[nele] + j] = temp.nfrome[e][j] + 1;
        }
        nele++;
      }
      break;
    case wedge:
      temp = m(wedge, node);
      if (temp.nfrome[e].size > 0) {
        nelpr[nele] = 7;
        int ss = temp.nfrome[e].size;
        elni[nele + 1] = elni[nele] + ss;
        for (int j = 0; j < ss; ++j) {
          elno[elni[nele] + j] = temp.nfrome[e][j] + 1;
        }
        nele++;
      }
      break;
    case hex:
      temp = m(hex, node);
      if (temp.nfrome[e].size > 0) {
        nelpr[nele] = 6;
        int ss = temp.nfrome[e].size;
        elni[nele + 1] = elni[nele] + ss;
        for (int j = 0; j < ss; ++j) {
          elno[elni[nele] + j] = temp.nfrome[e][j] + 1;
        }
        nele++;
      }
      break;
    default:
      break;
    }
  }
  ensightoutput(number,   // Number of time steps (should be <= 9999)
                filename, // Base filename for output files
                nnoe,     // Number of nodes
                coordinates, nele, nomes, nelpr, ncnos, elni,
                elno); // Node coordinates: one std::array<double,3> per node
}

void ensightfromdb(mm2m &m) {
  std::vector<std::array<double, 3>> coordinates;
  coordinates.reserve(19);
  coordinates.push_back({3.7, 4, 0.7});
  coordinates.push_back({3, 4, 0});
  coordinates.push_back({2.7, 4, 1.7});
  coordinates.push_back({2, 4, 1});
  coordinates.push_back({2, 4, 0});
  coordinates.push_back({2, 4, 2});
  coordinates.push_back({1, 3, 1});
  coordinates.push_back({1, 3, 0});
  coordinates.push_back({0, 3, 1});
  coordinates.push_back({0, 3, 0});
  coordinates.push_back({1, 1, 1});
  coordinates.push_back({0, 3, 2});
  coordinates.push_back({1, 1, 0});
  coordinates.push_back({0, 1, 1});
  coordinates.push_back({0, 1, 0});
  coordinates.push_back({1, 0, 0});
  coordinates.push_back({0, 1, 2});
  coordinates.push_back({0, 0, 1});
  coordinates.push_back({0, 0, 0});

  printmesh(m, 0, coordinates);
  marktoerase(m, 1, 2);
  setcompressed(m);
  printmesh(m, 1, coordinates);
}

#endif // OUTPUTTOENSIGHT_HPP
