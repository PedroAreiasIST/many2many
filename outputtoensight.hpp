//
// Created by pedro on 13-04-2025.
//

#ifndef OUTPUTTOENSIGHT_HPP
#define OUTPUTTOENSIGHT_HPP
#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <vector>
#include <array>
#include <string>
#include <cstdlib>

// This function writes simulation output in Ensight Gold format.
// Note: For simplicity the nodal coordinate array 'x' is assumed to be a vector
//       of std::array<double,3> (one per node); similarly, the connectivity and
//       field arrays are assumed to be provided in flattened form in row-major order.
void ensightoutput(
    int number,                                // Number of time steps (should be <= 9999)
    const std::string & filename,              // Base filename for output files
    int nnoe,                                  // Number of nodes
    const std::vector<std::array<double, 3>> & x, // Node coordinates: one std::array<double,3> per node
    int nele,                                  // Number of elements
    int nscalar, int nvector, int ntensor,       // Number of nodal scalar, vector, tensor fields
    const std::vector<std::string> & cscalar,    // Names for nodal scalar variables
    const std::vector<std::string> & cvector,    // Names for nodal vector variables
    const std::vector<std::string> & ctensor,    // Names for nodal tensor variables
    // Nodal field data are assumed to be stored as:
    //   scalar:   [nscalar][nnoe]  → index: scalar[var * nnoe + node]
    //   vector:   [nvector][nnoe][3] → index: vectorField[(var * nnoe + node)*3 + comp]
    //   tens:     [ntensor][nnoe][6] → index: tens[(var * nnoe + node)*6 + comp]
    const std::vector<double> & scalar,
    const std::vector<double> & vectorField,
    const std::vector<double> & tens,
    int nescalar, int nevector, int netensor,   // Number of element scalar, vector, tensor fields
    const std::vector<std::string> & cescalar,   // Names for element scalar variables
    const std::vector<std::string> & cevector,   // Names for element vector variables
    const std::vector<std::string> & cetensor,   // Names for element tensor variables
    // Element field data (assumed flattened similarly):
    //   escalar:  [nescalar][nele] → index: escalar[var * nele + element]
    //   evector:  [nevector][nele][3] → index: evector[(var * nele + element)*3 + comp]
    //   etensor:  [netensor][nele][6] → index: etensor[(var * nele + element)*6 + comp]
    const std::vector<double> & escalar,
    const std::vector<double> & evector,
    const std::vector<double> & etensor,
    // Part naming and connectivity grouping:
    //   names:   names for parts (expected size = mtp, here 7)
    //   nelpr:   element “type” per element (size: nele; values in 1..mtp)
    //   ncnos:   number of connectivity nodes per part (size: mtp)
    //   el_ni:   starting connectivity index for each element (size: nele)
    //   el_no:   connectivity array (flattened; assumed to have sufficient size)
    const std::vector<std::string> & names,
    const std::vector<int> & nelpr,
    const std::vector<int> & ncnos,
    const std::vector<int> & el_ni,
    const std::vector<int> & el_no
)
{
    const int mtp = 7; // maximum number of parts (element types)

    // -------------------------
    // Preliminary bounds checking
    // -------------------------
    if(nnoe > 99999999 || nele > 99999999) {
        std::cerr << "Too large problem, more than 99,999,999 elements or nodes" << std::endl;
        std::exit(1);
    }
    if(number > 9999) {
        std::cerr << "Too many output files, please limit them to 9999 steps" << std::endl;
        std::exit(1);
    }

    // -------------------------
    // Determine padded step number string.
    // This produces a string such that a number like 1 becomes "0001", 12 becomes "0012", etc.
    // -------------------------
    std::ostringstream oss;
    if(number <= 9)
        oss << "000";
    else if(number <= 99)
        oss << "00";
    else if(number <= 999)
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
        // Write node data: For each node, print node id (1-indexed) and the three coordinates.
        for (int node = 0; node < nnoe; node++) {
            geoFile << std::setw(8) << (node + 1);
            // In Fortran the loop was over i=1,3 for x(i, node). Here x[node][0..2] holds the coordinates.
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
                // names vector is assumed to be 0-indexed and correspond to parts 1..mtp.
                geoFile << names[part - 1] << "\n";
                geoFile << std::setw(8) << el_list.size() << "\n";
                int ilimit = ncnos[part - 1];
                // For each element in the current part, write the element id and connectivity.
                for (size_t idx = 0; idx < el_list.size(); idx++) {
                    int elem = el_list[idx];
                    geoFile << std::setw(8) << (elem + 1);  // output element id (1-indexed)
                    int start = el_ni[elem];  // starting connectivity index for this element
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
        for (int i = 0; i < nscalar; i++) {
            caseFile << "scalar per node: 1 " << cscalar[i] << " "
                     << filename << cscalar[i] << ".res****" << "\n";
        }
        for (int i = 0; i < nvector; i++) {
            caseFile << "vector per node: 1 " << cvector[i] << " "
                     << filename << cvector[i] << ".res****" << "\n";
        }
        for (int i = 0; i < ntensor; i++) {
            caseFile << "tensor symm per node: 1 " << ctensor[i] << " "
                     << filename << ctensor[i] << ".res****" << "\n";
        }
        // Write element variable definitions.
        for (int i = 0; i < nescalar; i++) {
            caseFile << "scalar per element: 1 " << cescalar[i] << " "
                     << filename << cescalar[i] << ".res****" << "\n";
        }
        for (int i = 0; i < nevector; i++) {
            caseFile << "vector per element: 1 " << cevector[i] << " "
                     << filename << cevector[i] << ".res****" << "\n";
        }
        for (int i = 0; i < netensor; i++) {
            caseFile << "tensor symm per element: 1 " << cetensor[i] << " "
                     << filename << cetensor[i] << ".res****" << "\n";
        }
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

    // -------------------------
    // Write nodal results files
    // -------------------------
    // Nodal scalar results.
    {
        for (int var = 0; var < nscalar; var++) {
            std::string outFilename = filename + cscalar[var] + ".res" + ext;
            std::ofstream outFile(outFilename);
            if (!outFile) {
                std::cerr << "Error opening file: " << outFilename << std::endl;
                std::exit(1);
            }
            outFile << cscalar[var] << "\n";
            outFile << std::scientific << std::setprecision(5);
            int count = 0;
            // Print 5 values per line.
            for (int node = 0; node < nnoe; node++) {
                outFile << std::setw(12) << scalar[var * nnoe + node];
                count++;
                if (count == 5) {
                    outFile << "\n";
                    count = 0;
                }
            }
            if (count != 0)
                outFile << "\n";
            outFile.close();
        }
    }

    // Nodal vector results.
    {
        for (int var = 0; var < nvector; var++) {
            std::string outFilename = filename + cvector[var] + ".res" + ext;
            std::ofstream outFile(outFilename);
            if (!outFile) {
                std::cerr << "Error opening file: " << outFilename << std::endl;
                std::exit(1);
            }
            outFile << cvector[var] << "\n";
            outFile << std::scientific << std::setprecision(5);
            int count = 0;
            // For each node, print three components.
            for (int node = 0; node < nnoe; node++) {
                for (int comp = 0; comp < 3; comp++) {
                    // Layout: vectorField[((var * nnoe + node) * 3) + comp]
                    outFile << std::setw(12) << vectorField[(var * nnoe + node) * 3 + comp];
                }
                count++;
                if (count == 2) {  // Print 2 nodes per line.
                    outFile << "\n";
                    count = 0;
                }
            }
            if (count != 0)
                outFile << "\n";
            outFile.close();
        }
    }

    // Nodal tensor results.
    {
        for (int var = 0; var < ntensor; var++) {
            std::string outFilename = filename + ctensor[var] + ".res" + ext;
            std::ofstream outFile(outFilename);
            if (!outFile) {
                std::cerr << "Error opening file: " << outFilename << std::endl;
                std::exit(1);
            }
            outFile << ctensor[var] << "\n";
            outFile << std::scientific << std::setprecision(5);
            // For each node, print 6 tensor components on one line.
            for (int node = 0; node < nnoe; node++) {
                for (int comp = 0; comp < 6; comp++) {
                    outFile << std::setw(12) << tens[(var * nnoe + node) * 6 + comp];
                }
                outFile << "\n";
            }
            outFile.close();
        }
    }

    // -------------------------
    // Write element results files
    // -------------------------
    // Element scalar results.
    {
        for (int var = 0; var < nescalar; var++) {
            std::string outFilename = filename + cescalar[var] + ".res" + ext;
            std::ofstream outFile(outFilename);
            if (!outFile) {
                std::cerr << "Error opening file: " << outFilename << std::endl;
                std::exit(1);
            }
            outFile << cescalar[var] << "\n";
            outFile << "part 1" << "\n";
            // For each part (from 1 to mtp), print element values.
            for (int part = 1; part <= mtp; part++) {
                int countElements = 0;
                for (int elem = 0; elem < nele; elem++) {
                    if (nelpr[elem] == part)
                        countElements++;
                }
                if (countElements > 0) {
                    outFile << " " << names[part - 1] << "\n";
                    int count = 0;
                    for (int elem = 0; elem < nele; elem++) {
                        if (nelpr[elem] == part) {
                            outFile << std::setw(12) << escalar[var * nele + elem];
                            count++;
                            if (count == 5) {
                                outFile << "\n";
                                count = 0;
                            }
                        }
                    }
                    if (count != 0)
                        outFile << "\n";
                }
            }
            outFile.close();
        }
    }

    // Element vector results.
    {
        for (int var = 0; var < nevector; var++) {
            std::string outFilename = filename + cevector[var] + ".res" + ext;
            std::ofstream outFile(outFilename);
            if (!outFile) {
                std::cerr << "Error opening file: " << outFilename << std::endl;
                std::exit(1);
            }
            outFile << cevector[var] << "\n";
            outFile << "part 1" << "\n";
            for (int part = 1; part <= mtp; part++) {
                int countElements = 0;
                for (int elem = 0; elem < nele; elem++) {
                    if (nelpr[elem] == part)
                        countElements++;
                }
                if (countElements > 0) {
                    outFile << " " << names[part - 1] << "\n";
                    int count = 0;
                    for (int elem = 0; elem < nele; elem++) {
                        if (nelpr[elem] == part) {
                            // Print three components per element vector.
                            for (int comp = 0; comp < 3; comp++) {
                                outFile << std::setw(12) << evector[(var * nele + elem) * 3 + comp];
                            }
                            count++;
                            if (count == 2) {
                                outFile << "\n";
                                count = 0;
                            }
                        }
                    }
                    if (count != 0)
                        outFile << "\n";
                }
            }
            outFile.close();
        }
    }

    // Element tensor results.
    {
        for (int var = 0; var < netensor; var++) {
            std::string outFilename = filename + cetensor[var] + ".res" + ext;
            std::ofstream outFile(outFilename);
            if (!outFile) {
                std::cerr << "Error opening file: " << outFilename << std::endl;
                std::exit(1);
            }
            outFile << cetensor[var] << "\n";
            outFile << "part 1" << "\n";
            for (int part = 1; part <= mtp; part++) {
                int countElements = 0;
                for (int elem = 0; elem < nele; elem++) {
                    if (nelpr[elem] == part)
                        countElements++;
                }
                if (countElements > 0) {
                    outFile << " " << names[part - 1] << "\n";
                    for (int elem = 0; elem < nele; elem++) {
                        if (nelpr[elem] == part) {
                            // Print 6 tensor components for this element.
                            for (int comp = 0; comp < 6; comp++) {
                                outFile << std::setw(12) << etensor[(var * nele + elem) * 6 + comp];
                            }
                            outFile << "\n";
                        }
                    }
                }
            }
            outFile.close();
        }
    }
}

#endif //OUTPUTTOENSIGHT_HPP
