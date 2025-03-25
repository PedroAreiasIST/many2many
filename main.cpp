#include <cassert>
#include <iostream>
#include <unordered_set>
#include "godoftypes.hpp"
#include "relationmatrix.hpp"
#include "relationmodel.hpp"
#include "relationonetomany.hpp"
#include "sek.hpp"
#include "test.hpp"
#include "typsek.hpp"
using namespace std;

struct age
{
    unsigned year;
};

PFR_FUNCTIONS_FOR(age);

struct some_person
{
    age year;
    std::string name;
    double salary;
    vector<double> vec{1.0, 2.0, 3.0};
};

PFR_FUNCTIONS_FOR(some_person);

struct point
{
    sek<double, 3> x;
};

PFR_FUNCTIONS_FOR(point);

struct edge
{
};

PFR_FUNCTIONS_FOR(edge);

struct triangle
{
    int palhaco;
    int palhaco2;
};

PFR_FUNCTIONS_FOR(triangle);

struct anotherass
{
    double ddd;
};

PFR_FUNCTIONS_FOR(anotherass);

/**
 * The main entry point for program execution. It contains a series of test
 * function calls to validate different functionalities, perform type sequence
 * mapping to structures, manage relations, and process various data using
 * custom containers and operations.
 *
 * @param argc The number of command-line arguments passed to the program.
 * @param argv An array of C-style strings representing the command-line arguments.
 * @return Returns an integer indicating the exit status of the program. Typically,
 *         0 is returned if the program executes successfully.
 */
int main(int argc, char *argv[])
{
    //    testsek();
    sek<double> vec = {1.0, 2.0, 3.0, 5.0};
    append(vec, {10.0, 20.0, 30.0, 40.0});
    bool res = isfullysatisfied(vec, [](AU gash) { return gash > 0; });
    auto vec2 = vec({2, 3, 1});
    std::cout << "I updated this in my laptop" << std::endl;
    // create type sequence
    using geo = typsek<point, edge>;
    using twod = typsek<triangle, anotherass>;
    using both = typsekmergetype<geo, twod>;
    // map type sequence to godstruct
    using typmanager = typsektostructtype<both, godstruct>;
    sek<size_t> order;
    // instantiate geomanager
    typmanager geomanager;
    relationmodel georelation;
    relationmodelsetnumberofentities(georelation, 4);
    relationmodelinsertdependence(georelation, godgetnumber<edge, typmanager>(), godgetnumber<point, typmanager>(), 2);
    relationmodelinsertdependence(georelation, godgetnumber<triangle, typmanager>(), godgetnumber<point, typmanager>(),
                                  3);
    relationmodelinsertdependence(georelation, godgetnumber<point, typmanager>(),
                                  godgetnumber<anotherass, typmanager>(), 1);

    sek<double> spd(20);
    spd[0] = 33;
    spd[19] = 34;
    std::cout << "SPD=" << spd << std::endl;
    order = relationmodelgetorder(georelation);
    std::cout << order << std::endl;
    relmanytomany rbbb;
    RF aaa = godgetsequence<anotherass>(geomanager);
    append(aaa, anotherass({3.0}));
    RF sp = godgetsequence<point>(geomanager);
    append(sp, {{10.0, 3.0, 5.0}});
    append(sp, {{30.0, 12.0, 34.0}});
    append(sp, {{-10.0, 33.0, 44.0}});
    RF se = godgetsequence<edge>(geomanager);
    append(se, {{}});
    append(se, {{}});
    RF st = godgetsequence<triangle>(geomanager);
    append(st, {100, 200});
    append(st, {200, 300});
    std::cout << "Triangles" << std::endl;
    std::cout << "Is 0 smaller than 1" << std::boolalpha << (st[0] < st[1]) << std::endl;
    std::cout << st << std::endl;
    std::cout << geomanager << std::endl;
    std::vector<std::string> nomestodos;
    PFRALLNAMES(some_person, nomestodos);

    for (size_t i = 0; i < nomestodos.size(); ++i)
    {
        std::cout << nomestodos[i] << "\n";
    }

    nomestodos.clear();
    pfrgetallnames<point>::item<>(nomestodos);
    for (size_t i = 0; i < nomestodos.size(); ++i)
    {
        screenwrite(nomestodos[i], "\n");
    }
    sek<double> aaa1 = {1.0, 2.0, 9.0, 15.0};
    sek<double> bbb1 = {-2.0, 0.0, 1.0, 2.0, 9.0, 13.0, 15.0};
    sek<double> ccc1 = {-3.0, 33.0, -10.0, -3.0, 9.0, 15.0, 100.0, -300.0, 15.0, 100.0};
    auto perm = getorder(ccc1);
    sek<size_t> ind100 = indicesfromvalue(ccc1, 100.0, perm);
    sek<size_t> ind15 = indicesfromvalue(ccc1, 15.0, perm);
    screenwrite(ind15);
    screenwrite(ind100);
    auto sum = [&](double ad, double bd) { return ad + bd; };
    auto prod = [&](double ad, double bd) { return ad * bd; };
    screenwrite(dotproduct(bbb1, ccc1, 0.0, sum, prod));
    sek<size_t> localglobal;
    sek<size_t> globallocal;
    sek<double> sunique;
    sek<size_t> newperm;
    auto newccc1 = ccc1;
    std::cout << "localglobal(globallocal)=" << localglobal(globallocal) << std::endl;
    std::cout << "globallocal(localglobal)=" << globallocal(localglobal) << std::endl;
    auto ccc1s = ccc1;
    screenwrite("\n ccc1 before removal of duplicates\n");
    screenwrite(ccc1);
    screenwrite("\n perm before removal of duplicates\n");
    screenwrite(perm);
    ccc1 = ccc1(localglobal);
    std::cout << "Original ->" << ccc1s << std::endl;
    std::cout << "Mapped ->" << ccc1(globallocal) << std::endl;
    std::cout << "\n localglobal=" << std::endl;
    std::cout << localglobal << std::endl;
    std::cout << "\n globallocal=" << std::endl;
    std::cout << globallocal << std::endl;
    screenwrite("\n ccc1 after removal of duplicates\n");
    screenwrite(ccc1);
    sek<double> ddd1 = getintersection(aaa1, bbb1);
    sek<double> eee1 = getintersection(ddd1, ccc1);
}
