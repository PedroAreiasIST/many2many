//
// Created by pedro on 20-09-2024.
//
#include "relationmodel.hpp"
void relationmodelsetnumberofentities(relationmodel &rm, size_t const &nentities)
{
    rm.nel = nentities;
    setnelem(rm.listofdependencies.nodesfromelement, nentities);
    setsize(rm.numberofrequiredobjects, nentities);
}
void relationmodelinsertdependence(relationmodel &rm, size_t const &dependenttype, size_t const &requiredtype,
                                   size_t const &howmanytimes)
{
    append(rm.numberofrequiredobjects[dependenttype], howmanytimes);
    //  appendnodetoelement(rm.listofdependencies.nodesfromelement, dependenttype, requiredtype);
}

void relationmodelclose(relationmodel &rm) { setfromonetomany(rm.listofdependencies); }

sek<size_t> relationmodelgetorder(relationmodel const &rm)
{
    lst order;
    toporder(rm.listofdependencies.nodesfromelement, order);
    return order;
}
