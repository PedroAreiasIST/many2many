#!/bin/bash
# Every measurement of the paper in sequence: comparison chain, splitter, scaling tables, derived queries.
cd "$(dirname "$0")"
./run_chain.sh > chain.log 2>&1
./run_remesh.sh > remesh.log 2>&1
./run_tables.sh > tables.log 2>&1
./run_graph.sh > graph.log 2>&1
echo ALL_DONE > all.done
