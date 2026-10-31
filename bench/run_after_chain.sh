#!/bin/bash
cd "$(dirname "$0")"
while ! grep -q DONE results.jsonl; do sleep 15; done
./run_remesh.sh > remesh.log 2>&1
./run_tables.sh > tables.log 2>&1
