#!/bin/bash

set -e

rm tshsh.eventlog.html || true
rm tshsh.svg || true

./test_scripts/run_tests_q.sh
./test_scripts/hexdump_eventlog.sh
nohup firefox tshsh.eventlog.html &
./test_scripts/hexdump_flamegraph.sh
nohup firefox tshsh.svg &

stack test
stack build --pedantic --fast --work-dir .fast-build
stack exec -- sh -c 'for i in $(find -name "*.hs" -not -path "./.stack-work/*"); do ormolu -o -XBangPatterns -i $i; done'
