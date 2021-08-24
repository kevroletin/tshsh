#!/bin/bash

set -e

rm tshsh.eventlog.html || true
rm tshsh.svg || true

./test_scripts/ctrl_z.exp
./test_scripts/xclip.exp
./test_scripts/hexdump_eventlog.sh
nohup firefox tshsh.eventlog.html &
./test_scripts/hexdump_flamegraph.sh
nohup firefox tshsh.svg &

stack test
stack build --pedantic --fast
stack exec -- sh -c 'for i in $(find -name "*.hs" -not -path "./.stack-work/*"); do ormolu -o -XBangPatterns -i $i; done'
