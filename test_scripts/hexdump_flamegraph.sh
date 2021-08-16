#!/bin/bash

set -e

./test_scripts/hexdump_p.exp
ghc-prof-flamegraph tshsh.prof
