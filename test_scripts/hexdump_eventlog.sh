#!/bin/bash

set -e

./test_scripts/hexdump.exp
eventlog2html tshsh.eventlog
