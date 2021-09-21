#!/bin/bash

set -e

echo "=== ./test_scripts/ctrl_z.exp ==="
./test_scripts/ctrl_z.exp
echo "=== ./test_scripts/xclip.exp ==="
./test_scripts/xclip.exp
echo "=== ./test_scripts/sync-env.exp ==="
./test_scripts/sync-env.exp
