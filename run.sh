#!/bin/sh

set -e

# too slow
# "Lang.Final.Simple.test_leftCnt"
# "Lang.Initial.Simple.test_leftCnt"
TESTS=(
        "Lang.Final.Simple.test_rightCnt"
        "Lang.Final.NoData.test_leftCnt"
        "Lang.Final.NoData.test_rightCnt"
        "Lang.Final.Zipper.test_leftCnt"
        "Lang.Final.Zipper.test_rightCnt"
        "Lang.Initial.Zipper.test_leftCnt"
        "Lang.Initial.Zipper.test_rightCnt"
        "Lang.Initial.Simple.test_rightCnt"
      )

# rm prof/*.prof 2> /dev/null || true
for i in "${TESTS[@]}"; do
    stack run tshsh:tshsh-bench --profile -- $i +RTS -p
    cp tshsh-bench.prof prof/$i.prof
done
