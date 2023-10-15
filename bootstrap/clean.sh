#!/usr/bin/env bash

set -eux -o pipefail

HERE="$(builtin cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

rm -rf \
    ~/.cabal/logs/ghc-*/sklly-* \
    ~/.cabal/store/ghc-*/sklly-* \
    ~/.cabal/store/ghc-*/package.db/sklly-* \
    ~/.cabal/store/ghc-*/lib/libHSsklly-* \
    ~/.cabal/store/ghc-*/incoming/sklly-* \
    "${HERE}/build" \
    "${HERE}/dist"
