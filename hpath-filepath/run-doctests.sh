#!/bin/sh

set -e

if [ -n "${SKIP_DOCTESTS}" ] ; then
    echo "Skipping doctests"
    exit 0
fi

if ! command -v doctest >/dev/null ; then
    tempdir="$(mktemp -d)"
    (
     cd "${tempdir}"
     cabal install --installdir="${tempdir}" doctest
    )
    export PATH="${tempdir}:$PATH"
fi

set -x

cabal exec doctest -- -ihpath-filepath/src -XOverloadedStrings System.Posix.FilePath
