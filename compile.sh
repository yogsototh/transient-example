#!/usr/bin/env bash -x

minnodeversion=6.5.0
if command -v node >/dev/null 2>&1
then
    function version_gt() { test "$(echo "$@" | tr " " "\n" | sort -t '.' -k 1,1 -k 2,2 -k 3,3 -g | head -n 1)" != "$1"; }
    version=$(node --version|sed 's/^v//')
    if version_gt  $minnodeversion $version; then
        echo "You should upgrade node.js to at least 6.5.0"
        exit 1
    fi
else
    echo "node.js is needed, please install a recent version (>=6.5.0)"
    exit 1
fi

if command -v stack >/dev/null 2>&1
then echo "stack is already installed."
else
    echo "stack is not installed. I will try to install it."
    curl -sSL https://get.haskellstack.org/ | sh
fi


if command -v stack >/dev/null 2>&1
then :
else
    echo "stack failed to install. Please install it manually."
    echo "See informations at http://haskellstack.org"
    exit 1
fi

# Downloading Haskell Compiler this can be very long
stack setup
stack --stack-yaml stack-ghcjs.yaml setup

# Building the project the first time it will also download and compile
# library
stack build
stack --stack-yaml stack-ghcjs.yaml build

# Link GHCJS result to the correct directory (static/out.jsexe)
./mklink.sh
