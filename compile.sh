#!/usr/bin/env bash -x

if command -v stack >/dev/null 2>&1
then echo "stack is already installed."
else
    echo "stack is not installed. I will try to install it."
    curl -sSL https://get.haskellstack.org/ | sh
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
