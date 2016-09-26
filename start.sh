#!/usr/bin/env bash

if command -v docker-machine >/dev/null 2>&1;
then
    dockerip=$(docker-machine ip)
else
    dockerip="localhost"
fi

echo "-----------------------"
echo "Please open your browser on"
echo "http://${dockerip}:${1:-3000}"
echo "-----------------------"

stack exec -- wse-exe -p start/0.0.0.0/${1:-3000}
