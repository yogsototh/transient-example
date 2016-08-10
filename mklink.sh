#!/usr/bin/env bash

dst=static/out.jsexe
\rm -rf $dst
[[ ! -d static ]] && mkdir static

ln -s ../$(stack --stack-yaml stack-ghcjs.yaml path --dist-dir)/build/wse-exe/wse-exe.jsexe/ $dst
