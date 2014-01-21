#!/bin/bash

rm -rf el-get/
git clean -X -f -d
git pull
git submodule update --init --recursive
emacs -nw
