#!/bin/bash

rm -rf el-get/
git clean -Xfd
git pull origin master
git submodule update --init --recursive
emacs -nw
