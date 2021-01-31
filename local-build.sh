#!/bin/bash

export BUILD_DIR=`pwd`
export BUILDPACK_DIR=$BUILD_DIR/../heroku-buildpack-common-lisp
export CACHE_DIR=$BUILD_DIR/cache

ros run -L ccl-bin --asdf --load "$BUILDPACK_DIR/setup/compile.lisp" --quit
