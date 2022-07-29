#!/bin/bash

NAME=github-actions
VERSION=0.1.0

export DOCKER_BUILDKIT=1

set -ex

docker build --add-host 'beta.quicklisp.org:13.33.243.6' --progress plain --tag cr.yandex/crp7b1mum9l62quiuuu0/$NAME:$VERSION .

docker push cr.yandex/crp7b1mum9l62quiuuu0/$NAME:$VERSION
