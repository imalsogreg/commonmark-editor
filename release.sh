#!/usr/bin/env bash

:set -ex

BRANCH=`git rev-parse --abbrev-ref HEAD`

if [ "$BRANCH" != "master" ]; then
    echo "Releases happen on master only"
fi


cp dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/commonmark-editor-0.1.0.0/x/commonmark-editor/build/commonmark-editor/commonmark-editor.jsexe/{rts,lib,out,runmain}.js ./docs/
git add docs/*
git commit -m "Release"
git push origin master
