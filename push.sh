#!/usr/bin/env bash
# g++ `llvm-config --cxxflags` llvm/llvmAPI.cpp  -fsyntax-only
nim check cc
nim doc --project --index:on --git.url:https://github.com/ianfun/cc --git.commit:master cc.nim
git add .
git commit 
git push
