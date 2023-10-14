#!/bin/bash

# run compiler on all test case
ocaml script/tester.ml

# remove test output
find tests -type f -name '*.asm' -exec rm -f {} \;
find tests -type f -name '*.imp' -exec rm -f {} \;
