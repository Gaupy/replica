#!/bin/sh
cd ..
echo "$1\n$2\n$3\n$4\n$5\n$6\n$7\n$8\n$9" > conf
ocamlbuild script.native
./script.native conf
