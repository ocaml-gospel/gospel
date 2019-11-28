##########################################################################
#                                                                        #
#  VOCaL -- A Verified OCaml Library                                     #
#                                                                        #
#  Copyright (c) 2018 The VOCaL Project                                  #
#                                                                        #
#  This software is free software, distributed under the MIT license     #
#  (as described in file LICENSE enclosed).                              #
##########################################################################

all:
	cd gospel && dune build @install
	cd why3gospel && dune build @install
	cd src && dune build @install

clean:
	cd gospel && dune clean
	cd why3gospel && dune clean
	cd src && dune clean

install:
	cd gospel && dune install
	cd why3gospel && dune install
	cd src && dune install

# update file headers (using headache)
.PHONY: headers
headers:
	headache -c misc/headache_config.txt -h misc/header.txt \
		Makefile src/*.mli src/*.ml
