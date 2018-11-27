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
	dune build @install

clean:
	dune clean

install:
	dune install

# update file headers (using headache)
.PHONY: headers
headers:
	headache -c misc/headache_config.txt -h misc/header.txt \
		Makefile src/*.mli src/*.ml
