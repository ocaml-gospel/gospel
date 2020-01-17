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
	$(MAKE) gospel
	$(MAKE) why3gospel
	$(MAKE) vocal

.PHONY: gospel why3gospel vocal

gospel:
	cd gospel && dune build @install && dune install

why3gospel:
	cd why3gospel && dune build @install && dune install

vocal:
	cd src && dune build @install && dune install

clean:
	cd gospel && dune clean
	cd why3gospel && dune clean
	cd src && dune clean

# update file headers (using headache)
.PHONY: headers
headers:
	headache -c misc/headache_config.txt -h misc/header.txt \
		Makefile src/*.mli src/*.ml
