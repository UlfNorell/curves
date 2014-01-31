
FLAGS   = -O2 -rtsopts -funbox-strict-fields -fwarn-incomplete-patterns -Werror -hide-package monads-tf
VERSION = 1.0.1
LIB			= dist/build/libHScurves-$(VERSION).a
hs_files = $(shell find Graphics -name '*.hs')

.PHONY: examples

default : $(LIB) examples tags

examples :
	make -C examples

examples/main : $(LIB) $(wildcard examples/*.hs)
	(cd examples; ghc --make Main.hs -o main $(FLAGS) -odir=../lib -hidir=../lib)

prof : examples/main_p

examples/main_p : library examples/Main.hs
	(cd examples; ghc --make Main.hs -prof -auto-all -o main_p $(FLAGS) -odir=../lib_p -hidir=../lib_p)

library : $(LIB)

dist/build/libHScurves-$(VERSION).a : dist/setup-config $(hs_files)
	cabal build
	cabal install --reinstall

dist/setup-config : curves.cabal
	cabal configure

tags : $(hs_files)
	hTags -c $(hs_files)
