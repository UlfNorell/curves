
FLAGS = -O2 -rtsopts -funbox-strict-fields -fwarn-incomplete-patterns -Werror

default :
	ghc --make Main.hs -o main $(FLAGS) -odir=lib -hidir=lib

prof :
	ghc --make Main.hs -prof -auto-all -o main_p $(FLAGS) -odir=lib_p -hidir=lib_p

.PHONY : tags

tags :
	hTags -c `find . -name '*.hs'`
