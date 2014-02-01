
example_files = Examples Basics Textures Styles Advanced Blending Fractals Text

all : $(patsubst %,html/%.html,$(example_files))

ExamplePP : ExamplePP.hs
	ghc --make -O -o $@ $<

HSCOLOUR := HsColour -lit -anchor -partial
PP       := ./ExamplePP

html :
	mkdir html

images :
	mkdir images

html/%.html : %.lhs images html $(PP) ExampleGen.hs
	$(PP) $< | $(HSCOLOUR) -css > $@

clean :
	-rm images/*
	-rm html/*.html

