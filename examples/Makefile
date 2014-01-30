
example_files = Basics Texture

all : $(patsubst %,html/%.html,$(example_files))

ExamplePP : ExamplePP.hs
	ghc --make -O -o $@ $<

HSCOLOUR := HsColour -lit
PP       := ./ExamplePP

html/%.html : %.lhs $(PP) ExampleGen.hs
	$(PP) $< | $(HSCOLOUR) -css > $@

