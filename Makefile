.PHONY: idea all

idea:
	mill mill.scalalib.GenIdea/idea

all:
	mill ichor.compile