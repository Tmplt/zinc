TEST_FILES := $(wildcard ./imp_programs/*.imp)

.PHONY: all clean $(TEST_FILES)

all: build

build:
	ocamlbuild -r -use-menhir -I extract -pkg zarith Cimp.native

test: build $(TEST_FILES)
	@./test.sh

clean:
	@- rm -rf _build Parser.ml Parser.mli
