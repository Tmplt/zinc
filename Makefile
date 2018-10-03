TEST_FILES := $(wildcard ./imp_programs/*.imp)

.PHONY: all clean $(TEST_FILES) extract

all: build

build:
	ocamlbuild -r -use-menhir -I extract -pkg zarith Zinc.native

test: build $(TEST_FILES)
	@./test.sh

extract:
	cd why3 && \
	why3 extract --recursive --modular -D ocaml64 -D ocaml64_bv.drv imp_ex_assignment.mlw -o ../extract -L . && \
	why3 extract --recursive --modular -D ocaml64 -D ocaml64_bv.drv vm_ex_assignment.mlw -o ../extract -L .

clean:
	@- rm -rf _build Parser.ml Parser.mli Parser.conflicts
