TEST_FILES := $(wildcard ./imp_programs/*.imp)

.PHONY: all clean $(TEST_FILES) extract

all: build

build:
	ocamlbuild -r -use-menhir -I extract -pkg zarith Zinc.native

debug:
	ocamlbuild -cflag "-g" -r -use-menhir -I extract -pkg zarith Zinc.native
	export OCAMLRUNPARAM=b

test: build $(TEST_FILES)
	@./test.sh

extract:
	cd why3 && \
	why3 extract --recursive --modular -D ocaml64 -D ocaml64_bv.drv imp_ex_assignment.mlw -o ../extract -L . && \
	why3 extract --recursive --modular -D ocaml64 -D ocaml64_bv.drv vm_ex_assignment.mlw -o ../extract -L . && \
	why3 extract --recursive --modular -D ocaml64 -D ocaml64_bv.drv compiler.mlw -o ../extract -L . && \
	why3 extract --recursive --modular -D ocaml64 -D ocaml64_bv.drv -D eq.drv ast_opt.mlw -o ../extract -L .

mips: # TODO: make this target %.s instead
	sde-as -march=r3k -O0 out.s -o mips.o && \
	sde-ld -T linker_script mips.o -o mips.out && \
	sde-objdump -h -z -s -d -t mips.out > mips_ext_program.objdump || \
	@- rm mips.out mips.o

mem: mips
	syncsim --no-gui --mips mips_ext_program.objdump --output-mem -c 65535

clean:
	@- rm -rf _build Parser.ml Parser.mli Parser.conflicts *.out *.objdump *.o

%:
	./Zinc.native imp_programs/$@ && \
	sde-as -march=r3k -O0 out.s -o mips.o && \
	sde-ld -T linker_script mips.o -o mips.out && \
	sde-objdump -h -z -s -d -t mips.out > mips_ext_program.objdump || \
	@- rm mips.out mips.o
