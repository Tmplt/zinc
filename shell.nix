with import (fetchTarball https://github.com/nixos/nixpkgs/archive/master.tar.gz) {};

mkShell {
  buildInputs = [
    ocaml
    why3
    coq

    # Provers
    alt-ergo
    z3
    eprover
    cvc4
  ] ++ (with ocamlPackages; [
    findlib
    utop
    core
    menhir
    ocamlbuild
    zarith
    merlin
  ]);
}
