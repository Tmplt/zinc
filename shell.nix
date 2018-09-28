with import (fetchTarball https://github.com/nixos/nixpkgs/archive/master.tar.gz) {};

mkShell {
  buildInputs = [
    opam
    coq

    # Opam build dependencies
    m4
    pkgconfig
    gnome2.gtksourceview
    gnome2.gtk
    perl
    which
    autoconf
    python27Full

    # Provers
    #alt-ergo
    #z3
    eprover
    cvc4
  ];
}

# opam install utop core zarith merlin why3-ide
