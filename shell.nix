with import (fetchTarball https://github.com/nixos/nixpkgs/archive/master.tar.gz) {};

let
  i686Linux = import <nixpkgs> { system = "i686-linux"; };
  mipsTools = i686Linux.stdenv.mkDerivation {
    name = "mips-sde-d7011e";
    src = fetchTarball {
      url = "https://vesuvio.neteq.ltu.se/downloads/mips-sde.tar.gz";
    };

    buildCommand = ''
      cd $src/06.61/bin
      mkdir -p $out/bin

      copy_and_patch() {
        binary="$out/bin/sde-$1"
        cp $src/06.61/bin/sde-$1 $binary

        # Allow us to modify the file
        chmod 0600 $binary

        # Patch!
        patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
          --set-rpath ${stdenv.cc.libc}/lib $binary

        # Allow us to execute it (fix command not found error)
        chmod +x $binary
      }

      copy_and_patch as
      copy_and_patch ld
      copy_and_patch objdump
    '';
  };

  syncsim = stdenv.mkDerivation {
    name = "SyncSim-3.2";
    src = fetchTarball {
      url = "https://vesuvio.neteq.ltu.se/downloads/syncsim.tar.gz";
    };

    buildInputs = [ jre ];

    buildCommand = ''
      mkdir -p $out/libexec
      mkdir -p $out/bin
      cp $src/SyncSim.jar $out/libexec

      # Create a executable script for convenience
      cat > $out/bin/syncsim <<EOF
      #! ${stdenv.shell} -e
      ${pkgs.jre}/bin/java -jar $out/libexec/SyncSim.jar "\$@"
      EOF

      chmod +x $out/bin/syncsim
    '';
  };
in
  mkShell {
    buildInputs = [
      opam
      coq
      mipsTools
      syncsim

      # Opam build dependencies
      m4
      pkgconfig
      gnome2.gtksourceview
      gnome2.gtk
      perl
      which
      autoconf
      automake
      python27Full

      # Provers
      #alt-ergo
      z3
      eprover
      cvc4
    ];
  }

# opam switch crate ocaml-base-compiler
# opam install utop core zarith merlin why3-ide
