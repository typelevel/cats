let

  # use a pinned version of nixpkgs for reproducability
  nixpkgs-version = "21.11";
  pkgs = import (builtins.fetchTarball {
    # Descriptive name to make the store path easier to identify
    name = "nixpkgs-${nixpkgs-version}";
    url = "https://github.com/nixos/nixpkgs/archive/${nixpkgs-version}.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "162dywda2dvfj1248afxc45kcrg83appjd0nmdb541hl7rnncf02";
  }) {};
in
  with pkgs;
  stdenv.mkDerivation {
    name = "cats-dev-env";

    buildInputs = [
      sbt
      git # used by sbt-buildinfo
      nodejs # used by Scala.js
      # used by sbt-microsites
      jekyll
      ruby
      gawk # used by scripts/parse-test-durations.awk
      graphviz # used for ScalaDoc diagrams
    ];
  }
