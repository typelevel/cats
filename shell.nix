let

  # use a pinned version of nixpkgs for reproducability
  nixpkgs-version = "21.05";
  pkgs = import (builtins.fetchTarball {
    # Descriptive name to make the store path easier to identify
    name = "nixpkgs-${nixpkgs-version}";
    url = "https://github.com/nixos/nixpkgs/archive/${nixpkgs-version}.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "1ckzhh24mgz6jd1xhfgx0i9mijk6xjqxwsshnvq789xsavrmsc36";
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
