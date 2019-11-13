{ compiler ? "ghc864"

, rev    ? "e0c7712eac67c6b820d9d1020f46bac96fd8cede"
, sha256 ? "08rcnqxkninl5a560ss39s4nbqf0a677q6qh1fh7i0lr9pxf6aan"

, pkgs   ?
    if builtins.compareVersions builtins.nixVersion "2.0" < 0
    then abort "winter requires at least nix 2.0"
    else import (builtins.fetchTarball {
           url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
           inherit sha256; }) {
           config.allowUnfree = true;
           config.allowBroken = false;
         }

, returnShellEnv ? pkgs.lib.inNixShell
, mkDerivation   ? null
}:

let

haskellPackages = pkgs.haskell.packages.${compiler};

wasm-src = pkgs.fetchFromGitHub {
    owner  = "WebAssembly";
    repo   = "spec";
    rev    = "a56cf2ec042da382f0196fe14dcbd7ff2e973466";
    sha256 = "02c7bpmjmq3bxp4w0g8gkg87ixh4x67d9hz4g25i4snxnc7bj0g7";
    # date = 2018-10-31T18:30:05+01:00;
  };

drv = haskellPackages.developPackage {
  name = "winter";
  root = ./.;

  overrides = with pkgs.haskell.lib; self: super: {
  };

  source-overrides = {};

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools = (attrs.buildTools or []) ++ [
      pkgs.wabt
    ];

    passthru = {
      nixpkgs = pkgs;
    };

  });

  inherit returnShellEnv;
};

in drv.overrideAttrs(old: { WASM_SPEC_TESTS = "${wasm-src}/test/core"; })
