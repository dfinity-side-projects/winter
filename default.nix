{ compiler ? "ghc865"

, rev    ? "ca3531850844e185d483fb878fcd00c6b44069e5"
, sha256 ? "1s1zhzdbmdd7l936g7ydzsjqdi5k5ch6vpjilil0jiwjhrpkw3m4"

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
    rev    = "v1.1";
    sha256 = "1jsgrjqzsdmm6f5pgd947nikj7pnxx1mqdnz16j7s62rg8x06h7d";
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
