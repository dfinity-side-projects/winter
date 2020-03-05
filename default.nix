{ compiler ? "ghc865"

, rev    ? "c667aba79ce4fd8fe0922024e0cb2826daf6a7c5"
, sha256 ? "03w6sxmr2am57m3hjzlzii73xrgkpvv1q9bg7y5nlnid21bp1fza"

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
