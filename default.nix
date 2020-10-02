{ rev    ? "66cd39409e8ae1e2981edfb33bb06c164893d70d" # release-20.09
, sha256 ? "1c44akgqbqsm2b6k5b850lb0q7wq5l5p4hm6bqbaif1h05sc2bkx"

, pkgs   ?
    if builtins.compareVersions builtins.nixVersion "2.0" < 0
    then abort "winter requires at least nix 2.0"
    else import (builtins.fetchTarball {
       url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
       inherit sha256; }) {
       config.allowUnfree = true;
       config.allowBroken = false;
       overlays = [ (self: super: {
        # due to https://github.com/WebAssembly/wabt/issues/1551:
        wabt = super.callPackage ./wabt.nix {};
      }) ];
    })

, returnShellEnv ? pkgs.lib.inNixShell
, mkDerivation   ? null
}:

let

wasm-src = pkgs.fetchFromGitHub {
    owner  = "WebAssembly";
    repo   = "spec";
    rev    = "opam-1.1.1";
    sha256 = "1kp72yv4k176i94np0m09g10cviqp2pnpm7jmiq6ik7fmmbknk7c";
  };

drv = pkgs.haskellPackages.developPackage {
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
