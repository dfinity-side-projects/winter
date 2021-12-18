{ rev    ? "0d40179fd4cd7252bd8f8763c02cc8991b891219" # release-21.05
, sha256 ? "0n2wm7n0aar2j2cjm22swm09dsmzgji34mb11nmr1ffs3vzhgr07"

, pkgs   ?
    if builtins.compareVersions builtins.nixVersion "2.0" < 0
    then abort "winter requires at least nix 2.0"
    else import (builtins.fetchTarball {
       url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
       inherit sha256;
      }) {
         config.allowUnfree = true;
         config.allowBroken = false;
      }

, returnShellEnv ? pkgs.lib.inNixShell
, mkDerivation   ? null
}:

let

spec-tests = pkgs.fetchFromGitHub {
    owner  = "WebAssembly";
    repo   = "testsuite";
    rev    = "6aacfd8929504d8e02a5144a14d184196ede6790";
    sha256 = "sha256-HrnTpIEVN3H9P4fuSBUkaMpNdMxa2pbfp1iPElJLlUM";
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

in drv.overrideAttrs(old: { WASM_SPEC_TESTS = "${spec-tests}"; })
