{ rev    ? "058f304d41966672be66efd978210b9ec8c81687" # release-21.11
, sha256 ? "1l0x61jpiw2q78p7bgjk93slgqd3nbbfp0mxlivf6jhsmiymzpzj"

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
}:

let

spec-tests-mvp = pkgs.fetchFromGitHub {
     owner  = "WebAssembly";
     repo   = "testsuite";
     rev    = "35c50bf6fbb002cfdc1227b0af731bdcaf877714";
     sha256 = "0difcpya5i7fc4xdrysx49186x9vh5yhm88dqpmfppj7ddj39l9i";
  };

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

in drv.overrideAttrs(old:
  { WASM_SPEC_TESTS_MVP = spec-tests-mvp;
    WASM_SPEC_TESTS = spec-tests;
  })
