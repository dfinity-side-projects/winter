{ rev    ? "6047d0269b0006756103db57bd5e47b8c4b6381b" # release-21.11
, sha256 ? "sha256:0hsvb1z8nx9alrhix16bcdjnsa6bv39n691vw8bd1ikvbri4r8yv"

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

spec-tests = pkgs.fetchFromGitHub {
    owner  = "WebAssembly";
    repo   = "testsuite";
    rev    = "35c50bf6fbb002cfdc1227b0af731bdcaf877714";
    sha256 = "0difcpya5i7fc4xdrysx49186x9vh5yhm88dqpmfppj7ddj39l9i";
  };

drv = pkgs.haskell.packages.ghc92.developPackage {
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
