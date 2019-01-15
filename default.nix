{ compiler ? "ghc844"

, rev    ? "61c3169a0e17d789c566d5b241bfe309ce4a6275"
, sha256 ? "0qbycg7wkb71v20rchlkafrjfpbk2fnlvvbh3ai9pyfisci5wxvq"

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

wasm = pkgs.ocamlPackages.wasm.overrideAttrs (attrs: {
  src = pkgs.fetchFromGitHub {
    owner  = "WebAssembly";
    repo   = "spec";
    rev    = "a56cf2ec042da382f0196fe14dcbd7ff2e973466";
    sha256 = "02c7bpmjmq3bxp4w0g8gkg87ixh4x67d9hz4g25i4snxnc7bj0g7";
    # date = 2018-10-31T18:30:05+01:00;
  };
  postInstall = attrs.postInstall + ''
    mkdir -p $out/test
    cp -pR test/core $out/test
  '';
});

drv = haskellPackages.developPackage {
  name = "winter";
  root = ./.;

  overrides = with pkgs.haskell.lib; self: super: {
  };

  source-overrides = {};

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools = (attrs.buildTools or []) ++ [
      pkgs.wabt wasm
    ];

    passthru = {
      nixpkgs = pkgs;
    };
  });

  inherit returnShellEnv;
};

in drv
