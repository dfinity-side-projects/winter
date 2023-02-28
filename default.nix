{ rev    ? "6047d0269b0006756103db57bd5e47b8c4b6381b" # release-22.11
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

haskellPackages = pkgs.haskellPackages;

# wabt >= 1.0.25 (until at least 1.0.32) cause winter's test suite to fail on
# the line: https://github.com/WebAssembly/testsuite/blob/35c50bf6fbb002cfdc1227b0af731bdcaf877714/elem.wast#L15
# with the error:
#
#   line 4
#     user error (./test1296-222.wat:13:4: error: redefinition of elem "$t"
#       (elem $t (i32.const 0) $f $f)
#        ^^^^
#     ./test1296-222.wat:14:4: error: redefinition of elem "$t"
#       (elem $t (offset (i32.const 0)))
#        ^^^^
#     ./test1296-222.wat:15:4: error: redefinition of elem "$t"
#       (elem $t (offset (i32.const 0)) $f $f)
#        ^^^^
#     )
#
# So we keep wabt at 1.0.24:
wabt = pkgs.wabt.overrideAttrs (_old: rec {
  name = "wabt-${version}";
  version = "1.0.24";
  src = pkgs.fetchFromGitHub {
    owner = "WebAssembly";
    repo = "wabt";
    rev = version;
    sha256 = "sha256-/blukivL6+xsnChxDp5gCr5w8S3bBuhO459YkLGxYmA=";
    fetchSubmodules = true;
  };
});

drv = haskellPackages.developPackage {
  name = "winter";
  root = ./.;
  overrides = with pkgs.haskell.lib; self: super: {
  };

  source-overrides = {};

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools = (attrs.buildTools or []) ++ [
      wabt
      pkgs.cabal-install
      haskellPackages.haskell-language-server
    ];

    passthru = {
      nixpkgs = pkgs;
    };

  });

  inherit returnShellEnv;
};

in drv.overrideAttrs(old: { WASM_SPEC_TESTS = "${spec-tests}"; })
