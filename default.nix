{ test ? "true" }:
let bootstrap = import <nixpkgs> {};
    reflex-platform-commit = builtins.fromJSON (builtins.readFile ./reflex-platform.json);
    reflex-platform-src = bootstrap.fetchFromGitHub {
      owner = "reflex-frp";
      repo  = "reflex-platform";
      inherit (reflex-platform-commit) rev sha256;
    };
    reflex-platform = import reflex-platform-src {};
    parseBool = str: with builtins;
      let json = fromJSON str; in if isBool json then json else throw "parseBool: ${str} is not a bool";
    overrides = reflex-platform.ghcjs.override {
      overrides = self: super:
        with reflex-platform; with self;
        let testFun  = if parseBool test then x: x else lib.dontCheck;
        in {
          # servant-reflex = callPackage ./deps/servant-reflex.nix {};
          servant-display = testFun (callPackage (cabal2nixResult ./.) {});
        };
    };
    drv = overrides.servant-display;
in if reflex-platform.nixpkgs.lib.inNixShell then
  reflex-platform.workOn overrides drv
else
  drv
