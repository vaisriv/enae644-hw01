{ pkgs, compiler ? "ghc910" }:
let
    myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
        overrides = self: super: rec {
            enae644-hw01 = self.callCabal2nix "enae644-hw01" (./enae644-hw01) {};
        };
    };
in
    myHaskellPackages.callCabal2nix "enae644-hw01" (../../.) { }
