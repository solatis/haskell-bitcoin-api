{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:
nixpkgs.haskellPackages.callPackage ./bitcoin-api.nix { }
