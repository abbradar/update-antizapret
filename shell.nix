{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages_ = if compiler == "default"
                        then pkgs.haskellPackages
                        else pkgs.haskell.packages.${compiler};

  lib = pkgs.haskell.lib;

  haskellPackages = haskellPackages_.override {
    overrides = self: super: {
      conduit-iconv = self.callPackage ./conduit-iconv.nix { };
    };
  };

  drv = lib.doBenchmark (haskellPackages.callPackage ./default.nix { });

in

  if pkgs.lib.inNixShell then drv.env else drv
