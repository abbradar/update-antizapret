{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, attoparsec, base, bytestring, containers
      , criterion, encoding, hspec, iconv, iproute, QuickCheck, stdenv
      , text
      }:
      mkDerivation {
        pname = "update-antizapret";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          attoparsec base containers iproute text
        ];
        executableHaskellDepends = [
          attoparsec base bytestring encoding text
        ];
        testHaskellDepends = [ base hspec iproute QuickCheck ];
        benchmarkHaskellDepends = [
          attoparsec base bytestring criterion iconv text
        ];
        description = "Build optimized lists of blocked IP addresses in Russia";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
