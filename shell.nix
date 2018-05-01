{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, attoparsec, base, bytestring, conduit
      , conduit-extra, containers, criterion, deepseq, exceptions, feed
      , filepath, fsnotify, hspec, http-conduit, iconv
      , interpolatedstring-perl6, iproute, monad-logger, QuickCheck
      , resourcet, stdenv, stm, text, transformers, unliftio-core, yaml
      }:
      mkDerivation {
        pname = "update-antizapret";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          attoparsec base bytestring containers deepseq iproute text
        ];
        executableHaskellDepends = [
          aeson attoparsec base bytestring conduit conduit-extra exceptions
          feed filepath fsnotify http-conduit iconv interpolatedstring-perl6
          monad-logger resourcet stm text transformers unliftio-core yaml
        ];
        testHaskellDepends = [ base hspec iproute QuickCheck ];
        benchmarkHaskellDepends = [
          attoparsec base bytestring containers criterion iconv text
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
