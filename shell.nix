{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, async, attoparsec, base, bytestring
      , conduit, conduit-extra, containers, criterion, deepseq, dns
      , exceptions, feed, filepath, fsnotify, hspec, http-conduit, iconv
      , idna, interpolatedstring-perl6, iproute, monad-control
      , monad-logger, network, QuickCheck, resourcet, stdenv, stm, text
      , time, transformers, unliftio-core, yaml
      }:
      mkDerivation {
        pname = "update-antizapret";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        enableSeparateDataOutput = true;
        libraryHaskellDepends = [
          attoparsec base bytestring containers deepseq dns idna iproute stm
          text time
        ];
        executableHaskellDepends = [
          aeson async attoparsec base bytestring conduit conduit-extra
          containers dns exceptions feed filepath fsnotify http-conduit iconv
          interpolatedstring-perl6 monad-control monad-logger network
          resourcet stm text time transformers unliftio-core yaml
        ];
        testHaskellDepends = [ base containers hspec iproute QuickCheck ];
        benchmarkHaskellDepends = [
          attoparsec base bytestring containers criterion iconv text
        ];
        doBenchmark = true;
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
