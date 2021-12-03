{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, async, attoparsec, base, bytestring
      , conduit, conduit-extra, containers, criterion, deepseq, dns
      , exceptions, feed, filepath, fsnotify, hpack, hspec, http-conduit
      , iconv, idna, iproute, lib, monad-control, monad-logger, network
      , QuickCheck, resourcet, stm, string-interpolate, text, time
      , transformers, unliftio-core, weigh, yaml
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
        libraryToolDepends = [ hpack ];
        executableHaskellDepends = [
          aeson async attoparsec base bytestring conduit conduit-extra
          containers deepseq dns exceptions feed filepath fsnotify
          http-conduit iconv idna iproute monad-control monad-logger network
          resourcet stm string-interpolate text time transformers
          unliftio-core yaml
        ];
        testHaskellDepends = [
          attoparsec base bytestring containers deepseq dns hspec idna
          iproute QuickCheck stm text time
        ];
        benchmarkHaskellDepends = [
          attoparsec base bytestring containers criterion deepseq dns iconv
          idna iproute stm text time weigh
        ];
        prePatch = "hpack";
        description = "Build optimized lists of blocked IP addresses in Russia";
        license = lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
