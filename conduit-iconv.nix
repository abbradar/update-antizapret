{ mkDerivation, base, bytestring, conduit, criterion, fetchgit, lib
, mtl, QuickCheck, test-framework, test-framework-quickcheck2, text
}:
mkDerivation {
  pname = "conduit-iconv";
  version = "0.1.2.0";
  src = fetchgit {
    url = "https://github.com/abbradar/conduit-iconv";
    sha256 = "17svnyq88qjcfqilxw3qx89v3g6jmf8j9ibsn2cmypxf8ygxl20f";
    rev = "cc950291caa08e87a8418e2db4faacc950b617ab";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base bytestring conduit ];
  testHaskellDepends = [
    base bytestring conduit mtl QuickCheck test-framework
    test-framework-quickcheck2 text
  ];
  benchmarkHaskellDepends = [
    base bytestring conduit criterion mtl text
  ];
  homepage = "https://github.com/sdroege/conduit-iconv";
  description = "Conduit for character encoding conversion";
  license = lib.licenses.bsd3;
}
