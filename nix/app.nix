{ mkDerivation, base, bound, bytestring, containers
, deriving-compat, llvm-hs, llvm-hs-pure, megaparsec, mtl
, optparse-applicative, pretty-simple, stdenv, text
}:
mkDerivation {
  pname = "scripter";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bound bytestring containers deriving-compat llvm-hs
    llvm-hs-pure megaparsec mtl pretty-simple text
  ];
  executableHaskellDepends = [
    base containers mtl optparse-applicative text
  ];
  license = stdenv.lib.licenses.mit;
}
