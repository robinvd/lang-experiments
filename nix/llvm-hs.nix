{ mkDerivation, array, attoparsec, base, bytestring, Cabal
, containers, exceptions, llvm_6, llvm-hs-pure, mtl
, pretty-show, QuickCheck, stdenv, tasty, tasty-hunit
, tasty-quickcheck, template-haskell, temporary, transformers
, utf8-string, fetchgit
}:
mkDerivation {
  pname = "llvm-hs";
  version = "5.1.0";
  src = fetchgit {
    url = https://github.com/llvm-hs/llvm-hs;
    rev = "f9dbd0a79e1ef1917878e7756690510c0ea153e3";
    sha256 = "1m1hnvi80imf3lvxnlbf0zayayscy3r8akins3nndrfg0ry4mdi8";
  } + "/llvm-hs";
  setupHaskellDepends = [ base Cabal containers ];
  libraryHaskellDepends = [
    array attoparsec base bytestring containers exceptions llvm-hs-pure
    mtl template-haskell transformers utf8-string
  ];
  libraryToolDepends = [ llvm_6 ];
  testHaskellDepends = [
    base bytestring containers llvm-hs-pure mtl pretty-show QuickCheck
    tasty tasty-hunit tasty-quickcheck temporary transformers
  ];
  homepage = "http://github.com/llvm-hs/llvm-hs/";
  description = "General purpose LLVM bindings";
  license = stdenv.lib.licenses.bsd3;
}
