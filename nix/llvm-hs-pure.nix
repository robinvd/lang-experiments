{ mkDerivation, attoparsec, base, bytestring, containers, mtl
, stdenv, tasty, tasty-hunit, tasty-quickcheck, template-haskell
, transformers, fetchgit, unordered-containers, fail
}:
mkDerivation {
  pname = "llvm-hs-pure";
  version = "5.1.0";
  src = fetchgit {
    url = https://github.com/llvm-hs/llvm-hs;
    rev = "f9dbd0a79e1ef1917878e7756690510c0ea153e3";
    sha256 = "1m1hnvi80imf3lvxnlbf0zayayscy3r8akins3nndrfg0ry4mdi8";
  } + "/llvm-hs-pure";
  libraryHaskellDepends = [
    unordered-containers attoparsec base bytestring containers mtl template-haskell
    transformers fail
  ];
  testHaskellDepends = [
    base containers mtl tasty tasty-hunit tasty-quickcheck transformers
  ];
  homepage = "http://github.com/llvm-hs/llvm-hs/";
  description = "Pure Haskell LLVM functionality (no FFI)";
  license = stdenv.lib.licenses.bsd3;
}
