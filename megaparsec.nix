{ mkDerivation, base, bytestring, case-insensitive, containers
, criterion, deepseq, fetchgit, hspec, hspec-discover
, hspec-expectations, mtl, parser-combinators, QuickCheck
, scientific, stdenv, text, transformers, weigh
}:
mkDerivation {
  pname = "megaparsec";
  version = "6.3.0";
  src = fetchgit {
    url = "https://github.com/mrkkrp/megaparsec";
    sha256 = "00zgrwjzgcimvk3bq8x9b619ldbgs26w61zmzjgbzss4y4jy9lzz";
    rev = "fd7f214e543d513d73aef34cf2dd92a7af8ad75d";
  };
  libraryHaskellDepends = [
    base bytestring case-insensitive containers deepseq mtl
    parser-combinators scientific text transformers
  ];
  testHaskellDepends = [
    base bytestring containers hspec hspec-expectations mtl QuickCheck
    scientific text transformers
  ];
  testToolDepends = [ hspec-discover ];
  benchmarkHaskellDepends = [ base criterion deepseq text weigh ];
  homepage = "https://github.com/mrkkrp/megaparsec";
  description = "Monadic parser combinators";
  license = stdenv.lib.licenses.bsd2;
}
