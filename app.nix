{ mkDerivation, base, bound, containers, megaparsec, mtl, stdenv
, text
}:
mkDerivation {
  pname = "scripter";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bound containers megaparsec mtl text
  ];
  license = stdenv.lib.licenses.mit;
}
