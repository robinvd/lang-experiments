{ mkDerivation, base, bound, containers, deriving-compat
, megaparsec, mtl, stdenv, text
}:
mkDerivation {
  pname = "scripter";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bound containers deriving-compat megaparsec mtl text
  ];
  license = stdenv.lib.licenses.mit;
}
