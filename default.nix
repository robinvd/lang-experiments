{system ? builtins.currentSystem}:
let
  nixpkgs = fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-18.03.tar.gz;
  pkgs = import nixpkgs { config = {}; };

  # pkgs = import <nixpkgs> {inherit system; };
  myCall = pkgs.lib.callPackageWith (pkgs // pkgs.haskell.packages.ghc822 // jobs);

  callCabal2nix = pkgs.haskellPackages.callCabal2nix;
  myCallCabal2nix = name: src: args: if builtins.typeOf src != "path"
    then myCall (pkgs.haskellPackages.haskellSrc2nix { inherit name src; }) args
    else
      # When `src` is a Nix path literal, only use `cabal2nix` on
      # the cabal file, so that the "import-from-derivation" is only
      # recomputed when the cabal file changes, and so your source
      # code isn't duplicated into the nix store on every change.
      # This can only be done when `src` is a Nix path literal
      # because that is the only kind of source that
      # `builtins.filterSource` works on. But this filtering isn't
      # usually important on other kinds of sources, like
      # `fetchFromGitHub`.
      pkgs.haskellPackages.overrideCabal (myCall (pkgs.haskellPackages.haskellSrc2nix {
        inherit name;
        src = builtins.filterSource (path: type: pkgs.lib.hasSuffix ".cabal" path) src;
      }) args) (_: { inherit src; });

  jobs = pkgs.lib.attrsets.mapAttrs (name: value: pkgs.haskell.lib.dontHaddock (pkgs.haskell.lib.dontCheck value)) {
    llvm-hs = myCall ./nix/llvm-hs.nix {};
    llvm-hs-pure = myCall ./nix/llvm-hs-pure.nix {};
    megaparsec = myCall ./nix/megaparsec.nix {};
    llvm-hs-pretty = myCall (pkgs.fetchgit {
      url = "https://github.com/llvm-hs/llvm-hs-pretty";
      rev = "ddc95f9bc6dd7b5690e96a4c548c059bcee43dd1";
      sha256 = "04p2ag2ishspa3ms9zc8xwzdr12ghf0ysgn9v53jblldv3dzl62x";
    }) {};

  };
  rlang = myCall ./nix/app.nix {};
in
if pkgs.lib.inNixShell then rlang.env else rlang
