{ pkgs ? (import <nixpkgs> {}).pkgs }:

let

   env = pkgs.haskell-ng.packages.ghc784.ghcWithPackages (p: with p; [
     sexp wai wai-conduit warp conduit-extra conduit-combinators logict stm monad-logger text-format MonadRandom
     ghc-mod
   ]);
in

 pkgs.stdenv.mkDerivation {
   name = "ggp";
   doCheck = false;
   doHaddock = false;
   buildInputs = [ env ];
   shellHook = ''
     export NIX_GHC="${env}/bin/ghc"
     export NIX_GHCPKG="${env}/bin/ghc-pkg"
     export NIX_GHC_DOCDIR="${env}/share/doc/ghc/html"
     export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
   '';
}
