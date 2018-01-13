{ mkDerivation, aeson, base, base16-bytestring, bytestring
, configurator, containers, cryptohash, http-types
, hvect, jwt, monad-logger, mtl, persistent, persistent-sqlite
, persistent-template, random, Spock, stdenv, text, time
, transformers, word8
, cabal-install
}:
mkDerivation {
  pname = "Flatr-App-Core";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [
    aeson base base16-bytestring bytestring configurator containers
    cryptohash http-types hvect jwt monad-logger mtl persistent
    persistent-sqlite persistent-template random Spock text time
    transformers word8
  ];
  testHaskellDepends = [ base ];
  buildDepends = [ cabal-install ];
  homepage = "https://github.com/flatrapp/core";
  license = stdenv.lib.licenses.asl20;
}
