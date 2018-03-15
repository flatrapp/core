{ mkDerivation, aeson, base, base16-bytestring, bytestring
, configurator, containers, cryptohash, cryptonite, esqueleto
, formatting, http-types, hvect, iso8601-time, jwt, mime-mail
, monad-logger, mtl, network, persistent, persistent-sqlite
, persistent-template, random, regex-pcre, smtp-mail, Spock, stdenv
, text, time, transformers, word8
}:
mkDerivation {
  pname = "Flatr-App-Core";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base base16-bytestring bytestring configurator containers
    cryptohash cryptonite esqueleto formatting http-types hvect
    iso8601-time jwt mime-mail monad-logger mtl network persistent
    persistent-sqlite persistent-template random regex-pcre smtp-mail
    Spock text time transformers word8
  ];
  homepage = "https://github.com/flatrapp/core";
  license = stdenv.lib.licenses.asl20;
}
