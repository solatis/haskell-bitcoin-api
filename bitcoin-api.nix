{ mkDerivation, aeson, base, base58string, binary, bitcoin-block
, bitcoin-script, bitcoin-tx, bitcoin-types, bytestring, hexstring
, hspec, http-client, lens, lens-aeson, stdenv, text
, unordered-containers, wreq
}:
mkDerivation {
  pname = "bitcoin-api";
  version = "0.12.2";
  src = ./.;
  buildDepends = [
    aeson base base58string binary bitcoin-block bitcoin-script
    bitcoin-tx bitcoin-types bytestring hexstring lens lens-aeson text
    unordered-containers wreq
  ];
  testDepends = [
    base base58string bitcoin-script bitcoin-tx bytestring hspec
    http-client lens text wreq
  ];
  homepage = "http://www.leonmergen.com/opensource.html";
  description = "Provides access to the RPC API of Bitcoin Core";
  license = stdenv.lib.licenses.mit;
}
