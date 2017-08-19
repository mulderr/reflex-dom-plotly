{ mkDerivation, aeson, base, containers, data-default, ghcjs-dom
, lens, reflex-dom, stdenv, text
}:
mkDerivation {
  pname = "reflex-dom-plotly";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base containers data-default ghcjs-dom lens reflex-dom text
  ];
  license = stdenv.lib.licenses.bsd3;
}
