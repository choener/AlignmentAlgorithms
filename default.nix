{ mkDerivation, ADPfusion, base, bytestring, containers, fmlist
, FormalGrammars, GrammarProducts, PrimitiveArray, stdenv, vector
}:
mkDerivation {
  pname = "AlignmentAlgorithms";
  version = "0.1.1.0";
  src = ./.;
  libraryHaskellDepends = [
    ADPfusion base bytestring containers fmlist FormalGrammars
    GrammarProducts PrimitiveArray vector
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/choener/AlignmentAlgorithms";
  description = "Collection of alignment algorithms";
  license = stdenv.lib.licenses.gpl3;
}
