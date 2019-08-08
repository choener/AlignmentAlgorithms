{ mkDerivation, ADPfusion, base, bytestring, containers, criterion
, fmlist, FormalGrammars, GrammarProducts, PrimitiveArray
, SciBaseTypes, stdenv, text, text-metrics, vector
}:
mkDerivation {
  pname = "AlignmentAlgorithms";
  version = "0.1.1.0";
  src = ./.;
  libraryHaskellDepends = [
    ADPfusion base bytestring containers fmlist FormalGrammars
    GrammarProducts PrimitiveArray SciBaseTypes text vector
  ];
  testHaskellDepends = [ base ];
  benchmarkHaskellDepends = [
    ADPfusion base bytestring containers criterion fmlist
    FormalGrammars GrammarProducts PrimitiveArray SciBaseTypes text
    text-metrics vector
  ];
  homepage = "https://github.com/choener/AlignmentAlgorithms";
  description = "Collection of alignment algorithms";
  license = stdenv.lib.licenses.bsd3;
}
