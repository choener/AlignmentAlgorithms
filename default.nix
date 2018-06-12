with import <nixpkgs> {};
with haskell.lib;

rec {
  packageOverrides = haskellPackages.override {
    overrides = self: super: {
      # old doctest
      pipes-group = dontCheck super.pipes-group;
    };
  };
  hsPkgs = packageOverrides.extend (packageSourceOverrides {
    ADPfusion = ../Lib-ADPfusion;
    AlignmentAlgorithms = ./.;
    bimaps = ../Lib-bimaps;
    DPutils = ../Lib-DPutils;
    #ForestStructures = ../Lib-ForestStructures;
    FormalGrammars = ../Lib-FormalGrammars;
    GrammarProducts = ../Lib-GrammarProducts;
    #InternedData = ../Lib-InternedData;
    #NaturalLanguageAlphabets = ../Lib-NaturalLanguageAlphabets;
    OrderedBits = ../Lib-OrderedBits;
    PrimitiveArray = ../Lib-PrimitiveArray;
    #SciBaseTypes = ../Lib-SciBaseTypes;
    #WordAlignment = ./.;
  });
  hsShell = with hsPkgs; shellFor {
    packages = p: [ p.AlignmentAlgorithms ];
    withHoogle = true;
    buildInputs = [
      cabal-install ghc
      ADPfusion
      bimaps
      DPutils
      #ForestStructures
      FormalGrammars
      GrammarProducts
      #InternedData
      #NaturalLanguageAlphabets
      OrderedBits
      PrimitiveArray
      #SciBaseTypes
    ];
  };
}
