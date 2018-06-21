with import <nixpkgs> {};
with haskell.lib;

rec {
  hsSrcSet = (lib.foldl' (s: p: s // (import p).hsSrcSet) {} [
    ../Lib-ADPfusion
    ../Lib-FormalGrammars
    ../Lib-GrammarProducts
    ../Lib-PrimitiveArray
  ]) // {AlignmentAlgorithms = ./.;};
  hsPkgs = haskellPackages.extend (packageSourceOverrides hsSrcSet);
  hsShell = with hsPkgs; shellFor {
    packages = p: [ p.AlignmentAlgorithms ];
    withHoogle = true;
    buildInputs = [
      cabal-install ghc
      ADPfusion
      FormalGrammars
      GrammarProducts
      PrimitiveArray
    ];
  };
}
