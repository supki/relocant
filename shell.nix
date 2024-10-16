{ pkgs ? import <nixpkgs> { }
, ghc ? pkgs.haskell.compiler.ghc965
}:

pkgs.mkShell rec {
  buildInputs = with pkgs; [
    ghc
    glibcLocales
    gmp
    stack
    zlib
  ];

  shellHook = ''
    export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath buildInputs}"
  '';
}
