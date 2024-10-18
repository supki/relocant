{ pkgs ? import <nixpkgs> { }
, ghc ? pkgs.haskell.compiler.ghc965
}:

pkgs.mkShell rec {
  buildInputs = with pkgs; [
    ghc
    glibcLocales
    gmp
    postgresql_16
    stack
    zlib
  ];

  shellHook = ''
  export \
      LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath buildInputs}" \
      PGDATABASE=relocant \
      PGDATA=$PWD/.pg/data \
      PGHOST=$PWD/.pg/host \
      PGPORT=5432 \
      PGUSER=$USER

  trap './asset/wrap-pg_ctl stop --no-wait' EXIT
  ./asset/wrap-pg_ctl start
  '';
}

