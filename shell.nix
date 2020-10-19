with import (import ./nix/sources.nix).nixpkgs {};

mkShell {
  buildInputs = [
    dhall
    nodejs
    purescript
    spago
  ];
}
