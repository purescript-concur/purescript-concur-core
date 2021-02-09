{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    buildInputs = [ pkgs.spago pkgs.nodejs pkgs.purescript ];
}