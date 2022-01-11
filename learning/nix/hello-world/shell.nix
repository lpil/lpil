#
# $ nix-shell
# $ nix-shell --pure
#

{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  buildInputs = [
    hello
  ];
}
