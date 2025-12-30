{ pkgs ? import <nixpkgs> { } }:

let
  randstr = pkgs.callPackage ./nix/package.nix { src = ./.; };
in
pkgs.mkShell {
  inputsFrom = [ randstr ];
  packages = with pkgs; [ just git ];
}
