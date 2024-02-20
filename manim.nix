{ pkgs ? import <nixpkgs> { }, pythonPackages ? pkgs.python3Packages }:

let poetry2nix = pkgs.poetry2nix.override { python = pythonPackages.python; };
in poetry2nix.mkPoetryApplication rec {
  pname = "manim";
  version = "0.18.0"; # Replace with the correct Manim version

  src = pythonPackages.fetchPypi {
    inherit pname version;
    sha256 = "sha256-VniAZrwa7CRxqYjJHjNxlP040QNe0bTRCDi/5kvSavg=";
  };
}
