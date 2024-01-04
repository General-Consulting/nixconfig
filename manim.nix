{ pkgs ? import <nixpkgs> {}, pythonPackages ? pkgs.python3Packages }:

pythonPackages.buildPythonPackage rec {
  pname = "manim";
  version = "0.18.0";  # Replace with the correct version

  src = pythonPackages.fetchPypi {
    inherit pname version;
    sha256 = "sha256-0000000000000000000000000000000000000000000=";  # Replace with the correct hash
  };

  doCheck = false;

  propagatedBuildInputs = [ pythonPackages.numpy ];  # Add necessary dependencies

  # Additional configuration if needed
}
