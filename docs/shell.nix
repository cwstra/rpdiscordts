let
  pkgs = import <nixpkgs> {};
  currentPath = builtins.toString ./.;
in pkgs.mkShell rec {
  name = "rpdiscordts-docs";

  buildInputs = with pkgs; [
    pandoc
    sphinx
  ];
}
