let
  pkgs = import <nixpkgs> {};
  currentPath = builtins.toString ./.;
in pkgs.mkShell rec {
  name = "rpdiscordts";

  shellHook = ''
    set -o allexport
    . ${currentPath}/.env
    set +o allexport
  '';
  buildInputs = with pkgs; [
    nodejs-18_x
    (yarn.override { nodejs = nodejs-18_x; })
  ];
}
