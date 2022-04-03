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
    nodejs-16_x
    (yarn.override { nodejs = nodejs-16_x; })
  ];
}
