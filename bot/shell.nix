let pkgs = import <nixpkgs> {};

in pkgs.mkShell rec {
  name = "rpdiscordts";

  shellHook = ''
    set -o allexport
    source .env
    set +o allexport
  '';
  buildInputs = with pkgs; [
    nodejs-16_x
    (yarn.override { nodejs = nodejs-16_x; })
  ];
}
