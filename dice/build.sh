#!/usr/bin/env bash
set -euo pipefail

rm default.nix
nix-shell --pure -p cabal2nix --run "cabal2nix ." > default.nix
nix-build release.nix
