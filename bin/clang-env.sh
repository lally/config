#!/bin/sh
nix-shell ~/config/nix/clang.nix -A clangEnv --run zsh "$@"
