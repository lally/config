#!/bin/bash
unset TMUX
pkill kwin 
sleep 1
LOCALPATH=$(pwd)
cd "$HOME"
${LOCALPATH}/.cabal-sandbox/bin/super-xmonad
