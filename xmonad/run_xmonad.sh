#!/bin/sh

pkill kwin
sleep 1
dist/build/xmonad/xmonad &
