#!/bin/sh
# Start with:
#  - Get arduino-1.6.9.tar.xz from https://www.arduino.cc/en/Main/Donate
#  - Put it in ~/config/docker/arduino-ide
#  - Make it with docker build -t arduino-1.6.9
# Obviously update to newer arduino builds as I see fit.  Maybe even generalize the script.
docker run --name arduino --rm -it -e DISPLAY=$DISPLAY -v /tmp/.X11-unix:/tmp/.X11-unix arduino-1.6.9 ./arduino

