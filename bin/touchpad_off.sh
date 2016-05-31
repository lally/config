#!/bin/bash
DEVICENUM=$(xinput list | grep TouchPad | cut -d = -f 2 | cut -f 1)
echo Disabling device ${DEVICENUM}: $(xinput list | grep TouchPad)
xinput set-prop $DEVICENUM "Device Enabled" 0
