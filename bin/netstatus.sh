#!/bin/bash
if [ $(nmcli -t --fields NAME c status | wc -l - | awk '{print $1}') -gt 0 ]
then
	echo "<fc=cyan>$(nmcli -t --fields NAME c status | tail -1): $(ip addr show | /bin/grep 'inet ' | /bin/grep -v 127.0.0.1 | tail -1 | awk '{print $2}')</fc>"
else
	echo "<fc=grey>Unconnected</fc>"
fi

