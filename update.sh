#!/bin/bash
uptime | cut -d : -f 5- > status.txt
emacsclient -e '(mapconcat (function (lambda (x) (car (filename-of-path x)))) (delq nil (mapcar (quote buffer-file-name) (buffer-list))) ";")'| tr -d \" | tr ';' '\n'  >>status.txt 
cat status.txt | z pbmtext -font pbmtext_atari6x6.pbm | pnmcrop -white >p1.pbm
convert blank.pbm p1.pbm -composite pbm:- | ./pbm2lpbm >/tmp/g13-0

