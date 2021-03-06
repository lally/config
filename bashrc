# Basic operating environment
if [ "${TERM}v" == "xtermv" ]
then
    export TERM=xterm-color
fi

# notify-send "TITLE" "MESSAGE" works for gnome.
export LS=$(which ls)

#  This will take out the color codes for $TERM=dumb, ala emacs shell mode.
if [ "${TERM}v" != "dumbv" ]
then
    #
    # A real terminal
#    export PS1="\[\033[31m\]$PWD\n\[\033[1;34m\][\D{%m/%d} \A::\u@\h]\\$\[\033[0m\] "
    export PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD}\007"'
    export PS1="\\[\033[1;34m\\][\D{%m/%d} \A::\u@\h \W]\\$\\[\033[0m\\] "
    alias ls="$LS --color -F"
    alias ll="$LS --color -lF"
    alias sl="$LS --color -F"
    alias open='xdg-open'
    function title () {
	export PROMPT_COMMAND='echo -ne "\033]0;'$1'\007"'
    }

    function mini () {
	export PS1='\[\033[1;34m\][sol \D{%m/%d} \A]\n\W\$\[\033[0m\] '
    }
    set -o emacs
else
    #
    # Emacs. No color, but otherwise the same.
    export PS1='[\u@\h \W]\$ '
    alias ls="$LS -F"
    alias ll="$LS -lF"
    alias sl="$LS -F"
    # This would probably be inherited from the environment (if I call EMACS from a shell, for example).
    unset PROMPT_COMMAND
    function title () {
	echo "Sorry, you're in emacs."
    }
    
    function mini () {
	export PS1='[sol \D{%m/%d} \A\W]\$ '
    }
fi

if [ "x$DISPLAY" != "x" ]
then
	xset b off
fi


# One of the longer PATHs I've seen.
export PATH=~/bin:$PATH  #:~/.cabal/bin:/usr/sbin:/usr/local/haskell/current/bin

export LC_COLLATE="C"

# Tools
#export JAVA_HOME=/usr/jdk/latest
#export SBCL_HOME=/opt/local/lib/sbcl
#export MANPATH=$MANPATH::/usr/man
#:/usr/man:/usr/sfw/man:/opt/csw/man:/opt/DTT/Man:/opt/sfw/man:/usr/X/man:/usr/X11/man:/usr/X11R6/man
#export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/haskell/current/lib

# Amazon web services
if [ -d ~/.ec2 ]
then
   export EC2_PRIVATE_KEY=~/.ec2/pk-BSL65PPFHEL7GOEIFCN4K7PU7ZRKEFRF.pem
   export EC2_CERT=~/.ec2/cert-BSL65PPFHEL7GOEIFCN4K7PU7ZRKEFRF.pem
   export EC2_HOME=/opt/local/amazon/ec2-api-tools-1.3-34128
   export AMI=`cat ~/.ec2/CURRENT_AMI`
fi

# Set History size
export HISTFILESIZE=3000       # 3k lines of history
export HISTCONTROL=ignoredups  # make those lines unique.

# CLI Usage
if [ "x`hostname`" == "xsol" ]
then
        alias vi=vim
        alias tar=gtar
        alias make=gmake
        alias cpan="pfexec perl -MCPAN -e 'shell'"
        alias gopubs='pushd /research/phd/pubs/2009'
        alias gophd='pushd /research/phd/torque/example/'
        alias scripts='pushd /research/phd/performance_data/scripts'
        alias emacs='/opt/emacs/bin/emacs'
        #alias gowriteup='pushd /research/phd/writeup/'
        alias gowriteup='pushd /research/phd/researchdef/'
        #alias edit='/opt/emacs/bin/emacsclient -cn'
        alias ctags='/opt/local/bin/ctags'
        export LD_LIBRARY_PATH=/lib:/usr/lib:/opt/csw/lib
	ulimit -n 65535
fi

export PAGER=less
export LESS=-R
export VISUAL=vim
alias svndiff='svn diff --diff-cmd diff -x -uw'

#source /opt/src/emacs-22.1/etc/emacs.bash
#. /opt/grid/default/common/settings.sh

#
# Essentially an ls -l of the result of 'which', to help resolve
# what the program really is, and where.
function lw () {
	for i
	do
		if which $i | grep "no $i" > /dev/null
		then 
			echo "No $i found."
		else 
			ls --color -lF `which $i`
		fi
	done
} 

function edit () {
	if pgrep emacs > /dev/null
	then 
		emacsclient -n $* 
	else
		emacs $* &
	fi
}

# Search history
function hgrep () {
	history | grep "$@"
}

# Launch a desktop app that's likely to complain to no end on useless bullshit
function launch () {
	"$@" >/dev/null 2>&1 &
}

function xcat () {
	gxmessage -fn 'mono' -file $1 -title "$1" -buttons "Close:0" -default Close -geometry 790x861+1680+0 &
}

alias vim='emacsclient -t'

export GIT_SSL_CAINFO=/etc/ssl/certs/ca-certificates.crt
