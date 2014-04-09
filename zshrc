# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
#ZSH_THEME="robbyrussell"
#ZSH_THEME="blinks"
ZSH_THEME="clean"
export REPORTTIME=10
#ZSH_THEME="juanghurtado"
#ZSH_THEME="alanpeabody"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
export COMPLETION_WAITING_DOTS="true"
export LESSOPEN="| /usr/share/source-highlight/src-hilite-lesspipe.sh %s"
export LS_COLORS="*~=90:*_test.cc=4:*_unittest.cc=4:*.org=44;37:*BUILD=01;38;5;163:${LS_COLORS}"
export LESS=' -R '
export P4CONFIG=.p4config
export P4DIFF=/home/build/public/google/tools/p4diff
export P4MERGE=/home/build/public/eng/perforce/mergep4.tcl
export P4EDITOR=emacsclient
export ALTERNATE_EDITOR=""
export GREP_OPTIONS='--color=auto'
export PROJECTNAME=
export PROJECTDIR=~
export PROJECTGOOG=~
export PATH=/ulg/bin:~/.cabal/bin:/ulg/home/lally/chromes/depot_tools:$PATH:~/config/git
export MANPATH=/usr/local/texlive/2011/texmf/doc/man:$MANPATH
export INFOPATH=/usr/local/texlive/2011/texmf/doc/info:$INFOPATH
#export JAVA_HOME=/usr/local/buildtools/java/jdk

# alias less='source-highlight --failsafe --infer-lang -f esc | less'
# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git svn zsh-syntax-highlighting tmux git-extras gpg-agent)

#fpath=(/google/src/files/head/depot/google3/devtools/blaze/scripts/zsh_completion $fpath)
#cache-path must exist
#
source $ZSH/oh-my-zsh.sh

# Presumably a fix for slow git prompts.  Nice in non-git directories, but everywhere else, yeesh.
#function git_prompt_info() {
#  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
#  echo "$ZSH_THEME_GIT_PROMPT_PREFIX${ref#refs/heads/}$ZSH_THEME_GIT_PROMPT_SUFFIX)"
#}

#cdpath=( . ~/gitwork ~ )
#export cdpath

# Customize to your needs...
# Shell commands
alias ls='ls --color=auto -F'
alias grpe='grep -n --color=auto'
alias grep='grep -n --color=auto'
alias fgrep='fgrep -n --color=auto'
alias egrep='egrep -n --color=auto'
alias nrep='grep --color=auto -n'
alias enrep='egrep --color=auto -n'
alias ack='ack-grep'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias fu=fileutil
alias bn=~/bin/branch_date.sh
alias vim='emacsclient -t'
alias ec='emacsclient -n'
alias ect='emacsclient -t'
# Custom-built binaries.  The aliases avoid putting /ulg/bin into PATH.
alias emacs='/ulg/bin/emacs'
# Google-specific
alias open='xdg-open'
alias prodvm=/google/data/rw/projects/prod71vm/prod71vm
alias gerrit='ssh prodkernel gerrit'
alias ack='ack-grep'
#alias log_sucker='/google/data/ro/projects/production/tools/log_sucker.par'
##alias runsll='/home/build/static/projects/mustang/run_borg_leaf_loadtest.par'
#alias runsll='/home/build/static/projects/mustang/run_loadtest.par'
#alias kborg='borg --borg=ig-kerneltest --borguser=kernel-librarian-sll'

# Git-specific
alias g5='/google/data/ro/projects/shelltoys/g5.sar'
alias gba='git branch -av'

# TMUX specific aliases
alias tls='tmux ls'
alias ta='tmux attach -t'
alias tn='tmux new -s'

zstyle ':completion:*' hosts off
zstyle ':completion:*' accept-exact '*(N)'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path /var/cache/zsh

alias basis="git5 status | cut -d ' ' -f 4"

function perflab() {
  ~/bin/perlab.sh "$@"
}

__git_files () {
      _wanted files expl 'local files' _files
}

# Move the history off of NFS, as I think it confuses ZSH a bit.
export HISTFILE=/usr/local/google/.zsh_history
setopt APPEND_HISTORY
unsetopt CORRECT_ALL
unsetopt INC_APPEND_HISTORY
zstyle '*' users lally
unsetopt autocd
setopt no_share_history
setopt no_multios

cdpath=

