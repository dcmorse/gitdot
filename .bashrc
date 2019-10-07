# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

git-branch-for-prompt() {
  git symbolic-ref --short HEAD 2> /dev/null
}

RED="$(tput setaf 1)"
GREEN="$(tput setaf 2)"
YELLOW="$(tput setaf 3)"
BLUE="$(tput setaf 4)"
RESET="$(tput sgr0)"

if [ "$color_prompt" = yes ]; then
  # this yellow business caused weird redisplay bugs:
  # PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\] ${YELLOW}$(git-branch-for-prompt)${RESET} \[\033[01;34m\]\w\[\033[00m\]\$ '
  PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\] $(git-branch-for-prompt) \[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h $(git-branch-for-prompt) \w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

alias gs='git status -s'
alias gst='git status -s'
alias gss='git status'
alias gd='git diff'
alias gb='git branch'
alias gl='git log'
alias gd='git diff'
alias gp='git push'
alias gc='git commit'
alias gcm='git commit -m'
alias gap='git add -p'
alias zr='zeus rspec'
alias be='bundle exec'
alias lcsql='mysql -u root -pMyNewPass launchcode_dev'

# not proud.
alias zues='zeus'

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

export EDITOR=emacsclient

# Launchcode
export MYSQL_HOST=localhost
export MYSQL_DATABASE=launchcode_dev
export MYSQL_USERNAME=root


alias fds='cd /home/dm/launch_code'
alias rew='cd /home/dm/contra'
alias lcca='cd /home/dm/launchcode_company_admin'
alias hjb='cd /home/dm/hot_job_bot'


# em: open an existing emacs on the files given
# if the first argument ends in :DDDD where DDDD is one or more digits, open
# emacs on that (one) file at that line number
# old version was this: alias em='emacsclient --no-wait'
function em ()
{
  if test rspec = "$1" ;
  then shift; echo 'hi'
  fi;
  if echo "$1" | egrep -q ':[0-9]+$';
  then
      MYFILE=`echo "$1" | sed 's/:.*$//'`
      MYLINE=`echo "$1" | sed 's/^.*://'`
      emacsclient --no-wait "+$MYLINE" "$MYFILE"
  else emacsclient --no-wait "$@";
  fi;
}

. $HOME/.supersecret.sh

if [ -d $HOME/.rbenv/bin ]; then
  export PATH="$HOME/.rbenv/bin:$PATH"
  eval "$(rbenv init -)"
  export PATH="$HOME/.rbenv/plugins/ruby-build/bin:$PATH"
fi

if [ -f ~/.rvm/scripts/rvm ]; then
  . ~/.rvm/scripts/rvm
fi

if [ -d /usr/local/heroku/bin ]; then
  export PATH="/usr/local/heroku/bin:$PATH"
fi

# fuzzy find
ff ()
{
  acc='*'
  while (( "$#" )); do
    acc="$acc$1*"
    shift
  done
  # echo find . -iname "$acc"
  find . -iname "$acc"
}

alias contradb='ssh rails@45.55.252.228'
export CONTRADB='rails@45.55.252.228'
export CONTRADB_IP='45.55.252.228'

# also user root is an option
alias hot-job-bot='ssh hot-job-bot@165.227.4.101'
export HOT_JOB_BOT_IP='165.227.4.101'

stty -ixon # disable control-s shenanigans via Sarah magic
alias diff=colordiff
alias ag='\ag --pager="less -XFR"'

contradb_backup_file="$HOME/priv/contradb/contradb-$(date -u +%Y-%m-%d).sql"
case "$-" in
*i*)	if ! [[ -r "$contradb_backup_file" && -s "$contradb_backup_file" ]]
then
  echo "time to run contradb-backup"
fi
;;
esac

# added by Miniconda3 installer
export PATH="/home/dm/miniconda3/bin:$PATH"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# function c () {
#   $@ PATH="$HOME/contradb/bin:$PATH"
# }

export LAUNCH_CODE_CAPYBARA_DEFAULT_MAX_WAIT_TIME=2
