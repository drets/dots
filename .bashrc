source ~/.git-prompt.sh

export PS1='\w\[\033[01;33m\]$(__git_ps1)\[\033[01;34m\] \$\[\033[00m\] '
export GREP_OPTIONS="--color"
export PATH="/usr/local/bin:/usr/bin:$HOME/.local/bin:$PATH"

alias p8='ping 8.8.8.8'
alias rm='echo "rm is disabled, use trash or /bin/rm instead."'
alias e='open /Applications/Emacs.app'

function compare-images() {
    compare -metric AE -fuzz %5 $1 $2 diff.png
}
