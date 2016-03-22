source ~/.git-prompt.sh

export PS1='\w\[\033[01;33m\]$(__git_ps1)\[\033[01;34m\] \$\[\033[00m\] '
export PATH="/usr/local/bin:/usr/bin:$PATH"
export PATH="~/.cabal/bin:$PATH"
export GREP_OPTIONS="--color"

alias ls='ls -GFhl'
alias ccat='pygmentize -g'
alias p8='ping 8.8.8.8'
alias g='git'
alias rm='echo "rm is disabled, use trash or /bin/rm instead."'

# autocomplete for 'g'
complete -o default -o nospace -F _git g

function compare-images() {
    compare -metric AE -fuzz %5 $1 $2 diff.png
}

function cd() {
   builtin cd "$*" && ls
}

if [ -f ~/.git-completion.bash ]; then
  . ~/.git-completion.bash
fi
