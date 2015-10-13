source ~/.git-prompt.sh

export JAVA_HOME=$(/usr/libexec/java_home)
export PS1='\w\[\033[01;33m\]$(__git_ps1)\[\033[01;34m\] \$\[\033[00m\] '
export PATH="/usr/local/bin:/usr/bin:$PATH"
export GREP_OPTIONS="--color"

alias ls='ls -GFhl'
alias ccat='pygmentize -g'
alias p8='ping 8.8.8.8'
alias v='vim'
alias e='open -a emacs'
alias g='git'

export PATH="~/.cabal/bin:$PATH"

# autocomplete for 'g'
complete -o default -o nospace -F _git g

function compare-images() {
    compare -verbose -metric AE $1 $2 null
}

if [ -f ~/.git-completion.bash ]; then
  . ~/.git-completion.bash
fi
