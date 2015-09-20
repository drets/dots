source /Library/Developer/CommandLineTools/usr/share/git-core/git-prompt.sh
export JAVA_HOME=$(/usr/libexec/java_home)
export PS1='\w\[\033[01;33m\]$(__git_ps1)\[\033[01;34m\] \$\[\033[00m\] '
export PATH="/usr/local/bin:/usr/bin:$PATH"
export GREP_OPTIONS="--color"
alias ls='ls -GFhl'
alias show='pygmentize -g'
alias p8='ping 8.8.8.8'

function compare-images() {
    compare -verbose -metric AE $1 $2 null
}

if [ -f ~/.git-completion.bash ]; then
  . ~/.git-completion.bash
fi

export GHC_DOT_APP="/Users/wikia/Documents/DUMMY/ghc-7.10.2.app"
if [ -d "$GHC_DOT_APP" ]; then
  export PATH="${HOME}/.local/bin:${HOME}/.cabal/bin:${GHC_DOT_APP}/Contents/bin:${PATH}"
fi
