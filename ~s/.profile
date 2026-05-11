
# >>> coursier install directory >>>
export PATH="$PATH:/Users/l/Library/Application Support/Coursier/bin"
# <<< coursier install directory <<<

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
