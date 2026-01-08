
eval "$(/opt/homebrew/bin/brew shellenv)"

#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

export PATH="/usr/local/MacGPG2/bin:$PATH"
export PATH="/opt/homebrew/bin:$PATH"
export GOPATH=$HOME/go
export PATH=$PATH:$GOROOT/bin:$GOPATH/bin
export FZF_DEFAULT_COMMAND='fd --type f --strip-cwd-prefix --hidden --follow --exclude *.srt'
export PATH=$PATH:$HOME/.emacs.d/bin
export PATH=$PATH:/Library/TeX/texbin/
export PATH=$PATH:$HOME/.doom.d/bin
export PATH=$PATH:$HOME/.bin/
export PATH=$PATH:$HOME/.local/bin/
export PATH=$PATH:$HOME/.cargo/bin
export PATH=$PATH:$HOME/.dotnet/tools
export PATH=$PATH:$HOME/.local/bin
export PATH=$PATH:$HOME/Library/Python/3.9/bin
export PATH="/usr/local/opt/openjdk/bin:$PATH"

export FrameworkPathOverride=/lib/mono/

export GOPATH=$HOME/go
export PATH=$PATH:$GOROOT/bin:$GOPATH/bin

export LANG=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export LC_COLLATE=en_US.UTF-8
export GTK_USE_PORTAL=gedit
export _JAVA_AWT_WM_NONREPARENTING=1

GPG_TTY=$(tty)
export GPG_TTY

# Lazy load nvm for faster startup (hardcoded path instead of slow brew --prefix)
export NVM_DIR="$HOME/.nvm"
[ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh" --no-use

# Check if ssh-agent is already running
if [ -z "$SSH_AUTH_SOCK" ]; then
    # Check for existing ssh-agent process
    if ! pgrep -u "$USER" ssh-agent >/dev/null; then
        ssh-agent -s >"$HOME/.ssh/agent.env"
    fi
    if [ -f "$HOME/.ssh/agent.env" ]; then
        source "$HOME/.ssh/agent.env" >/dev/null
    fi
fi

# shellcheck shell=bash
export -a chpwd_functions
function __zsh_like_cd() {
    \typeset __zsh_like_cd_hook
    if builtin "$@"; then
        for __zsh_like_cd_hook in chpwd "${chpwd_functions[@]}"; do
            if \typeset -f "$__zsh_like_cd_hook" >/dev/null 2>&1; then
                "$__zsh_like_cd_hook" || break # finish on first failed hook
            fi
        done
        true
    else
        return $?
    fi
}

# shellcheck shell=bash
[[ -n "${ZSH_VERSION:-}" ]] || {
    function cd() { __zsh_like_cd cd "$@"; }
    function popd() { __zsh_like_cd popd "$@"; }
    function pushd() { __zsh_like_cd pushd "$@"; }
}

export ANDROID_HOME=$HOME/Library/Android/sdk
export PATH=$PATH:$ANDROID_HOME/emulator
export PATH=$PATH:$ANDROID_HOME/platform-tools
export PATH=$PATH:$ANDROID_HOME/tools
export PATH=$PATH:$ANDROID_HOME/tools/bin

export JAVA_HOME=$(/usr/libexec/java_home -v17)
export PATH="$JAVA_HOME/bin:$PATH"

function kdo() {
    ps ax | grep -i docker | egrep -iv 'grep|com.docker.vmnetd' | awk '{print $1}' | xargs kill
}

# >>> coursier install directory >>>
export PATH="$PATH:/Users/l/Library/Application Support/Coursier/bin"
# <<< coursier install directory <<<
