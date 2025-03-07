#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

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
export PATH="/usr/local/opt/openjdk/bin:$PATH"



export FrameworkPathOverride=/lib/mono/

export GOPATH=$HOME/go
export PATH=$PATH:$GOROOT/bin:$GOPATH/bin

export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export GTK_USE_PORTAL=gedit
export _JAVA_AWT_WM_NONREPARENTING=1

GPG_TTY=$(tty)
export GPG_TTY


source $(brew --prefix nvm)/nvm.sh

# Check if ssh-agent is already running
if [ -z "$SSH_AUTH_SOCK" ]; then
    # Check for existing ssh-agent process
    if ! pgrep -u "$USER" ssh-agent > /dev/null; then
        ssh-agent -s > "$HOME/.ssh/agent.env"
    fi
    if [ -f "$HOME/.ssh/agent.env" ]; then
        source "$HOME/.ssh/agent.env" > /dev/null
    fi
fi

