eval "$(/opt/homebrew/bin/brew shellenv)"

export PATH="/usr/local/MacGPG2/bin:$PATH"
export PATH="/usr/local/opt/openjdk/bin:$PATH"
export PATH="$HOME/Library/Application Support/Coursier/bin:$PATH"

export GOPATH="$HOME/go"
export PATH="$PATH:$GOROOT/bin:$GOPATH/bin"
export PATH="$PATH:$HOME/.emacs.d/bin"
export PATH="$PATH:/Library/TeX/texbin"
export PATH="$PATH:$HOME/.doom.d/bin"
export PATH="$PATH:$HOME/.bin" export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:$HOME/.cargo/bin"
export PATH="$PATH:$HOME/.dotnet/tools"
export PATH="$PATH:$HOME/Library/Python/3.9/bin"

export ANDROID_HOME="$HOME/Library/Android/sdk"
export PATH="$PATH:$ANDROID_HOME/emulator:$ANDROID_HOME/platform-tools:$ANDROID_HOME/tools:$ANDROID_HOME/tools/bin"

export LANG=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export LC_COLLATE=en_US.UTF-8

export FrameworkPathOverride=/lib/mono/
export GTK_USE_PORTAL=gedit
export _JAVA_AWT_WM_NONREPARENTING=1
export GPG_TTY="$(tty)"

export NVM_DIR="$HOME/.nvm"
[ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh" --no-use

if [ -z "$SSH_AUTH_SOCK" ]; then
    if ! pgrep -u "$USER" ssh-agent >/dev/null; then
        ssh-agent -s >"$HOME/.ssh/agent.env"
    fi
    [ -f "$HOME/.ssh/agent.env" ] && source "$HOME/.ssh/agent.env" >/dev/null
fi


compressvideo() {
    if [[ $# -ne 1 ]]; then
        echo "Usage: compressvideo <video>"
        return 1
    fi

    input="$1"
    output="${input%.*}-compressed.mp4"

    ffmpeg -i "$input" \
        -c:v libx264 \
        -profile:v high \
        -level 4.0 \
        -pix_fmt yuv420p \
        -crf 36 \
        -preset slow \
        -vf "scale='min(1280,iw)':-2" \
        -movflags +faststart \
        -c:a aac \
        -b:a 128k \
        "$output"

    echo "Saved to: $output"
}



if [[ "$INSIDE_EMACS" = 'ghostel' ]]; then
    # Open a file in Emacs from the terminal
    e()   { ghostel_cmd find-file-other-window "$@"; }

    # Open dired in another window
    dow() { ghostel_cmd dired-other-window "$@"; }

    # Open magit for the current directory
    gst() { ghostel_cmd magit-status-setup-buffer "$(pwd)"; }
fi
