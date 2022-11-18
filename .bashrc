
#
# ~/.bashrc
#
[[ $- != *i* ]] && return
export PATH=$PATH:/home/ghd/.cargo/bin
export PATH=$PATH:/home/ghd/.dotnet/tools
export PATH=$PATH:/home/ghd/.local/bin
export FrameworkPathOverride=/lib/mono/


export GOPATH=$HOME/go
export PATH=$PATH:$GOROOT/bin:$GOPATH/bin

[ -n "$DISPLAY" ]  && command -v xdo >/dev/null 2>&1 && xdo id > /tmp/term-wid-"$$"
trap "( rm -f /tmp/term-wid-"$$" )" EXIT HUP

# blog
alias pubb1='cd public && git rm -rf . && cd .. && ./build.sh && cd public && git add . &&  git commit -m \"update\" && git push origin master '
alias pub1='git add . && git commit -m update && git push origin latest_branch'
alias pub='pub1 && pubb1'

#programs
alias mic='pavucontrol'
alias wife='nmtui'
alias hd='nmcli con up'
alias s='setsid rsblocks '
alias mvi='mpv --config-dir=$HOME/.config/mvi'
alias mva='mpv --config-dir=$HOME/.config/mvi *jpg *png *gif *webp *tiff *raw *bmp *svg'
alias htop='gotop'
alias netwatch='sudo nethogs'
alias sdocker='systemctl start docker.service'
alias docker='sudo docker'

#make it easier
alias c='clear'
alias doom='~/.emacs.d/bin/doom'
alias sv='sudo vim'
alias smk='sudo make clean install'
alias update='sudo pacman -Syu'
alias i='sudo pacman -S '
alias r='sudo pacman -Rs'
alias lsd='du -h --max-depth=1 | sort -hr'
alias grep='grep --colour=auto'
alias f="lfrun"
alias cp="cp -i"                          # confirm before overwriting something
alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB
alias d='yt-dlp -o '\''%(title)s.%(ext)s'\'' '
alias ds='yt-dlp -f 136 -o  '\''%(title)s.%(ext)s'\'' '
alias dss='yt-dlp -f '\''bestvideo[height<=480]+bestaudio/best[height<=480]'\''  --write-auto-sub -o  '\''%(title)s.%(ext)s'\'' '
alias mp3='yt-dlp -o '\''%(title)s.%(ext)s'\'' --extract-audio --audio-format mp3 --add-metadata'
unset use_color safe_term match_lhs sh
alias po='castero'
export _JAVA_AWT_WM_NONREPARENTING=1
alias ls='ls -l'
alias ll='ls -l'
alias sz='du -sh'
alias k='killall'
alias shut='shutdown -P '
alias psql='sudo -iu postgres'

# send file over network
alias send='tar -cz . | nc -q 10 -l -p 9090'
alias rec='nc -w 10 192.168.1.6 9090 | tar -xz'


alias cat='bat'
alias intl="WD=\$(pwd);cd ~/source;zathura \"\$(fzf)\";cd \$WD"
alias share="caddy file-server --listen :2030 --browse"
alias films="sudo cryptsetup luksOpen /dev/sda3 winsys && sudo mount /dev/mapper/winsys /mnt/winsys/"
alias cmus="cd ~/music && cmus"
alias frozen="pkill -SIGUSR2 emacs"
alias 'cd..'='cd ..'
alias sp='systemctl suspend'
alias vim='emacsclient -nw'



xhost +local:root > /dev/null 2>&1

complete -cf sudo
# Bash won't get SIGWINCH if another process is in the foreground.
# Enable checkwinsize so that bash will check the terminal size when
# it regains control.  #65623
# http://cnswww.cns.cwru.edu/~chet/bash/FAQ (E11)
shopt -s checkwinsize
shopt -s expand_aliases
shopt -s histappend

export EDITOR="/usr/bin/nvim"
force_color_prompt=yes



_show_git_status() {
  # Get the current git branch and colorize to indicate branch state
  # branch_name+ indicates there are stash(es)
  # branch_name? indicates there are untracked files
  # branch_name! indicates your branches have diverged
  local unknown untracked stash clean ahead behind staged dirty diverged
  unknown='0;34'      # blue
  untracked='0;32'    # green
  stash='0;32'        # green
  clean='0;32'        # green
  ahead='0;33'        # yellow
  behind='0;33'       # yellow
  staged='0;96'       # cyan
  dirty='0;31'        # red
  diverged='0;31'     # red

  if [[ $TERM = *256color ]]; then
    unknown='38;5;20'     # dark blue
    untracked='38;5;76'   # mid lime-green
    stash='38;5;76'       # mid lime-green
    clean='38;5;82'       # brighter green
    ahead='38;5;226'      # bright yellow
    behind='38;5;142'     # darker yellow-orange
    staged='38;5;214'     # orangey yellow
    dirty='38;5;202'      # orange
    diverged='38;5;196'   # red
  fi

  branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)
  if [[ -n "$branch" ]]; then
    git_status=$(git status 2> /dev/null)
    # If nothing changes the color, we can spot unhandled cases.
    color=$unknown
    if [[ $git_status =~ 'Untracked files' ]]; then
      color=$untracked
      branch="${branch}?"
    fi
    if git stash show &>/dev/null; then
      color=$stash
      branch="${branch}+"
    fi
    if [[ $git_status =~ 'working directory clean' ]]; then
      color=$clean
    fi
    if [[ $git_status =~ 'Your branch is ahead' ]]; then
      color=$ahead
      branch="${branch}>"
    fi
    if [[ $git_status =~ 'Your branch is behind' ]]; then
      color=$behind
      branch="${branch}<"
    fi
    if [[ $git_status =~ 'Changes to be committed' ]]; then
      color=$staged
    fi
    if [[ $git_status =~ 'Changed but not updated' ||
          $git_status =~ 'Changes not staged'      ||
          $git_status =~ 'Unmerged paths' ]]; then
      color=$dirty
    fi
    if [[ $git_status =~ 'Your branch'.+diverged ]]; then
      color=$diverged
      branch="${branch}!"
    fi
    echo -n "\[\033[${color}m\]${branch}\[\033[0m\]"
  fi
  return 0
}

_show_last_exit_status() {
  # Display the exit status of the last run command
  exit_status=$?
  if [[ "$exit_status" -ne 0 ]]; then
    echo "Exit $exit_status"
  fi
}

_build_prompt() {
  local git_status prompt_dir
  git_status=$(_show_git_status)
  if [[ -n "$git_status" ]]; then
    git_status=":${git_status}"
  fi
  prompt_dir=$(basename "${PWD}")
  # Set xterm title
  echo -ne "\033]0;${HOSTNAME}\007"
  # Check to see if inside screen
  if [[ -n "$STY" ]]; then
    # Set xterm title, from within screen
    echo -ne "\033_${HOSTNAME}\033\0134"
    # Set screen window name
    echo -ne "\033k\033\0134"
  fi
  PS1="\h [${prompt_dir}${git_status}]\\\$ "
  return 0
}

PROMPT_COMMAND="_show_last_exit_status; _build_prompt;"
if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
  exec tmux
fi


