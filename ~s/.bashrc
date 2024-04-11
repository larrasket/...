# ~/.bashrc
#
[[ $- != *i* ]] && return

[ -n "$DISPLAY" ] && command -v xdo >/dev/null 2>&1 && xdo id >/tmp/term-wid-"$$"
trap "( rm -f /tmp/term-wid-"$$" )" EXIT HUP

unset use_color safe_term match_lhs sh
force_color_prompt=yes
_show_git_status() {
	# Get the current git branch and colorize to indicate branch state
	# branch_name+ indicates there are stash(es)
	# branch_name? indicates there are untracked files
	# branch_name! indicates your branches have diverged
	local unknown untracked stash clean ahead behind staged dirty diverged
	unknown='0;34'   # blue
	untracked='0;32' # green
	stash='0;32'     # green
	clean='0;32'     # green
	ahead='0;33'     # yellow
	behind='0;33'    # yellow
	staged='0;96'    # cyan
	dirty='0;31'     # red
	diverged='0;31'  # red

	if [[ $TERM = *256color ]]; then
		unknown='38;5;20'   # dark blue
		untracked='38;5;76' # mid lime-green
		stash='38;5;76'     # mid lime-green
		clean='38;5;82'     # brighter green
		ahead='38;5;226'    # bright yellow
		behind='38;5;142'   # darker yellow-orange
		staged='38;5;214'   # orangey yellow
		dirty='38;5;202'    # orange
		diverged='38;5;196' # red
	fi

	branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)
	if [[ -n "$branch" ]]; then
		git_status=$(git status 2>/dev/null)
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
			$git_status =~ 'Changes not staged' ||
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
	if [[ "$PWD" == "$HOME"* ]]; then
		prompt_dir="~${PWD#$HOME}"
	else
		prompt_dir="${PWD}"
	fi

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

leinn() {
	lein new simple-layout "$1" -- 1.11.0
}

# Conv
alias sudo='sudo ' # Fix sudo in alias
alias wife='nmtui'
alias gotmp='git clone https://github.com/tarqeem/template . && ./init.sh '
alias goent="go run -mod=mod entgo.io/ent/cmd/ent"
alias hd='nmcli con up'
alias s='setsid rsblocks '
alias htop='gotop'
alias netwatch='sudo nethogs'
alias grep='grep --colour=auto'
alias smk='sudo make clean install'
alias f="lfrun"
alias d='yt-dlp -f "mp4" -o "%(title)s.%(ext)s" '
alias c='clear'
alias w4='function _convertwebm(){ ffmpeg  -fflags +genpts -i "$1" -r 24 -vf "pad=ceil(iw/2)*2:ceil(ih/2)*2" "${1%.webm}.mp4" && rm "$1"; };_convertwebm'
alias mp3='yt-dlp -o '\''%(title)s.%(ext)s'\'' --extract-audio --audio-format mp3 --add-metadata'
alias cp="cp -i"
alias i='sudo pacman -S '
alias r='sudo pacman -Rs'
alias lsd='du -h --max-depth=1 | sort -hr'
alias ll='ls -l'
alias sz='du -sh'
alias cat='bat'
alias share="caddy file-server --listen :2030 --browse"
alias frozen="pkill -SIGUSR2 emacs"
alias 'cd..'='cd ..'
alias sp='systemctl suspend'
alias dss='yt-dlp -S "res:480"'
alias djvu2pdf='docker run --rm -u $(id -u):$(id -g) -v $(pwd):/opt/work ilyabystrov/djvu2pdf'
alias tadwin='EMACS=/home/l/.emacs.db/ && /home/l/blog/tadwin.el'
alias pub='cd ~/blog && tadwin && cd public && flyctl deploy'
alias cljp='function _cljp(){ lein new simple-layout "$1" -- 1.11.0};_cljp'
alias docker='sudo docker'

# alias po='castero'
# not deleting this line for nostalgia. :). I wrote it in my
# first year of using linux. I was trying to get a cli podcast aggregator. Those
# ones too:
# alias k='killall'
# alias shut='shutdown -P '
# alias psql='sudo -iu postgres'

# I also rembmer those very well. I was learning differences between BSD's nc
# vs. GNU's. I recoreded a video on that.
# alias send='tar -cz . | nc -q 10 -l -p 9090'
# alias rec='nc -w 10 192.168.1.6 9090 | tar -xz'

# I wrote this before using Emacs. I was using zathura with fzf
# alias intl="WD=\$(pwd);cd ~/source;zathura \"\$(fzf)\";cd \$WD"

complete -cf sudo
# Bash won't get SIGWINCH if another process is in the foreground.
# Enable checkwinsize so that bash will check the terminal size when
# it regains control.  #65623
# http://cnswww.cns.cwru.edu/~chet/bash/FAQ (E11)
shopt -s checkwinsize
shopt -s expand_aliases
shopt -s histappend
PROMPT_COMMAND="_show_last_exit_status; _build_prompt;"
if command -v tmux &>/dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
	exec tmux
fi

# BEGIN_KITTY_SHELL_INTEGRATION
if test -n "$KITTY_INSTALLATION_DIR" -a -e "$KITTY_INSTALLATION_DIR/shell-integration/bash/kitty.bash"; then source "$KITTY_INSTALLATION_DIR/shell-integration/bash/kitty.bash"; fi
# END_KITTY_SHELL_INTEGRATION
export TERM=xterm-256color

shopt -s autocd

source /home/l/.config/broot/launcher/bash/br
