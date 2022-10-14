
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



colors() {
	local fgc bgc vals seq0

printf "Color escapes are %s\n" '\e[${value};...;${value}m'
	printf "Values 30..37 are \e[33mforeground colors\e[m\n"
	printf "Values 40..47 are \e[43mbackground colors\e[m\n"
	printf "Value  1 gives a  \e[1mbold-faced look\e[m\n\n"

	# foreground colors
	for fgc in {30..37}; do
		# background colors
		for bgc in {40..47}; do
			fgc=${fgc#37} # white
			bgc=${bgc#40} # black

			vals="${fgc:+$fgc;}${bgc}"
			vals=${vals%%;}

			seq0="${vals:+\e[${vals}m}"
			printf "  %-9s" "${seq0:-(default)}"
			printf " ${seq0}TEXT\e[m"
			printf " \e[${vals:+${vals+$vals;}}1mBOLD\e[m"
		done
		echo; echo
	done
}
[ -r /usr/share/bash-completion/bash_completion ] && . /usr/share/bash-completion/bash_completion

# Change the window title of X terminals
case ${TERM} in
	xterm*|rxvt*|Eterm*|aterm|kterm|gnome*|interix|konsole*)
		PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/\~}\007"'
		;;
	screen*)
		PROMPT_COMMAND='echo -ne "\033_${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/\~}\033\\"'
		;;
esac

use_color=true

# Set colorful PS1 only on colorful terminals.
# dircolors --print-database uses its own built-in database
# instead of using /etc/DIR_COLORS.  Try to use the external file
# first to take advantage of user additions.  Use internal bash
# globbing instead of external grep binary.
safe_term=${TERM//[^[:alnum:]]/?}   # sanitize TERM
match_lhs=""
[[ -f ~/.dir_colors   ]] && match_lhs="${match_lhs}$(<~/.dir_colors)"
[[ -f /etc/DIR_COLORS ]] && match_lhs="${match_lhs}$(</etc/DIR_COLORS)"
[[ -z ${match_lhs}    ]] \
	&& type -P dircolors >/dev/null \
	&& match_lhs=$(dircolors --print-database)
[[ $'\n'${match_lhs} == *$'\n'"TERM "${safe_term}* ]] && use_color=true

if ${use_color} ; then
	# Enable colors for ls, etc.  Prefer ~/.dir_colors #64489
	if type -P dircolors >/dev/null ; then
		if [[ -f ~/.dir_colors ]] ; then
			eval $(dircolors -b ~/.dir_colors)
		elif [[ -f /etc/DIR_COLORS ]] ; then
			eval $(dircolors -b /etc/DIR_COLORS)
		fi
	fi

	if [[ ${EUID} == 0 ]] ; then
		PS1='\[\033[01;31m\][\h\[\033[01;36m\] \W\[\033[01;31m\]]\$\[\033[00m\] '
	else
		PS1='\[\033[01;32m\][\u@\h\[\033[01;37m\] \W\[\033[01;32m\]]\$\[\033[00m\] '
	fi










alias "bt-l"='bt-device -l'
alias mvs="mpv --config-dir=~/.config/mvs/"
alias "bt-c"='bt-device -c'
alias gitt='cd aur && git clone'
alias post='/home/ghd/me/temp/page/app/GetFilesForPosting'
alias camera="mplayer -tv device=/dev/video1 tv://"
alias lib='libgen -s'
alias pubb1='cd public && git rm -rf . && cd .. && ./build.sh && cd public && git add . &&  git commit -m \"update\" && git push origin master '
alias pub1='git add . && git commit -m update && git push origin latest_branch'
alias pub='pub1 && pubb1'
alias mic='ncpamixer'
alias wife='nmtui'
alias n='cd books/notes'
alias hd='nmcli con up'
alias pro='cd /Documents/prop'
alias s='setsid rsblocks '
alias mynet='sudo netstat -atupen'
alias vi='nvim'
alias c='clear'
alias sv='sudo vim'
alias smk='sudo make clean install'
alias imk='makepkg -si'
alias update='sudo pacman -Syu'
alias i='sudo pacman -S '
alias r='sudo pacman -Rs'
alias host='sv /etc/hosts'
alias ls='ls --color=auto'
alias lsd='du -h --max-depth=1 | sort -hr'
alias doom='~/.emacs.d/bin/doom'
alias grep='grep --colour=auto'
alias egrep='egrep --colour=auto'
alias fgrep='fgrep --colour=auto'
alias pss='keepassxc'
alias f="lfrun"
alias cp="cp -i"                          # confirm before overwriting something
alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB
alias np='nano -w PKGBUILD'
alias more=less
alias d='yt-dlp -o '\''%(title)s.%(ext)s'\'' '
alias todo='v ~/org/todo.org'
alias ds='yt-dlp -f 136 -o  '\''%(title)s.%(ext)s'\'' '
alias dss='yt-dlp -f '\''bestvideo[height<=480]+bestaudio/best[height<=480]'\''  --write-auto-sub -o  '\''%(title)s.%(ext)s'\'' '
alias mp3='yt-dlp -o '\''%(title)s.%(ext)s'\'' --extract-audio --audio-format mp3 --add-metadata'
unset use_color safe_term match_lhs sh
alias mvi='mpv --config-dir=$HOME/.config/mvi'
alias mva='mpv --config-dir=$HOME/.config/mvi *jpg *png *gif *webp *tiff *raw *bmp *svg'
alias po='castero'
export _JAVA_AWT_WM_NONREPARENTING=1
alias run='rm -r ~/source/Politics'
alias cin='vi ~/blog/content/cinema.md'
alias bin='vi ~/blog/content/books.md'
alias you='lf-yt'
alias p='kitty +kitten icat'
alias ls='ls -l'
alias ll='ls -l'
alias vim='nvim'
alias e=emacs
alias v='vi'
alias a='v ~/org/todo.org'
alias sz='du -sh'
alias msd='cd ~/me/music/l/'
alias t='v ~/me/temp/x.cpp'
alias k='killall'
alias htop='gotop'
alias tori='tordl'
alias send='tar -cz . | nc -q 10 -l -p 9090'
alias rec='nc -w 10 192.168.1.6 9090 | tar -xz'
alias shut='shutdown -P '
alias psql='sudo -iu postgres'
alias mincraft='prime-run /usr/bin/java -jar /opt/tlauncher/tlauncher.jar'
alias netwatch='sudo nethogs'
alias cat='highlight -O ansi --force'
alias scriptcs='cscs'
alias z='zathura'
alias clone='git clone'
alias add='git add .'
alias pull='git pull'
alias commit='git commit'
alias intl="WD=\$(pwd);cd ~/source;zathura \"\$(fzf)\";cd \$WD"
alias share="caddy file-server --listen :2030 --browse"
alias films="sudo cryptsetup luksOpen /dev/sda3 winsys && sudo mount /dev/mapper/winsys /mnt/winsys/"




else
	if [[ ${EUID} == 0 ]] ; then
		# show root@ when we don't have colors
		PS1='\u@\h \W \$ '
	else
		PS1='\u@\h \w \$ '
	fi
fi

xhost +local:root > /dev/null 2>&1

complete -cf sudo

# Bash won't get SIGWINCH if another process is in the foreground.
# Enable checkwinsize so that bash will check the terminal size when
# it regains control.  #65623
# http://cnswww.cns.cwru.edu/~chet/bash/FAQ (E11)
shopt -s checkwinsize

shopt -s expand_aliases

# export QT_SELECT=4

# Enable history appending instead of overwriting.  #139609
shopt -s histappend

#
# # ex - archive extractor
# # usage: ex <file>
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1     ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}
openclose() {
    "$@" &
    disown
    exit
}
export PS1="\W > "

#setsid rsblocks
#setsid dunst

#[ -f ~/.fzf.bash ] && source ~/.fzf.bash


#source /home/saleh/.config/broot/launcher/bash/br
export EDITOR="/usr/bin/nvim"
# export EDITOR="emacs --no-window-system"
 # export EDITOR="emacs"
force_color_prompt=yes

if [ -n "$GTK_MODULES" ]; then
    GTK_MODULES="${GTK_MODULES}:appmenu-gtk-module" # unity-gtk-module
    #GTK_MODULES="${GTK_MODULES}:unity-gtk-module" # unity-gtk-module
else
    GTK_MODULES="appmenu-gtk-module"
    #GTK_MODULES="unity-gtk-module"
fi

if [ -z "$UBUNTU_MENUPROXY" ]; then
    UBUNTU_MENUPROXY=1
fi
# Use bash-completion, if available
[[ $PS1 && -f /usr/share/bash-completion/bash_completion ]] && \
    . /usr/share/bash-completion/bash_completion


#source /home/ghd/.config/broot/launcher/bash/br
alias 'cd..'='cd ..'
alias sp='systemctl suspend'


# BEGIN_KITTY_SHELL_INTEGRATION
if test -n "$KITTY_INSTALLATION_DIR" -a -e "$KITTY_INSTALLATION_DIR/shell-integration/bash/kitty.bash"; then source "$KITTY_INSTALLATION_DIR/shell-integration/bash/kitty.bash"; fi
# END_KITTY_SHELL_INTEGRATION
export TERM=xterm-256color
