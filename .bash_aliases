alias a v ~/org/todo.org $*
alias add git add . $*
alias bin vi ~/blog/content/books.md $*
alias bt-c bt-device -c $*
alias bt-l bt-device -l $*
alias c clear $*
alias camera mplayer -tv device=/dev/video1 tv:// $*
alias cat highlight -O ansi --force $*
alias cd.. cd_up $*
alias cin vi ~/blog/content/cinema.md $*
alias clone git clone $*
alias commit git commit $*
alias cp cp -i $*
alias d yt-dlp -o '%(title)s.%(ext)s'  $*
alias df df -h $*
alias doom ~/.emacs.d/bin/doom $*
alias ds yt-dlp -f 136 -o  '%(title)s.%(ext)s'  $*
alias dss yt-dlp -f 'bestvideo[height<=480]+bestaudio/best[height<=480]'  --write-auto-sub -o  '%(title)s.%(ext)s'  $*
alias e emacs $*
alias egrep egrep --colour=auto $*
alias f lfrun $*
alias fgrep fgrep --colour=auto $*
alias films sudo cryptsetup luksOpen /dev/sda3 Films && sudo mount -t ext4 /dev/sda3 /mnt/Films $*
alias free free -m $*
alias gitt cd aur && git clone $*
alias grep grep --colour=auto $*
alias hd nmcli con up $*
alias host sv /etc/hosts $*
alias htop gotop $*
alias i sudo pacman -S  $*
alias imk makepkg -si $*
alias intl WD=$(pwd);cd ~/source;zathura "$(fzf)";cd $WD $*
alias k killall $*
alias lib libgen -s $*
alias ll ls -l $*
alias ls ls -l $*
alias mic ncpamixer $*
alias mincraft prime-run /usr/bin/java -jar /opt/tlauncher/tlauncher.jar $*
alias more less $*
alias mp3 yt-dlp -o '%(title)s.%(ext)s' --extract-audio --audio-format mp3 --add-metadata $*
alias msd cd ~/me/music/l/ $*
alias mva mpv --config-dir=$HOME/.config/mvi *jpg *png *gif *webp *tiff *raw *bmp *svg $*
alias mvi mpv --config-dir=$HOME/.config/mvi $*
alias mvs mpv --config-dir=~/.config/mvs/ $*
alias mynet sudo netstat -atupen $*
alias n cd books/notes $*
alias netwatch sudo nethogs $*
alias np nano -w PKGBUILD $*
alias p kitty +kitten icat $*
alias po castero $*
alias post /home/ghd/me/temp/page/app/GetFilesForPosting $*
alias pro cd /Documents/prop $*
alias psql sudo -iu postgres $*
alias pss keepassxc $*
alias pub pub1 && pubb1 $*
alias pub1 git add . && git commit -m update && git push origin latest_branch $*
alias pubb1 cd public && git rm -rf . && cd .. && ./build.sh && cd public && git add . &&  git commit -m \"update\" && git push origin master  $*
alias pull git pull $*
alias r sudo pacman -R $*
alias rec nc -w 10 192.168.1.6 9090 | tar -xz $*
alias run rm -r ~/source/Politics $*
alias s setsid rsblocks  $*
alias scriptcs cscs $*
alias send tar -cz . | nc -q 10 -l -p 9090 $*
alias share caddy file-server --listen :2030 --browse $*
alias shut shutdown -P  $*
alias smk sudo make clean install $*
alias sp systemctl suspend $*
alias sv sudo vim $*
alias sz du -sh $*
alias t v ~/me/temp/x.cpp $*
alias todo v ~/org/todo.org $*
alias tori tordl $*
alias update sudo pacman -Syu $*
alias v vi $*
alias vi nvim $*
alias vim nvim $*
alias wife nmtui $*
alias you lf-yt $*
alias z zathura $*
