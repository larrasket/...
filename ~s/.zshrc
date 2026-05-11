# ~/.zshrc — interactive zsh config

HISTFILE="$HOME/.zsh_history"
HISTSIZE=200000
SAVEHIST=200000
setopt EXTENDED_HISTORY          
setopt INC_APPEND_HISTORY        
setopt SHARE_HISTORY             
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE         
setopt HIST_REDUCE_BLANKS
setopt HIST_VERIFY               
setopt HIST_FIND_NO_DUPS

setopt AUTO_CD                   
setopt AUTO_PUSHD                
setopt PUSHD_IGNORE_DUPS
setopt PUSHD_SILENT
setopt CORRECT                  
setopt INTERACTIVE_COMMENTS    
setopt EXTENDED_GLOB          
setopt GLOB_DOTS             
setopt NO_BEEP
setopt PROMPT_SUBST

export CLICOLOR=1
export LSCOLORS='ExGxFxDxCxegedabagacad'
export LS_COLORS='di=1;36:ln=35:so=32:pi=33:ex=1;32:bd=34;46:cd=34;43:su=30;41:sg=30;46:tw=30;42:ow=30;43'
export GREP_COLORS='ms=01;33:mc=01;31:sl=:cx=:fn=35:ln=32:bn=32:se=36'
export LESS='-R -i -M -F -X -j.5'
export LESSOPEN='|/opt/homebrew/bin/bat --color=always --paging=never --style=plain %s 2>/dev/null'
export PAGER='less'
export MANPAGER="sh -c 'col -bx | bat -l man -p --paging=always'"
export MANROFFOPT='-c'

export LESS_TERMCAP_mb=$'\e[1;31m'
export LESS_TERMCAP_md=$'\e[1;36m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[01;33m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1;32m'

fpath=(/opt/homebrew/share/zsh-completions $fpath)
fpath=(/opt/homebrew/share/zsh/site-functions $fpath)

autoload -Uz compinit
compinit -i

zstyle ':completion:*' menu select                          
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'   
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"     
zstyle ':completion:*' group-name ''                        
zstyle ':completion:*:descriptions' format '%F{cyan}%B%d%b%f'
zstyle ':completion:*:warnings' format '%F{red}no matches%f'
zstyle ':completion:*' use-cache true
zstyle ':completion:*' cache-path "$HOME/.cache/zsh"
zstyle ':completion:*:processes' command 'ps -au$USER'
zstyle ':completion:*:default' list-prompt '%S%M matches%s'

bindkey -e                                  

autoload -U up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey '^[[A' up-line-or-beginning-search       
bindkey '^[[B' down-line-or-beginning-search    

bindkey '^[[1;5C' forward-word
bindkey '^[[1;5D' backward-word
bindkey '^[[H'  beginning-of-line
bindkey '^[[F'  end-of-line
bindkey '^[[3~' delete-char

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=8'                       
ZSH_AUTOSUGGEST_STRATEGY=(history completion)
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20                           
source /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh

export FZF_DEFAULT_COMMAND='fd --type f --strip-cwd-prefix --hidden --follow --exclude .git --exclude node_modules --exclude *.srt'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND='fd --type d --strip-cwd-prefix --hidden --follow --exclude .git --exclude node_modules'
export FZF_DEFAULT_OPTS='
  --height=40% --layout=reverse --border --info=inline
  --color=bg+:#313244,bg:#1e1e2e,spinner:#f5e0dc,hl:#f38ba8
  --color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc
  --color=marker:#f5e0dc,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8
  --bind=ctrl-d:half-page-down,ctrl-u:half-page-up'
export FZF_CTRL_T_OPTS="--preview 'bat -n --color=always --line-range :500 {}'"
export FZF_ALT_C_OPTS="--preview 'eza --tree --color=always --icons=never {} | head -200'"

[[ -r /opt/homebrew/opt/fzf/shell/completion.zsh   ]] && source /opt/homebrew/opt/fzf/shell/completion.zsh
[[ -r /opt/homebrew/opt/fzf/shell/key-bindings.zsh ]] && source /opt/homebrew/opt/fzf/shell/key-bindings.zsh

command -v zoxide >/dev/null && eval "$(zoxide init zsh --cmd cd)"
command -v direnv >/dev/null && eval "$(direnv hook zsh)"

if command -v eza >/dev/null; then
    alias ls='eza --group-directories-first --color=auto'
    alias ll='eza -lh --group-directories-first --git --color=auto'
    alias la='eza -lah --group-directories-first --git --color=auto'
    alias lt='eza --tree --level=2 --color=auto'
    alias lT='eza --tree --color=auto'
else
    alias ls='ls -G'
    alias ll='ls -lhG'
    alias la='ls -lahG'
fi

command -v bat >/dev/null && alias cat='bat --paging=never --style=plain'

alias grep='grep --colour=auto'
alias egrep='egrep --colour=auto'
alias fgrep='fgrep --colour=auto'
alias diff='diff --color=auto'
alias df='df -h'
alias du='du -h'
alias path='echo -e ${PATH//:/\\n}'
alias reload='source ~/.zshrc'
alias mkdir='mkdir -pv'
alias ports='lsof -i -P -n | grep LISTEN'
alias myip='curl -s ifconfig.me; echo'

alias sudo='sudo '
alias serve='npx serve'

alias d='yt-dlp -f "mp4" -o "%(title)s.%(ext)s" '
alias w4='function _convertwebm(){ ffmpeg -fflags +genpts -i "$1" -r 24 -vf "pad=ceil(iw/2)*2:ceil(ih/2)*2" "${1%.webm}.mp4" && rm "$1"; };_convertwebm'
alias mp3='yt-dlp -o '\''%(title)s.%(ext)s'\'' --extract-audio --audio-format mp3 --add-metadata'
alias convertmp4tomp3='function _convertmp4tomp3() { ffmpeg -i "$1" "${1%.*}.mp3"; }; _convertmp4tomp3'
alias dss='yt-dlp -S "res:480"'
alias djvu2pdf='docker run --rm -u $(id -u):$(id -g) -v $(pwd):/opt/work ilyabystrov/djvu2pdf'

alias i='sudo pacman -S '
alias r='sudo pacman -Rs'

alias f="lfrun"
alias htop='gotop'
alias netwatch='sudo nethogs'
alias c='clear'
alias cp="cp -i"
alias dirsize='du -h -d 1 | sort -hr'
alias sz='du -sh'
alias share="caddy file-server --listen :2030 --browse"
alias frozen="pkill -SIGUSR2 emacs"
alias 'cd..'='cd ..'


mkcd() { mkdir -p "$1" && cd "$1"; }

extract() {
    [[ -f "$1" ]] || { echo "extract: '$1' is not a file"; return 1; }
    case "$1" in
        *.tar.bz2|*.tbz2) tar xjf "$1" ;;
        *.tar.gz|*.tgz)   tar xzf "$1" ;;
        *.tar.xz|*.txz)   tar xJf "$1" ;;
        *.tar)            tar xf  "$1" ;;
        *.bz2)            bunzip2 "$1" ;;
        *.gz)             gunzip  "$1" ;;
        *.zip)            unzip   "$1" ;;
        *.7z)             7z x    "$1" ;;
        *.rar)            unrar x "$1" ;;
        *.Z)              uncompress "$1" ;;
        *) echo "extract: don't know how to extract '$1'"; return 1 ;;
    esac
}

rgf() {
    local file
    file=$(rg --color=always --line-number --no-heading --smart-case "$@" |
        fzf --ansi --delimiter : \
            --preview 'bat --color=always --highlight-line {2} {1}' \
            --preview-window 'up,60%,border-bottom,+{2}+3/3' |
        awk -F: '{print $1 ":" $2}')
    [[ -n "$file" ]] && ${EDITOR:-vi} "${file%:*}" "+${file##*:}"
}

kdo() {
    ps ax | grep -i docker | egrep -iv 'grep|com.docker.vmnetd' | awk '{print $1}' | xargs kill
}

if test -n "$KITTY_INSTALLATION_DIR" -a -e "$KITTY_INSTALLATION_DIR/shell-integration/zsh/kitty.zsh"; then
    source "$KITTY_INSTALLATION_DIR/shell-integration/zsh/kitty.zsh"
fi



#halan

function kdo() {
    ps ax | grep -i docker | egrep -iv 'grep|com.docker.vmnetd' | awk '{print $1}' | xargs kill
}

alias argodev='kubectl port-forward svc/argocd-server -n argocd 8080:80'
alias argopass='kubectl -n argocd get secret argocd-initial-admin-secret -o jsonpath="{.data.password}" | base64 -d'

alias fwdargocd="kubectl -n argocd get secret argocd-initial-admin-secret -o jsonpath="{.data.password}" | base64 -d && kubectl port-forward svc/argocd-server -n argocd 8080:80"

alias argocdpassword="kubectl -n argocd get secret argocd-initial-admin-secret -o jsonpath="{.data.password}" | base64 -d"

alias stgcluster="kubectl config use-context halangw-stg-new"
alias devcluster="kubectl config use-context arn:aws:eks:us-east-1:624792314775:cluster/halan-gateway-dev"

alias vpn2="sudo openfortivpn 102.221.142.122:10443 --username=ahmed.shaheen@halan.com --pinentry=pinentry-mac --trusted-cert 1c684e7f00f892fb95c6f5429dc43a0668950b1bd59a02c9fabf9c6cdd7ffa6c"

alias vpn="sudo openfortivpn 102.221.142.122:10443 --username=ahmed.shaheen@halan.com --trusted-cert 1c684e7f00f892fb95c6f5429dc43a0668950b1bd59a02c9fabf9c6cdd7ffa6c"

klogs() {
    svc=$(kubectl get svc -A | fzf)
    ns=$(echo $svc | awk '{print $1}')
    name=$(echo $svc | awk '{print $2}')
    kubectl logs -f svc/$name -n$ns --since=55m
}

ksecret() {
    local ns=${1:-default}
    local secret=$(kubectl get secret -n $ns | fzf | awk '{print $1}')
    local key=$(kubectl get secret $secret -n $ns -o jsonpath='{.data}' | jq -r 'keys[]' | fzf)
    kubectl get secret $secret -n $ns -o jsonpath="{.data.$key}" | base64 --decode
    echo
}

kpf() {
    local ns=${1:-default}

    local svc=$(kubectl get svc -n $ns | fzf | awk '{print $1}')
    if [ -z "$svc" ]; then
        echo "No service selected."
        return 1
    fi

    echo -n "Local port: "
    read lport
    echo -n "Remote port: "
    read rport

    echo "Forwarding $svc:$rport to localhost:$lport (namespace: $ns)"
    kubectl port-forward svc/$svc $lport:$rport -n $ns
}

kexec() {
    local ns=${1:-default}

    # Pick a pod interactively
    local pod=$(kubectl get pods -n $ns | fzf | awk '{print $1}')
    if [ -z "$pod" ]; then
        echo "No pod selected."
        return 1
    fi

    # Pick a container if multiple exist
    local containers=$(kubectl get pod $pod -n $ns -o jsonpath='{.spec.containers[*].name}')
    local container
    if [[ $(echo $containers | wc -w) -gt 1 ]]; then
        container=$(echo $containers | tr ' ' '\n' | fzf)
    else
        container=$containers
    fi

    # Choose mode: shell or redis-cli
    echo "Choose mode:"
    select mode in "Shell" "Redis-CLI"; do
        case $mode in
        "Shell")
            echo "Exec into pod: $pod (container: $container, namespace: $ns)"
            kubectl exec -it $pod -n $ns -c $container -- /bin/sh || kubectl exec -it $pod -n $ns -c $container -- /bin/bash
            break
            ;;
        "Redis-CLI")
            echo "Exec into pod: $pod (container: $container, namespace: $ns) with redis-cli"
            kubectl exec -it $pod -n $ns -c $container -- redis-cli
            break
            ;;
        *)
            echo "Invalid option."
            ;;
        esac
    done
}

kdesc() {
    local ns=${1:-default}

    # Pick a pod interactively
    local pod=$(kubectl get pods -n $ns | fzf | awk '{print $1}')
    if [ -z "$pod" ]; then
        echo "No pod selected."
        return 1
    fi

    echo "Describing pod: $pod (namespace: $ns)"
    kubectl describe pod $pod -n $ns | less
}

kedit() {
    local ns=$1
    if [ -z "$ns" ]; then
        echo "Usage: kedit <namespace>"
        return 1
    fi

    local deployment=$(kubectl get deployments -n "$ns" --no-headers -o custom-columns=":metadata.name" | fzf)
    if [ -n "$deployment" ]; then
        echo "Editing deployment: $deployment (namespace: $ns)"
        KUBE_EDITOR="code -w" kubectl edit deployment "$deployment" -n "$ns"
    else
        echo "No deployment selected."
    fi
}

#end halan






export TERM=xterm-256color

command -v starship >/dev/null && eval "$(starship init zsh)"



