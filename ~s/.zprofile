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
export PATH=$PATH:$HOME/.config/emacs/bin
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
