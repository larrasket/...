#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

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




# TODO remove this later

export MISE_SHELL=bash
export __MISE_ORIG_PATH="$PATH"

mise() {
  local command
  command="${1:-}"
  if [ "$#" = 0 ]; then
    command /usr/local/bin/mise
    return
  fi
  shift

  case "$command" in
  deactivate|shell|sh)
    # if argv doesn't contains -h,--help
    if [[ ! " $@ " =~ " --help " ]] && [[ ! " $@ " =~ " -h " ]]; then
      eval "$(command /usr/local/bin/mise "$command" "$@")"
      return $?
    fi
    ;;
  esac
  command /usr/local/bin/mise "$command" "$@"
}

_mise_hook() {
  local previous_exit_status=$?;
  eval "$(mise hook-env -s bash)";
  return $previous_exit_status;
};
if [[ ";${PROMPT_COMMAND:-};" != *";_mise_hook;"* ]]; then
  PROMPT_COMMAND="_mise_hook${PROMPT_COMMAND:+;$PROMPT_COMMAND}"
fi
# shellcheck shell=bash
export -a chpwd_functions
function __zsh_like_cd()
{
  \typeset __zsh_like_cd_hook
  if
    builtin "$@"
  then
    for __zsh_like_cd_hook in chpwd "${chpwd_functions[@]}"
    do
      if \typeset -f "$__zsh_like_cd_hook" >/dev/null 2>&1
      then "$__zsh_like_cd_hook" || break # finish on first failed hook
      fi
    done
    true
  else
    return $?
  fi
}

# shellcheck shell=bash
[[ -n "${ZSH_VERSION:-}" ]] ||
{
  function cd()    { __zsh_like_cd cd    "$@" ; }
  function popd()  { __zsh_like_cd popd  "$@" ; }
  function pushd() { __zsh_like_cd pushd "$@" ; }
}

chpwd_functions+=(_mise_hook)
_mise_hook
if [ -z "${_mise_cmd_not_found:-}" ]; then
    _mise_cmd_not_found=1
    if [ -n "$(declare -f command_not_found_handle)" ]; then
        _mise_cmd_not_found_handle=$(declare -f command_not_found_handle)
        eval "${_mise_cmd_not_found_handle/command_not_found_handle/_command_not_found_handle}"
    fi

    command_not_found_handle() {
        if [[ "$1" != "mise" && "$1" != "mise-"* ]] && /usr/local/bin/mise hook-not-found -s bash -- "$1"; then
          _mise_hook
          "$@"
        elif [ -n "$(declare -f _command_not_found_handle)" ]; then
            _command_not_found_handle "$@"
        else
            echo "bash: command not found: $1" >&2
            return 127
        fi
    }
fi


export ANDROID_HOME=$HOME/Library/Android/sdk
export PATH=$PATH:$ANDROID_HOME/emulator
export PATH=$PATH:$ANDROID_HOME/platform-tools
export PATH=$PATH:$ANDROID_HOME/tools
export PATH=$PATH:$ANDROID_HOME/tools/bin
