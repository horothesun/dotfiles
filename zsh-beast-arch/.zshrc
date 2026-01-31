# Path to your Oh My Zsh installation.
ZSH="/usr/share/oh-my-zsh/"

ZSH_THEME="robbyrussell"

plugins=(
  git
  web-search
)

ZSH_CACHE_DIR="${HOME}/.cache/oh-my-zsh"
if [[ ! -d $ZSH_CACHE_DIR ]]; then
  mkdir $ZSH_CACHE_DIR
fi

source "${ZSH}/oh-my-zsh.sh"


setopt APPEND_HISTORY
setopt SHARE_HISTORY
#setopt NOTIFY
#setopt NOHUP
#setopt MAILWARN

HISTFILE="${HOME}/.zsh_history"
HISTSIZE=5000
SAVEHIST=5000

# mill (JVM build tool):
# `[_]` is a pattern used in mill, by default it's a default zsh match pattern.
# Avoid the collision with the following zsh config.
unsetopt NOMATCH


# used by gh
export BROWSER="brave"


function clean_java_instances() {
  ps auxww | \grep "/bin/java" | \grep -v "grep" |\
    jq --raw-input 'split(" ") | map(select(. != ""))[1]' |\
    xargs --no-run-if-empty --max-args 1 kill --signal 9
}


alias cl="clear"
alias l="ls -lah --color=auto"
alias ls="ls --color=auto"
alias grep="grep --color=auto"
alias bat="bat --style=plain"
alias vi="nvim"

alias gdh="gdiff HEAD"

function gdiff() {
  GDIFF_PREVIEW="git diff $@ --color=always -- {-1}"
  git diff $@ --name-only | fzf -m --ansi --preview-window 'top,85%,wrap' --preview "${GDIFF_PREVIEW}"
}

#alias xcopy="xclip -rmlastnl -selection clipboard"
#alias xpaste="xsel --clipboard"

alias rg="rg --hidden --glob !**/.git/**"


# preferred editor for local and remote sessions
if [[ -n "${SSH_CONNECTION}" ]]; then
  export VISUAL="vi"
else
  export VISUAL="nvim"
fi
export EDITOR="${VISUAL}"


function show_all_branches() {
  ls -d */ | xargs -I ^ /bin/bash -c 'cd ^; echo "$(git branch --show-current) -> ^"'
}

function pull_all_repos() {
  time ( \
    ls -d */ | xargs -P 0 -I ^ \
      /bin/bash -c 'echo "⏳ Processing ^..." && cd ^ && git status && git fetch --all --prune --jobs=10 && git pull && echo' \
  )
}


# GitHub SSH key
eval "$(ssh-agent)" > /dev/null
SSH_ASKPASS_REQUIRE="force" SSH_ASKPASS="${HOME}/.ssh/askpass.sh" ssh-add "${HOME}/.ssh/horothesun" &> /dev/null


# starship
eval "$(starship init zsh)"


# fzf
source <(fzf --zsh)


PATH=$PATH:$HOME/bin

PATH="$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH"


# get external monitor brightness/contrast
function get_monitor_stats() {
  ddcutil getvcp 10 12 |\
    jq --raw-input --raw-output '
        split(" ")
      | map(select(. != ""))
      | {
          "\(.[3] | split("(")[1] | ascii_downcase)": {
            "value": .[8] | split(",")[0] | tonumber,
            "max": .[12] | tonumber
            }
        }' |\
    jq --slurp 'add'
}


# tldr's apt version's very old and --update doesn't work
function update_tldr() {
  echo "update_tldr BEGIN"
  tldr --update
  echo "update_tldr END"
}
