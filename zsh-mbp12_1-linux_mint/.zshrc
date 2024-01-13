export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="robbyrussell"

plugins=(
  git
  web-search
)

source "${ZSH}/oh-my-zsh.sh"


PATH="${PATH}:${HOME}/.cargo/bin"


alias rg="rg --hidden"


# alias vi=nvim


# alias ncdu="ncdu --color off"


# alias bat="bat --style=plain"


function show_all_branches() {
  ls | xargs -n 1 -I ^ /bin/bash -c 'cd ^; echo "$(git branch --show-current) -> ^"'
}

function pull_all_repos() {
  time ( \
    ls | xargs -n 1 -P 0 -I ^ \
      /bin/bash -c 'echo "⏳ Processing ^..." && cd ^ && git status && git fetch --all --prune --jobs=10 && git pull && echo' \
  )
}


# start ssh agent
eval "$(ssh-agent)" > /dev/null


# starship
eval "$(starship init zsh)"


# fzf
source "/usr/share/doc/fzf/examples/key-bindings.zsh"


# jenv
function init_jenv() {
  export PATH="$HOME/.jenv/bin:$PATH"
  eval "$(jenv init -)"
}

# for JVM-based workflow convenience
init_jenv


alias gdh="gdiff HEAD"

function gdiff() {
  GDIFF_PREVIEW="git diff $@ --color=always -- {-1}"
  git diff $@ --name-only | fzf -m --ansi --preview-window 'top,85%,wrap' --preview "${GDIFF_PREVIEW}"
}


function update_apt() {
  sudo apt update && sudo apt upgrade -y && sudo apt autoremove -y
}

# tldr's apt version's very old and --update doesn't work
function update_tldr() {
  echo "update_tldr BEGIN"
  tldr --update
  echo "update_tldr END"
}

function update_all() {
  update_apt && \
    echo && omz update && \
    echo && flatpak update && \
    echo && am -u
}