export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="robbyrussell"

plugins=(
  git
  web-search
)

source "${ZSH}/oh-my-zsh.sh"


PATH="${PATH}:${HOME}/.cargo/bin"


if [[ -x "$(command -v cached-nix-shell)" ]]; then
  NIX_SHELL_COMMAND="cached-nix-shell"
else
  NIX_SHELL_COMMAND="nix-shell"
fi


alias xcopy="xclip -rmlastnl -selection clipboard"
alias xpaste="xsel --clipboard"


alias vi=nvim


function rg() {
  rg_ --hidden "$@"
}
function rg_() {
  RIPGREP_ARGS="$@"
  "${NIX_SHELL_COMMAND}" -p ripgrep --run "rg ${RIPGREP_ARGS}"
}


function ncdu() {
  NCDU_ARGS="$@"
  "${NIX_SHELL_COMMAND}" -p ncdu --run "ncdu --color off ${NCDU_ARGS}"
}


function bat() {
  bat_ --style=plain "$@"
}
function bat_() {
  BAT_ARGS="$@"
  "${NIX_SHELL_COMMAND}" -p bat --run "bat ${BAT_ARGS}"
}


function tldr() {
  TLDR_ARGS="$@"
  "${NIX_SHELL_COMMAND}" -p tldr --run "tldr ${TLDR_ARGS}"
}


function aws() {
  AWS_CLI_ARGS="$@"
  "${NIX_SHELL_COMMAND}" -p awscli2 --run "aws ${AWS_CLI_ARGS}"
}


function scala-cli() {
  SCALA_CLI_ARGS="$@"
  "${NIX_SHELL_COMMAND}" -p scala-cli --run "scala-cli ${SCALA_CLI_ARGS}"
}


# preferred editor for local and remote sessions
if [[ -n "${SSH_CONNECTION}" ]]; then
  export VISUAL="vi"
else
  export VISUAL="nvim"
fi
export EDITOR="${VISUAL}"


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
  sudo apt update && \
    sudo apt upgrade -y && \
    sudo apt dist-upgrade -y && \
    sudo apt autoremove -y
}

# tldr's apt version's very old and --update doesn't work
function update_tldr() {
  echo "update_tldr BEGIN"
  tldr --update
  echo "update_tldr END"
}

function update_flatpaks() {
  echo "update_flatpaks BEGIN"
  flatpak update
  echo "update_flatpaks END"
}

function update_appimages() {
  echo "update_appimages BEGIN"
  am update
  echo "update_appimages END"
}

function update_alacritty() {
  echo "update_alacritty BEGIN"
  cargo install alacritty
  echo "update_alacritty END"
}

function update_all() {
  update_apt
  echo
  omz update
  echo
  update_flatpaks
  echo
  update_appimages
  echo
  update_alacritty
}
