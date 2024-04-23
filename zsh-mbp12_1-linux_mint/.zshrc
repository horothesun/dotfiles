export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="robbyrussell"

plugins=(
  git
  web-search
)

source "${ZSH}/oh-my-zsh.sh"


PATH="${PATH}:${HOME}/.cargo/bin"


# used by gh
export BROWSER="brave"


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


function gh() {
  GH_ARGS="$@"
  "${NIX_SHELL_COMMAND}" -p gh --run "gh ${GH_ARGS}"
}


function htop() {
  HTOP_ARGS="$@"
  "${NIX_SHELL_COMMAND}" -p htop --run "htop ${HTOP_ARGS}"
}


function btop() {
  BTOP_ARGS="$@"
  "${NIX_SHELL_COMMAND}" -p btop --run "btop ${BTOP_ARGS}"
}


function trans() {
  TRANSLATE_SHELL_ARGS="$@"
  "${NIX_SHELL_COMMAND}" -p translate-shell --run "trans ${TRANSLATE_SHELL_ARGS}"
}


function tldr() {
  TLDR_ARGS="$@"
  "${NIX_SHELL_COMMAND}" -p tldr --run "tldr ${TLDR_ARGS}"
}


function pdfview() {
  ZATHURA_ARGS="$@"
  ( "${NIX_SHELL_COMMAND}" -p zathura --run "zathura ${ZATHURA_ARGS}" & ) &> /dev/null
}


function aws() {
  AWS_CLI_ARGS="$@"
  "${NIX_SHELL_COMMAND}" -p awscli2 --run "aws ${AWS_CLI_ARGS}"
}


function scala-cli() {
  SCALA_CLI_ARGS="$@"
  "${NIX_SHELL_COMMAND}" -I "nixpkgs=channel:nixpkgs-unstable" -p scala-cli --run "scala-cli ${SCALA_CLI_ARGS}"
}


function asciiquarium() {
  ASCIIQUARIUM_ARGS="$@"
  "${NIX_SHELL_COMMAND}" -p asciiquarium --run "LANG=C asciiquarium ${ASCIIQUARIUM_ARGS}"
}


IDEA_FLATPACK_ID="com.jetbrains.IntelliJ-IDEA-Community"

function idea() {
  if [[ $# -eq 0 ]]; then
    ( flatpak run "${IDEA_FLATPACK_ID}" & ) &> /dev/null
  else
    IDEA_ARGS="$@"
    ( flatpak run "${IDEA_FLATPACK_ID}" "${IDEA_ARGS}" & ) &> /dev/null
  fi
}

function idea_verbose() {
  if [[ $# -eq 0 ]]; then
    flatpak run "${IDEA_FLATPACK_ID}"
  else
    IDEA_ARGS="$@"
    flatpak run "${IDEA_FLATPACK_ID}" "${IDEA_ARGS}"
  fi
}


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


# start ssh agent
eval "$(ssh-agent)" > /dev/null

# GitHub SSH key
SSH_ASKPASS_REQUIRE="force" SSH_ASKPASS="${HOME}/.ssh/askpass.sh" \
  ssh-add "${HOME}/.ssh/horothesun" &> /dev/null


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

function reset_jenv() {
  echo "reset_jenv BEGIN"
  echo "jenv versions # old"
  jenv versions
  echo
  ls "${HOME}/.jenv/versions" | xargs -n 1 jenv remove
  echo
  JVM_LIBRARY_BASE_DIR="/usr/lib/jvm"
  echo "${JVM_LIBRARY_BASE_DIR} content:"
  ls -lah "${JVM_LIBRARY_BASE_DIR}"
  echo
  ls "${JVM_LIBRARY_BASE_DIR}" | grep "temurin" | xargs -I ^ jenv add "${JVM_LIBRARY_BASE_DIR}/^"
  echo
  echo "jenv versions # new"
  jenv versions
  echo
  echo "jenv doctor"
  jenv doctor
  echo "reset_jenv END"
}


alias gdh="gdiff HEAD"

function gdiff() {
  GDIFF_PREVIEW="git diff $@ --color=always -- {-1}"
  git diff $@ --name-only | fzf -m --ansi --preview-window 'top,85%,wrap' --preview "${GDIFF_PREVIEW}"
}


function update_apt() {
  echo "update_apt BEGIN"
  echo "Kernel: $(uname -r)"
  echo
  sudo apt update && \
    sudo apt upgrade -y && \
    sudo apt dist-upgrade -y && \
    sudo apt autoremove -y && \
    sudo apt clean
  echo
  echo "Kernel: $(uname -r)"
  echo "update_apt END"
}

function update_starship() {
  echo "update_starship BEGIN"
  starship --version
  echo
  HOME_TMP_DIR="${HOME}/tmp"
  STARSHIP_INSTALL_SH="${HOME_TMP_DIR}/install_starship.sh"
  mkdir -p "${HOME_TMP_DIR}"
  curl -sS "https://starship.rs/install.sh" > "${STARSHIP_INSTALL_SH}"
  chmod u+x "${STARSHIP_INSTALL_SH}"
  "${STARSHIP_INSTALL_SH}" --yes
  rm "${STARSHIP_INSTALL_SH}"
  echo "update_starship END"
}

# tldr's apt version's very old and --update doesn't work
function update_tldr() {
  echo "update_tldr BEGIN"
  tldr --update
  echo "update_tldr END"
}

function update_jenv() {
  echo "update_jenv BEGIN"
  jenv --version
  echo
  (
    cd "${HOME}/.jenv"
    git pull origin master
  )
  echo
  jenv --version
  echo "update_jenv END"
}

function update_flatpaks() {
  echo "update_flatpaks BEGIN"
  flatpak list --app
  echo
  flatpak update --assumeyes && \
    flatpak uninstall --unused
  echo "update_flatpaks END"
}

function update_appimages() {
  echo "update_appimages BEGIN"
  am files
  am update && \
    am clean
  echo "update_appimages END"
}

function update_alacritty() {
  echo "update_alacritty BEGIN"
  alacritty --version
  echo
  cargo install alacritty
  echo "update_alacritty END"
}

function update_all() {
  update_apt
  echo
  reset_jenv
  echo
  update_jenv
  echo
  update_flatpaks
  echo
  update_appimages
  echo
  update_tldr
  echo
  update_starship
  echo
  update_alacritty
  echo
  omz update
}
