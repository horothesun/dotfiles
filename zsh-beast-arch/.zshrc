#export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="robbyrussell"

plugins=(
  git
  web-search
)

#source "${ZSH}/oh-my-zsh.sh"


setopt share_history

# mill (JVM build tool):
# `[_]` is a pattern used in mill, by default it's a default zsh match pattern.
# Avoid the collision with the following zsh config.
unsetopt nomatch


# used by gh
export BROWSER="brave"


function clean_java_instances() {
  ps auxww | \grep "/bin/java" | \grep -v "grep" |\
    jq --raw-input 'split(" ") | map(select(. != ""))[1]' |\
    xargs --no-run-if-empty --max-args 1 kill --signal 9
}


alias cl=clear


#alias xcopy="xclip -rmlastnl -selection clipboard"
#alias xpaste="xsel --clipboard"


alias vi=nvim


alias rg="rg --hidden"


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
source <(fzf --zsh)


alias gdh="gdiff HEAD"

function gdiff() {
  GDIFF_PREVIEW="git diff $@ --color=always -- {-1}"
  git diff $@ --name-only | fzf -m --ansi --preview-window 'top,85%,wrap' --preview "${GDIFF_PREVIEW}"
}


# tldr's apt version's very old and --update doesn't work
function update_tldr() {
  echo "update_tldr BEGIN"
  tldr --update
  echo "update_tldr END"
}
