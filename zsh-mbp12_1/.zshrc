export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="robbyrussell"

plugins=(git)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

PATH="${PATH}:${HOME}/.cargo/bin"

eval "$(ssh-agent)" > /dev/null

eval "$(starship init zsh)"

source "/usr/share/doc/fzf/examples/key-bindings.zsh"

export PATH="$HOME/.jenv/bin:$PATH"
eval "$(jenv init -)"

