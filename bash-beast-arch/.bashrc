#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias cl='clear'
alias l='ls -lah --color=auto'
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias bat='bat --style=plain'

# git aliases
alias gcl='git clone --recurse-submodules'
alias gb='git branch'
alias gbD='git branch --delete --force'
alias gco='git checkout'
alias gfa='git fetch --all --tags --prune --jobs=10'
alias gl='git pull'
alias gst='git status'
alias ga='git add'
alias gaa='git add --all'
alias gc='git commit --verbose'
alias gp='git push'

alias vi='nvim'

PS1='[\u@\h \W]\$ '

# GitHub SSH key
eval "$(ssh-agent)" > /dev/null
SSH_ASKPASS_REQUIRE="force" SSH_ASKPASS="${HOME}/.ssh/askpass.sh" ssh-add "${HOME}/.ssh/horothesun" &> /dev/null

eval "$(fzf --bash)"

PATH=$PATH:$HOME/bin
