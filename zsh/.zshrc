# Path to your oh-my-zsh installation
export ZSH=~/.oh-my-zsh

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="robbyrussell"


# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup
plugins=(git)


# Homebrew autocomplete
# Must be done before `compinit` gets called (i.e.: `oh-my-zsh.sh`)
# Reference: https://docs.brew.sh/Shell-Completion#configuring-completions-in-zsh
if type brew &>/dev/null; then
  FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"
  autoload -Uz compinit
  compinit
fi


source $ZSH/oh-my-zsh.sh


# unsetopt inc_append_history
setopt share_history


# HOMEBREW_OPT_DIR: different values between Intel and Apple Silicon (arm64)
export HOMEBREW_OPT_DIR=$(brew --prefix)/opt

# for Homebrew's formulae installed in /usr/local/sbin
export PATH=/usr/local/sbin:$PATH


# ~/bin for custom automation scripts and
# tfswitch global terraform version
export PATH=$HOME/bin:$PATH


alias ncdu="ncdu --color off"


alias vi=nvim


alias bat="bat --style=plain"


# preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export VISUAL='vim'
else
  export VISUAL='nvim'
fi
export EDITOR=$VISUAL


# rbenv
function initRbenv() {
  eval "$(rbenv init -)"
  export RUBY_CONFIGURE_OPTS="--with-openssl-dir=$(brew --prefix openssl@1.1)"
}


# nvm
function initNvm() {
  export NVM_DIR="$HOME/.nvm"
  [ -s "$HOMEBREW_OPT_DIR/nvm/nvm.sh" ] && . "$HOMEBREW_OPT_DIR/nvm/nvm.sh"
  [ -s "$HOMEBREW_OPT_DIR/nvm/etc/bash_completion.d/nvm" ] && . "$HOMEBREW_OPT_DIR/nvm/etc/bash_completion.d/nvm"
}

# vim/neovim's CoC plugin requires Node.js
initNvm


# lynx
export WWW_HOME=https://duckduckgo.com/


# starship
eval "$(starship init zsh)"


# fzf: different configuration between Intel and Apple Silicon (arm64)
if [ "`arch`" = arm64 ]; then
  source $HOMEBREW_OPT_DIR/fzf/shell/key-bindings.zsh
  source $HOMEBREW_OPT_DIR/fzf/shell/completion.zsh
else
  [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
fi


# pyenv
function initPyenv() {
  export PYENV_ROOT="$HOME/.pyenv"
  export PATH="$PYENV_ROOT/bin:$PATH"
  eval "$(pyenv init --path)"
}

# required after macOS 12.3 removed system Python
initPyenv


# jenv
function initJenv() {
  export PATH="$HOME/.jenv/bin:$PATH"
  eval "$(jenv init -)"
}

# for JVM-based workflow convenience
initJenv

function resetJenv() {
  echo resetJenv BEGIN
  echo jenv versions \# old
  jenv versions
  echo
  ls $HOME/.jenv/versions | xargs -n 1 jenv remove
  echo
  /usr/libexec/java_home -V
  echo
  JVM_LIBRARY_BASE_DIR=/Library/Java/JavaVirtualMachines
  ls $JVM_LIBRARY_BASE_DIR | xargs -n 1 -I ^ jenv add $JVM_LIBRARY_BASE_DIR/^/Contents/Home
  echo
  echo jenv versions \# new
  jenv versions
  echo
  echo jenv doctor
  jenv doctor
  echo resetJenv END
}


alias gdh="gdiff HEAD"

function gdiff() {
  GDIFF_PREVIEW="git diff $@ --color=always -- {-1}"
  git diff $@ --name-only | fzf -m --ansi --preview-window 'top,85%,wrap' --preview $GDIFF_PREVIEW
}


function updateGems() {
  echo updateGems BEGIN
  initRbenv && time ( gem update && gem cleanup )
  echo updateGems END
}

function updateBrews() {
  echo updateBrews BEGIN
  initJenv && initPyenv && time ( brew update && brew upgrade && brew upgrade --cask; brew cleanup; resetJenv )
  echo updateBrews END
}

function updateNode() {
  echo updateNode BEGIN
  initNvm && time ( npm update --global )
  echo updateNode END
}

function updateAll() {
  updateGems && echo && updateBrews && echo && updateNode && echo && omz update
}


# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
function __condaSetupScript() {
  "$(brew --prefix)/Caskroom/miniconda/base/bin/conda" 'shell.zsh' 'hook' 2> /dev/null
}

function initConda() {
  __conda_setup="$( __condaSetupScript )"
  if [ $? -eq 0 ]; then
    eval "$__conda_setup"
  else
    if [ -f "$(brew --prefix)/Caskroom/miniconda/base/etc/profile.d/conda.sh" ]; then
      . "$(brew --prefix)/Caskroom/miniconda/base/etc/profile.d/conda.sh"
    else
      export PATH="$(brew --prefix)/Caskroom/miniconda/base/bin:$PATH"
    fi
  fi
  unset __conda_setup
}
# <<< conda initialize <<<
