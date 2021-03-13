export ZSH="/home/jz/.oh-my-zsh"

ZSH_THEME="af-magic"

DISABLE_UPDATE_PROMPT="true"
COMPLETION_WAITING_DOTS="true"

plugins=(
    git
    vi-mode
)

source $ZSH/oh-my-zsh.sh

export LANG=en_US.UTF-8

export EDITOR='emacs -nw'

alias emacsconfig="emacs -nw ~/.emacs.el"
alias zshconfig="emacs -nw ~/.zshrc"
alias zshsource="source ~/.zshrc"
alias vimconfig="emacs -nw ~/.vimrc"
alias edit-text="emacs"
alias taskjuggler='tj3'
alias emacs="emacs"
alias vim="emacs -nw"
alias down="cd ~/Downloads"

export PATH=$PATH:/home/jz/.local/bin

export NVM_DIR="/home/jz/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export PATH="/home/$USER/.pyenv/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

function puml {
  java -jar ~/.local/bin/plantuml.jar $@
}

source ~/.zsh-parts/luiza-normey.sh
export PATH="$PATH:/home/jz/.dotnet/tools"
export DOTNET_ROOT="/snap/dotnet-sdk/current"
export MSBuildSDKsPath=$DOTNET_ROOT/sdk/$(${DOTNET_ROOT}/dotnet --version)/Sdks
export PATH="${PATH}:${DOTNET_ROOT}"

alias activate-dotnet5="snap refresh dotnet-sdk --channel=latest/stable"
alias activate-dotnet50100="snap refresh dotnet-sdk --channel=5.0/beta"
alias activate-dotnet3="snap refresh dotnet-sdk --channel=3.1/stable"
alias activate-dotnet2="snap refresh dotnet-sdk --channel=2.1/stable"
export DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=1
source /home/jz/.local/bin/git-subrepo/.rc
