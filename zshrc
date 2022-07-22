export ZSH="/home/jz/.oh-my-zsh"

ZSH_THEME="af-magic"

DISABLE_UPDATE_PROMPT="true"
COMPLETION_WAITING_DOTS="true"

plugins=(
    git
    vi-mode
)

source $ZSH/oh-my-zsh.sh

export HISTSIZE=1000000000
export SAVEHIST=$HISTSIZE
setopt EXTENDED_HISTORY

export LANG=en_US.UTF-8

alias myedit='emacsclient --alternate-editor="" --no-wait $*'
export EDITOR='emacsclient --alternate-editor="" --no-wait $*'
# export EDITOR='vim'
# alias myedit='vim'
alias emacsconfig="myedit ~/.emacs.el"
alias zshconfig="myedit -nw ~/.zshrc"
alias zshsource="source ~/.zshrc"
alias vimconfig="myedit -nw ~/.vimrc"
alias edit-text="myedit"
alias taskjuggler='tj3'
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

# source ~/Work/tech-unit/colgate/predictive-forecasting/sh/colgate.sh
# source ~/.zsh-parts/colgate.sh
source /home/jz/Work/tech-unit/model-marketplace/sh/model-marketplace.sh
source ~/.zsh-parts/model-marketplace.sh
export PATH="$PATH:/home/jz/.dotnet/tools"

function activate-dotnet-datapi {
    export DOTNET_ROOT="/home/jz/.local/dotnet-5.0.403"
    export MSBuildSDKsPath=$DOTNET_ROOT/sdk/$(${DOTNET_ROOT}/dotnet --version)/Sdks
    export PATH="${PATH}:${DOTNET_ROOT}"
}

function activate-dotnet-cpf {
    export DOTNET_ROOT="/home/jz/.local/dotnet-5.0.201"
    export MSBuildSDKsPath=$DOTNET_ROOT/sdk/$(${DOTNET_ROOT}/dotnet --version)/Sdks
    export PATH="${DOTNET_ROOT}:${PATH}"
}

function activate-dotnet-6 {
    export DOTNET_ROOT="/home/jz/.local/dotnet-6.0.301/"
    export MSBuildSDKsPath=$DOTNET_ROOT/sdk/$(${DOTNET_ROOT}/dotnet --version)/Sdks
    export PATH="${DOTNET_ROOT}:${PATH}"
}

activate-dotnet-6

export DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=1
source /home/jz/.local/bin/git-subrepo/.rc
if [ -e /home/jz/.nix-profile/etc/profile.d/nix.sh ]; then . /home/jz/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
source /home/jz/.nix-profile/etc/profile.d/nix.sh
