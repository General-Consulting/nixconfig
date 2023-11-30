# Created by newuser for 5.8
source ~/.dotfiles/aliases.sh
source ~/.dotfiles/functions.sh
source ~/.dotfiles/exports.sh
[ -x $(command -v direnv) ] && eval "$(direnv hook zsh)"
#eval `ssh-agent` # Autostart ssh-agent
#source <(summon --bash-completion-script `which summon`)
bindkey '^[[Z' autosuggest-accept  # shift + tab  | autosuggest
# Dir: current working directory
# https://njkyu.com/2020/11/26/fish/
prompt_dir() {
  setopt prompt_subst
  prompt_segment blue $CURRENT_FG "$(shrink_path -f)"
}
