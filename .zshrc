export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

#GIT_PS1_SHOWDIRTYSTATE=ture
#export PS1='\[\e[36m\]\W\[\e[36m\]$(__git_ps1) \$\[\e[0m\] '

alias ls='ls -G'
alias la='ls -la'
alias ll='ls -l'

alias cdde='cd ~/Desktop'
alias cddo='cd ~/Documents'

source ~/.git-completion
source ~/.git-prompt.sh
#source ch_term_color.sh

#GIT_PS1_SHOWDIRTYSTATE=ture
export PS1='%F{cyan}%n%f:%F{green}%c%f$(__git_ps1)$ '
#export PS1='%F{blue}%n%f:%F{green}%~%f$ '

# Git
 
fpath=(~/.zsh $fpath)
 
if [ -f ${HOME}/.zsh/git-completion.zsh ]; then
        zstyle ':completion:*:*:git:*' script ~/.zsh/git-completion.zsh
fi
 
if [ -f ${HOME}/.zsh/git-prompt.sh ]; then
        source ${HOME}/.zsh/git-prompt.sh
fi
 
GIT_PS1_SHOWDIRTYSTATE=true
GIT_PS1_SHOWUNTRACKEDFILES=true
GIT_PS1_SHOWSTASHSTATE=true
GIT_PS1_SHOWUPSTREAM=auto
 
setopt PROMPT_SUBST ; 
#PS1='[%n@%m %c$(__git_ps1 " (%s)")]\$ '

clear