#GIT_PS1_SHOWDIRTYSTATE=ture
#export PS1='\[\e[36m\]\W\[\e[36m\]$(__git_ps1) \$\[\e[0m\] '

alias ls='ls -G'
alias la='ls -la'
alias ll='ls -l'

alias cdde='cd ~/Desktop'
alias cddo='cd ~/Documents'

source ~/.git-completion
source ~/git-prompt.sh
#source ch_term_color.sh

GIT_PS1_SHOWDIRTYSTATE=ture
export PS1='\[\e[36m\]\W\[\e[36m\]$(__git_ps1) \$\[\e[0m\] '
clear

