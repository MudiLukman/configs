#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='\[\e[32m\][\u@\h \W]\$\[\e[0m\] '

alias ls='ls --color=auto'
alias grep='grep --color=auto'
# Uncomment to switch to default
#PS1='[\u@\h \W]\$ '
