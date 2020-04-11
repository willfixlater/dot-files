#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
export PS1='[\u@\h \W]\$ '

export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):1
source .bash_aliases
