alias wm="nohup xmonad > /dev/null 2>&1 &"
alias remap-keys="xmodmap ~/.Xmodmap"
alias emd="emacs --daemon=default"
alias go="(wm) && (emd) && (remap-keys)"

alias em="nohup emacsclient -s default -c > /dev/null 2>&1 &"
alias emnw="emacs -nw"
