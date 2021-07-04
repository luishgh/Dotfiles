# /etc/skel/.bashrc
#
# This file is sourced by all *interactive* bash shells on startup,
# including some apparently interactive shells such as scp and rcp
# that can't tolerate any output.  So make sure this doesn't display
# anything or bad things will happen !


# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
	# Shell is non-interactive.  Be done now!
	return
fi


# Put your fun stuff here.

# Dotfiles repo
alias dotrepo="git --git-dir=$HOME/Documents/dotfiles --work-tree=$HOME"

# Doom Emacs
# export PATH=$HOME/.emacs.d/bin:$PATH

export EDITOR="nvim"

# Export .local/bin to PATH
export PATH=$HOME/.local/bin:$PATH

# Export .yarn/bin to PATH
export PATH=$HOME/.yarn/bin:$PATH

# Export cargo bins to PATH
export PATH=$HOME/.cargo/bin:$PATH

# Prompt
export PS1="\[$(tput bold)\]\[$(tput setaf 1)\][\[$(tput setaf 3)\]\u\[$(tput setaf 2)\]@\[$(tput setaf 4)\]\h \[$(tput setaf 5)\]\W\[$(tput setaf 1)\]]\[$(tput setaf 7)\]\\$ \[$(tput sgr0)\]"
