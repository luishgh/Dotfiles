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
if [ -n "$GUIX_ENVIRONMENT" ]
then
    # Guix env prompt
    export PS1="\u@\h \w [guix-env]\$ "
else
    # Normal prompt
    export PS1="\[$(tput bold)\]\[$(tput setaf 1)\][\[$(tput setaf 3)\]\u\[$(tput setaf 2)\]@\[$(tput setaf 4)\]\h \[$(tput setaf 5)\]\W\[$(tput setaf 1)\]]\[$(tput setaf 7)\]\\$ \[$(tput sgr0)\]"
fi

