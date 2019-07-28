#!/bin/sh

# Try to symlink .ssh directory from old $HOME
if [ ! -L "/$HOME/.ssh" ]; then
    ln -s "/home/$USER/.ssh" "$HOME/.ssh"
fi

exec s17-turbo-switch "$@"
