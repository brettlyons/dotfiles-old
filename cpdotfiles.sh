#!/bin/sh
# Don't need as many of these as I used to.
# cp ~/dotfiles/.i3/config ~/.config/i3/config
# cp ~/dotfiles/.Xresources ~/.Xresources
cp ~/dotfiles/.spacemacs ~/
cp ~/dotfiles/.zshrc ~/.zshrc
cp ~/dotfiles/.zshenv ~/.zshenv
# cp ~/dotfiles/.vimrc ~/.vimrc
# cp -r ~/dotfiles/.vim ~/.config/nvim
# ln -s ~/.vim ~/.config/nvim
# cp -r ~/dotfiles/.vim ~/.vim
# install spacemacs
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
# and antigen
mkdir ~/.antigen
curl -L git.io/antigen > ~/.antigen/antigen.zsh
