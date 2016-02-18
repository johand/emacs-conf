## Installation


clone

    git clone git://github.com/johand/emacs-conf.git

create symlink

    ln -rs emacs-conf/emacs.d ~/.emacs.d

before opening emacs install and update submodules

    git submodule update --init --recursive
    git submodule foreach git pull origin master

open emacs and wait to finish the packages installation
