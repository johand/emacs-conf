## Instalacion


**clonar repositorio**

    git clone git://github.com/johand/emacs-conf.git

**crear enlaces simbolicos**

    ln -s emacs-conf/emacs.d ~/.emacs.d

    ln -s emacs ~/.emacs

**instalar y actualizar submodulos**

    git submodule update --init --recursive

    git submodule foreach git pull origin master
