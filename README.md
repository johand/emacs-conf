## Instalacion


**clonar repositorio**

<code>git clone git://github.com/johand/emacs-conf.git</code>


**crear enlace simbolico y copiar fichero**

<code>ln -s emacs-conf/emacs.d ~/.emacs.d</code>

<code>cd emacs-conf</code>

<code>cp emacs ~/.emacs</code>


**instalar y actualizar submodulos**

<code>git submodule update --init --recursive</code>

<code>git submodule foreach git pull origin master</code>
