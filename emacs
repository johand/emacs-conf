;;; twitter
(add-to-list 'load-path "~/.emacs.d/twittering-mode")
(autoload 'twit "twittering-mode" nil t)
(setq twittering-icon-mode t)
(setq twittering-timer-interval 120)

;;; php-mode
(add-to-list 'load-path "~/.emacs.d/php-mode/")
(autoload 'php-mode "php-mode" t)
(add-to-list 'auto-mode-alist
	          '("\\.php[45]?\\'\\|\\.phtml\\'" . php-mode))

;;; flymake-easy
(add-to-list 'load-path "~/.emacs.d/flymake-easy")
(require 'flymake-easy)

;;; flymake-ruby
(add-to-list 'load-path "~/.emacs.d/flymake-ruby")
(require 'flymake-ruby) 
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;;; flymake-php
(add-to-list 'load-path "~/.emacs.d/flymake-php")
(require 'flymake-php) 
(add-hook 'php-mode-hook 'flymake-php-load)

;;; auto-complete
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(add-to-list 'load-path "~/.emacs.d/popup-el")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;;; yasnippet
(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)
(setq yas/snippet-dirs '("~/.emacs.d/yasnippet/snippets" "~/.emacs.d/yasnippet/extras/imported"))
(yas/global-mode 1) 

;;; magit
(add-to-list 'load-path "~/.emacs.d/magit")
(add-to-list 'load-path "~/.emacs.d/git-modes")
(require 'magit)

;;; scss-mode
(add-to-list 'load-path (expand-file-name "~/.emacs.d/scss-mode"))
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;;; rinari
(add-to-list 'load-path "~/.emacs.d/rinari")
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(require 'rinari)

;;; projectile
(add-to-list 'load-path "~/.emacs.d/projectile")
(add-to-list 'load-path "~/.emacs.d/dash.el")
(add-to-list 'load-path "~/.emacs.d/s.el")
(require 'projectile)
(add-hook 'ruby-mode-hook #'(lambda () (projectile-mode)))
(add-hook 'php-mode-hook #'(lambda () (projectile-mode)))

;;; ido-mode
(require 'ido) 
(ido-mode 'both)
(setq 
 ido-ignore-buffers '("^ " "*scra" "*Mess" "*Compl" "*Hel")
 ido-ignore-files '("^ " "#" "Gemfile.lock" "README.rdoc" "ac-comph")
 ido-max-prospects 8
 ido-enable-flex-matching t)

;;; dictionary
(setq ispell-dictionary "castellano")

;;; marmalade
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;; smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(global-set-key (kbd "M-x") 'smex)

;;; ruby indent
(define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)

;;; ruby-block
(require 'ruby-block)
(ruby-block-mode t)

;;; ruby-end
(require 'ruby-end)

;;; autopair
(require 'autopair)
(autopair-global-mode)

;;; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;; rbenv.el
(add-to-list 'load-path (expand-file-name "~/.emacs.d/rbenv.el/"))
(setq rbenv-modeline-function 'rbenv--modeline-plain)
(require 'rbenv)
(add-hook 'ruby-mode-hook #'(lambda () (global-rbenv-mode)))

;;; web-mode
(add-to-list 'load-path "~/.emacs.d/web-mode")
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'\" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-hook 'web-mode-hook #'(lambda () (smartparens-mode -1)))
(setq web-mode-enable-auto-pairing t)
(setq web-mode-tag-auto-close-style 0)
(setq web-mode-enable-auto-pairing nil)

;;; ebuild-mode
(add-to-list 'load-path "/usr/share/emacs/site-lisp/ebuild-mode")
(autoload 'ebuild-mode "ebuild-mode"
  "Major mode for Portage .ebuild and .eclass files." t)
(autoload 'gentoo-newsitem-mode "gentoo-newsitem-mode"
  "Major mode for Gentoo GLEP 42 news items." t)

(add-to-list 'auto-mode-alist
             '("\\.\\(ebuild\\|eclass\\|eblit\\)\\'" . ebuild-mode))
(add-to-list 'auto-mode-alist
             '("/[0-9]\\{4\\}-[01][0-9]-[0-3][0-9]-.+\\.[a-z]\\{2\\}\\.txt\\'"
               . gentoo-newsitem-mode))
(add-to-list 'interpreter-mode-alist '("runscript" . sh-mode))
(modify-coding-system-alist 'file "\\.\\(ebuild\\|eclass\\|eblit\\|eselect\\)\\'" 'utf-8)

;;; browser
;(setq browse-url-browser-function 'browse-url-generic
   ; browse-url-generic-program "foo")

;;; theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/zen-and-art-theme")
(load-theme 'zen-and-art t)

(custom-set-variables
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))

