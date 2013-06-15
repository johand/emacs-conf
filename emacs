;;; identica
(add-to-list 'load-path "~/.emacs.d/identica-mode")
(autoload 'identica "identica-mode" nil t)

;;; twitter
(add-to-list 'load-path "~/.emacs.d/twittering-mode")
(autoload 'twit "twittering-mode" nil t)
(setq twittering-icon-mode t)
(setq twittering-timer-interval 120)

;;; php-mode
(add-to-list 'load-path "~/.emacs.d/php-mode/")
(autoload 'php-mode "php-mode" t)
(add-to-list 'auto-mode-alist
	          '("\\.php[34]?\\'\\|\\.phtml\\'" . php-mode))

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
(require 'magit)

;;; scss-mode
(add-to-list 'load-path (expand-file-name "~/.emacs.d/scss-mode"))
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;;; rinari
(add-to-list 'load-path "~/.emacs.d/rinari")
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(require 'rinari)

;;; rhtml-mode
(add-to-list 'load-path "~/.emacs.d/rhtml")
(require 'rhtml-mode)

;;; projectile
(add-to-list 'load-path "~/.emacs.d/projectile")
(add-to-list 'load-path "~/.emacs.d/dash.el")
(add-to-list 'load-path "~/.emacs.d/s.el")
(require 'projectile)
(add-hook 'ruby-mode-hook #'(lambda () (projectile-mode)))
(add-hook 'php-mode-hook #'(lambda () (projectile-mode)))

;;; iswitchb
(iswitchb-mode 1)
(setq iswitchb-buffer-ignore '("^ " "*scratch" "*Messages" "*Completions" "*tw"))
(defun iswitchb-local-keys ()
  (mapc (lambda (K) 
	  (let* ((key (car K)) (fun (cdr K)))
	    (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
	'(("<right>" . iswitchb-next-match)
	  ("<left>"  . iswitchb-prev-match)
	  ("<up>"    . ignore             )
	  ("<down>"  . ignore             ))))
(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

;;; dictionary
(setq ispell-dictionary "castellano")

;;; marmalade
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;; smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(global-set-key (kbd "M-x") 'smex)

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

;;; gentoo-syntax
(add-to-list 'load-path "/usr/portage/app-emacs/gentoo-syntax/files")
(autoload 'ebuild-mode "gentoo-syntax"
  "Major mode for Portage .ebuild and .eclass files." t)
(autoload 'eselect-mode "gentoo-syntax" "Major mode for .eselect files." t)
(autoload 'gentoo-newsitem-mode "gentoo-syntax"
  "Major mode for Gentoo GLEP 42 news items." t)

(add-to-list 'auto-mode-alist
             '("\\.\\(ebuild\\|eclass\\|eblit\\)\\'" . ebuild-mode))
(add-to-list 'auto-mode-alist '("\\.eselect\\'" . eselect-mode))
(add-to-list 'auto-mode-alist
             '("/[0-9]\\{4\\}-[01][0-9]-[0-3][0-9]-.+\\.[a-z]\\{2\\}\\.txt\\'"
               . gentoo-newsitem-mode))
(add-to-list 'interpreter-mode-alist '("runscript" . sh-mode))
(modify-coding-system-alist
 'file "\\.\\(ebuild\\|eclass\\|eblit\\|eselect\\)\\'" 'utf-8)
(setq ebuild-mode-portdir "@PORTDIR@")

;;; setting rbenv path
(setenv "PATH" (concat (getenv "HOME") "/.rbenv/shims:" (getenv "HOME") "/.rbenv/bin:" (getenv "PATH")))
(setq exec-path (cons (concat (getenv "HOME") "/.rbenv/shims") (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))

;;; browser
;(setq browse-url-browser-function 'browse-url-generic
   ; browse-url-generic-program "foo")

;;; theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/zen-and-art-theme")

(custom-set-variables
 '(custom-enabled-themes (quote (zen-and-art)))
 '(custom-safe-themes (quote ("648ef38f7a6e5cef4a04f529b6bc66834447905b46aa5828089ef88327209d01" default)))
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(custom-set-faces
 
 )

(define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
