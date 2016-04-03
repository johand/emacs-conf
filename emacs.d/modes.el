;;; auto-complete

(use-package auto-complete
  :ensure t
  :config
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (ac-config-default)
  (add-to-list 'ac-modes 'web-mode)
  (add-to-list 'ac-modes 'haml-mode))


;;; dictionary

(setq ispell-dictionary "castellano")
(modify-coding-system-alist 'file "\\.log\\'" 'utf-8)


;;; dpaste

(use-package dpaste
  :load-path "site-packages/dpaste.el")


;;; emacs-git-gutter

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1))


;;; flycheck

(use-package flycheck
  :ensure t
  :config
  (add-hook 'ruby-mode-hook #'global-flycheck-mode)
  (add-hook 'js-mode-hook #'global-flycheck-mode))


;;; gnusocial

(use-package gnu-social-mode
  :load-path "site-packages/gnu-social-mode"
  :config
  (setq gnu-social-server "gnusocial.no"
	gnu-social-server-textlimit 1000)
  (autoload 'gnu-social-mode "gnu-social-mode" nil t))


;;; helm

(use-package helm
  :ensure t
  :config
  (require 'helm-config)
  (add-hook 'ruby-mode-hook #'(lambda () (helm-mode)))
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c")))


;;; ido

(use-package ido
  :config
  (ido-mode 'both)
  (setq
   ido-ignore-buffers '("^ " "*scra" "*Mess" "*Compl" "*Hel")
   ido-ignore-files '("^ " "#" "Gemfile.lock" "README.rdoc" "ac-comph")
   ido-max-prospects 8
   ido-enable-flex-matching t))


;;; markdown-mode

(use-package markdown-mode
  :ensure t
  :config
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))


;;; projectile

(use-package projectile
  :ensure t
  :config
  (setq projectile-require-project-root nil)
  (add-hook 'ruby-mode-hook #'(lambda () (projectile-mode))))


;;; rbenv.el

(use-package rbenv
  :ensure t
  :config
  (setq rbenv-modeline-function 'rbenv--modeline-plain)
  (add-hook 'ruby-mode-hook #'(lambda () (global-rbenv-mode))))


;;; ruby-mode

(use-package ruby-mode
  :config
  (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("rake" . ruby-mode))
  (eval-after-load 'ruby-mode
    '(progn
       (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent))))


;;; ruby-end

(use-package ruby-end
  :ensure t)


;;; scss-mode

(use-package scss-mode
  :ensure t
  :config
  (autoload 'scss-mode "scss-mode")
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
  (setq scss-compile-at-save nil))


;;; smartparens

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode t))


;;; smex

(use-package smex
  :ensure t
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (global-set-key (kbd "M-x") 'smex))


;;; twittering-mode

(use-package twittering-mode
  :ensure t
  :config
  (autoload 'twit "twittering-mode" nil t)
  (setq twittering-icon-mode t)
  (setq twittering-timer-interval 120))


;;; web-mode

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-hook 'web-mode-hook #'(lambda () (smartparens-mode -1)))
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t))


;;; yaml-mode

(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))


;;; yasnippet

(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'ruby-mode-hook #'(lambda () (yas-minor-mode)))
  (setq yas/snippet-dirs '("~/.emacs.d/yasnippet/snippets")))
