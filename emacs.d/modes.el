;;; auto-complete

(use-package auto-complete
  :config
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (ac-config-default)
  (add-to-list 'ac-modes 'ruby-mode)
  (add-to-list 'ac-modes 'web-mode)
  (add-to-list 'ac-modes 'haml-mode))


;;; dictionary

(setq ispell-dictionary "castellano")
(modify-coding-system-alist 'file "\\.log\\'" 'utf-8)


;;; dpaste

(use-package dpaste)


;;; emacs-git-gutter

(use-package git-gutter
  :config
  (global-git-gutter-mode +1))


;;; flycheck

(use-package flycheck
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


;;; ivy

(use-package ivy
  :bind (("C-s" . swiper)
         ("C-c j" . counsel-git-grep))
  :config
  (ivy-mode 1)
  (global-set-key (kbd "C-s")  'swiper)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-c C-r") 'ivy-resume))

;;; ido

(use-package ido
  :defer t
  :config
  (ido-mode 'both)
  (setq
   ido-ignore-buffers '("^ " "*scra" "*Mess" "*Compl" "*Hel")
   ido-ignore-files '("^ " "#" "Gemfile.lock" "README.rdoc" "ac-comph")
   ido-max-prospects 8
   ido-enable-flex-matching t))


;;; markdown-mode

(use-package markdown-mode
  :config
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))


;;; projectile

(use-package projectile
  :defer t
  :bind ("C-c p p" . projectile-switch-project)
  :config
  (setq projectile-require-project-root nil)
  (add-hook 'ruby-mode-hook #'(lambda () (projectile-mode))))


;;; rbenv.el

(use-package rbenv
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


;;; scss-mode

(use-package scss-mode
  :config
  (autoload 'scss-mode "scss-mode")
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
  (setq scss-compile-at-save nil))


;;; smartparens

(use-package smartparens-config
  :ensure smarparens
  :config
  (progn
    (show-smartparens-global-mode t)))

(sp-with-modes '(ruby-mode js-mode web-mode)
  (sp-local-pair "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (sp-local-pair "[" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (sp-local-pair "(" nil :post-handlers '((my-create-newline-and-enter-sexp "RET"))))

(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))


;;; smex

(use-package smex
  :defer t
  :bind ("M-x" . smex)
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items")))


;;; twittering-mode

(use-package twittering-mode
  :config
  (autoload 'twit "twittering-mode" nil t)
  (setq twittering-icon-mode t)
  (setq twittering-timer-interval 120))


;;; web-mode

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-hook 'web-mode-hook #'(lambda () (smartparens-mode -1)))
  (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "brightwhite")
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-script-padding 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t))


;;; yaml-mode

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))


;;; yasnippet

(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'ruby-mode-hook #'(lambda () (yas-minor-mode)))
  (add-hook 'js-mode-hook #'(lambda () (yas-minor-mode)))
  (add-hook 'web-mode-hook #'(lambda () (yas-minor-mode)))
  (setq yas/snippet-dirs '("~/.emacs.d/yasnippet/snippets")))
