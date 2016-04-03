(custom-set-variables
 
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))


;;; melpa

(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)


;;; use-package

(setq package-list '(use-package))

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'use-package)


;;; modes

(setq modes (expand-file-name "modes.el" user-emacs-directory))
(load modes)


;;; theme

(setq theme (expand-file-name "theme.el" user-emacs-directory))
(load theme)
