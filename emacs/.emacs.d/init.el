(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)

(eval-when-compile
  (require 'use-package))

(setq delete-old-versions -1 )
(setq version-control t )
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) 
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(setq vc-follow-symlinks t )
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) )
(setq inhibit-startup-screen t )
(setq ring-bell-function 'ignore )
(setq coding-system-for-read 'utf-8 )
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)
(setq default-fill-column 80)
(setq byte-compile-warnings nil)
(global-display-line-numbers-mode)
(setq-default mode-line-format nil)
(menu-bar-mode -1) 

(use-package evil
  :ensure t
  :config
  (evil-mode t))

(use-package evil-easymotion
  :ensure t
  )

(use-package helm
  :ensure t
  :config
  (require 'helm-config)
  (helm-mode 1)
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line)
  )

(use-package ido
  :config
  (ido-mode 1)
  )

(use-package base16-theme
  :ensure t
  :config
  (setq base16-theme-256-color-source "colors")
  (load-theme 'base16-default-dark t)
  )

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-buffers (:hint nil)
    "
^Switch^
^^^^^^^^-----------------------------------------------------------------
_l_: list
"
    ("l" helm-buffers-list :exit t)
    )
  (defhydra hydra-menu nil
    "menu"
    ("b" hydra-buffers/body "buffer" :exit t)
    )
  (define-key evil-normal-state-map (kbd "SPC") 'hydra-menu/body))


(use-package evil-snipe
  :ensure t
  :config
  (evil-snipe-mode 1)
  (setq evil-snipe-scope 'whole-buffer)
  (setq evil-snipe-repeat-scope 'whole-buffer)
  (setq evil-snipe-show-prompt nil)
  )


(use-package linum-relative
  :ensure t
  :config
  (setq linum-relative-backend 'display-line-numbers-mode)
  (linum-on)
  (linum-relative-mode)
  )
