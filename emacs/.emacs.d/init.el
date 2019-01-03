(require 'package)
(add-to-list 'package-archives '("melpa-mirror" . "http://www.mirrorservice.org/sites/melpa.org/packages/") t)
;(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
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
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(defun display-startup-echo-area-message nil)
(global-display-line-numbers-mode)

(recentf-mode 1)
(setq recentf-max-menu-items 20)

(bind-key (kbd "<escape>") 'keyboard-escape-quit)

(setq-default mode-line-format nil)
(menu-bar-mode -1) 

(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(when window-system
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

(use-package evil :ensure t
  :config
  (evil-mode t)
  :config
  (define-key evil-normal-state-map (kbd "ä") 'switch-to-last-buffer))

(use-package helm :ensure t
  :config
  (require 'helm-config)
  (helm-mode 1)
  (global-set-key (kbd "M-x") 'helm-M-x)
  :bind (:map helm-map
    ("C-j" . 'helm-next-line)
    ("C-k" . 'helm-previous-line)))

(use-package base16-theme :ensure t
  :config
  (setq base16-theme-256-color-source "colors")
  (load-theme 'base16-default-dark t))

(use-package hydra :ensure t :defer t
  :bind (:map evil-normal-state-map ("SPC" . hydra-menu/body)))

(defhydra hydra-buffers (:color blue)
  ("l" helm-mini "list" :column "switch")
  ("s" helm-do-ag-buffers "search in buffers")
  ("a" save-buffer "save" :column "action")
  ("k" (kill-buffer (current-buffer)) "kill"))

(defhydra hydra-emacs (:color blue)
  ("r" (load-file "~/.emacs.d/init.el") "reload")
  ("q" save-buffers-kill-terminal "save and quit"))

(defhydra hydra-files (:color blue)
  ("r" helm-recentf "recent"))

(defhydra hydra-projects (:color blue)
  ("w" helm-projectile-switch-project "switch")
  ("s" helm-do-ag-project-root "search")
  ("f" helm-projectile-find-file "files")
  ("d" projectile-discover-projects-in-directory "discover"))

(defhydra hydra-menu (:color blue)
  ("b" hydra-buffers/body "buffer" :exit t)
  ("e" hydra-emacs/body "emacs" :exit t)
  ("p" hydra-projects/body "projects" :exit t)
  ("f" hydra-files/body "files" :exit t))

(use-package avy :ensure t
  :bind (:map evil-normal-state-map
    ("s" . avy-goto-char-2)))

(use-package dtrt-indent :ensure t
  :config
  (dtrt-indent-mode 1))

(use-package linum-relative :ensure t
  :config
  (setq linum-relative-backend 'display-line-numbers-mode)
  (linum-on)
  (linum-relative-mode)
  (helm-linum-relative-mode 1))

(use-package projectile :ensure t
  :config
  (projectile-mode 1))

(use-package helm-projectile :ensure t)
(use-package helm-ag :ensure t)

(use-package key-chord :ensure t
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state))

(use-package super-save :ensure t
  :config
  (setq auto-save-default nil)
  (super-save-mode +1))

(use-package ace-window :ensure t
  :bind
  (:map evil-normal-state-map ("å" . ace-window)))

(use-package markdown-mode :ensure t
  :commands
  (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
    ("\\.md\\'" . markdown-mode)
    ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))

(use-package evil-surround :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package php-mode :ensure t
  :init
  (require 'php-mode))

