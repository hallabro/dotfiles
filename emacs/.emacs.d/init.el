;bootstrap straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(require 'subr-x)
(straight-use-package 'git)

;workaround for installing org-mode with straight
(defun org-git-version ()
  (require 'git)
  (let ((git-repo (expand-file-name "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  (require 'git)
  (let ((git-repo (expand-file-name "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)

(setq delete-old-versions -1)
(setq version-control t)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(setq vc-follow-symlinks t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(set-language-environment "UTF-8")
(setq sentence-end-double-space nil)
(setq default-fill-column 80)
(setq byte-compile-warnings nil)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq c-syntactic-indentation nil)
(setq require-final-newline t)
(setq show-trailing-whitespace t)
(defun display-startup-echo-area-message nil)
(global-display-line-numbers-mode)
(recentf-mode 1)
(setq recentf-max-menu-items 20)
(bind-key (kbd "<escape>") 'keyboard-escape-quit)
(setq-default mode-line-format nil)
(menu-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)

(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun hide-gui-elements (&optional frame)
  "Hides some GUI elements."
  (unless frame
    (setq frame (selected-frame)))
  (when frame
    (with-selected-frame frame
      (when window-system
        (scroll-bar-mode -1)
        (tool-bar-mode -1)))))

(hide-gui-elements)
(add-hook 'after-make-frame-functions #'hide-gui-elements t)

(use-package evil
  :config
  (evil-mode t)
  (evil-select-search-module 'evil-search-module 'evil-search)
  (define-key evil-normal-state-map (kbd "ä") 'switch-to-last-buffer)
  (define-key evil-normal-state-map "s" nil)
  (define-key evil-normal-state-map "J" nil)
  (advice-add 'evil-ex-search-next :after
    (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos))))
  (advice-add 'evil-ex-search-previous :after
    (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos))))

  (define-key evil-normal-state-map (kbd "C-n") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-t") 'evil-scroll-down)

  (evil-define-key nil evil-motion-state-map
    "t" 'evil-next-line
    "n" 'evil-previous-line
    "h" 'evil-backward-char
    "s" 'evil-forward-char
    "j" 'evil-ex-search-next
    "J" 'evil-ex-search-previous)

  (evil-define-key 'visual evil-motion-state-map
    "t" 'evil-next-line
    "n" 'evil-previous-line
    "h" 'evil-backward-char
    "s" 'evil-forward-char
    "j" 'evil-ex-search-next
    "J" 'evil-ex-search-previous))

(use-package helm
  :config
  (require 'helm-config)
  (setq helm-mode-fuzzy-match t)

  (helm-mode 1)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
  :bind (:map helm-map
    ("C-t" . 'helm-next-line)
    ("C-n" . 'helm-previous-line)))

(use-package base16-theme
  :config
  (setq base16-theme-256-color-source "colors")
  (load-theme 'base16-chalk t))

(use-package hydra :defer t
  :bind (:map evil-normal-state-map
      ("SPC" . hydra-menu/body)
  	("m" . hydra-major/body))
  :config
  (setq hydra-cell-format "% -0s %% -8`%s"))

(defun hydra-major/body ()
  (interactive)
  (cl-case major-mode
    (org-mode (hydra-org/body))
    (latex-mode (hydra-tex/body))
    (t (error "%S not supported" major-mode))))

(defhydra hydra-buffers (:color blue)
  ("l" helm-mini "list")
  ("s" helm-do-ag-buffers "search")
  ("a" save-buffer "save")
  ("d" (kill-buffer (current-buffer)) "destroy"))

(defhydra hydra-emacs (:color blue)
  ("r" (load-file "~/.emacs.d/init.el") "reload")
  ("l" (list-packages) "list packages")
  ("q" save-buffers-kill-terminal "save and quit"))

(defhydra hydra-files (:color blue)
  ("r" helm-recentf "recent"))

(defhydra hydra-projects (:color blue)
  ("w" helm-projectile-switch-project "switch")
  ("s" helm-do-ag-project-root "search")
  ("f" helm-projectile-find-file "files")
  ("d" projectile-discover-projects-in-directory "discover"))

(defhydra hydra-tex (:color blue)
  ("b" (lambda () (interactive) (save-buffer) (TeX-command "LaTeX" 'TeX-master-file)) "build")
  ("v" (lambda () (interactive) (save-buffer) (TeX-command-run-all ())) "build and view"))

(defhydra hydra-org (:color blue)
  ("t" org-todo "toggle todo status")
  ("i" org-clock-in "clock in")
  ("o" org-clock-out "clock out")
  ("u" org-timestamp-up "timestamp up")
  ("d" org-timestamp-down "timestamp down")
  ("d" org-update-all-dblocks "update dblocks"))

(defhydra hydra-navigation (:color blue)
  ("t" neotree-toggle "toggle"))

(defhydra hydra-menu (:color blue)
  ("b" hydra-buffers/body "buffer" :exit t)
  ("e" hydra-emacs/body "emacs" :exit t)
  ("p" hydra-projects/body "projects" :exit t)
  ("f" hydra-files/body "files" :exit t)
  ("n" hydra-navigation/body "navigation" :exit t)
  ("m" hydra-major/body "major" :exit t))

(use-package avy
  :config
  (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))
  :bind (:map evil-normal-state-map
    ("å" . avy-goto-char-2)))

(use-package dtrt-indent
  :config
  (setq dtrt-indent-verbosity 0)
  :hook
  (prog-mode . dtrt-indent-mode))
  
(use-package linum-relative
  :config
  (setq linum-relative-backend 'display-line-numbers-mode)
  (linum-on)
  (linum-relative-mode)
  (helm-linum-relative-mode 1))

(use-package projectile
  :config
  (projectile-mode 1))
  (setq projectile-sort-order 'recently-active)
  (setq projectile-generic-command "fd . -0")

(use-package helm-projectile )
(use-package helm-ag )

(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "hh" 'evil-normal-state))

(use-package super-save
  :config
  (setq auto-save-default nil)
  (super-save-mode +1))

(use-package ace-window
  :bind
  (:map evil-normal-state-map ("ö" . ace-window)))

(use-package markdown-mode
  :commands
  (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
    ("\\.md\\'" . markdown-mode)
    ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package php-mode
  :init
  (require 'php-mode))

(use-package python-mode
  :init
  (require 'python-mode))

(use-package org
  :config
  (setq org-duration-format (quote h:mm))
  (setq org-todo-keywords '((sequence "TODO" "STARTED" "PENDING" "DONE")))
  (evil-define-key 'normal org-mode-map
    "N" 'org-timestamp-up
    "T" 'org-timestamp-down
    "S" 'org-clock-timestamps-up
    "H" 'org-clock-timestamps-down))

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0))

(use-package neotree
  :config
  (setq neo-window-width 40)
  (setq neo-cwd-line-style 'button)
  (setq neo-autorefresh nil)
  (evil-define-key 'normal neotree-mode-map
    "q" 'neotree-hide
    "o" 'neotree-quick-look
    "u" 'neotree-refresh
    "." 'neotree-hidden-file-toggle
    "c" 'neotree-create-node
    "r" 'neotree-rename-node
    "d" 'neotree-delete-node
    "h" 'neotree-select-up-node
    "s" 'neotree-enter
    "t" 'neotree-next-line
    "n" 'neotree-previous-line
    "a" 'neotree-change-root))

(use-package tex
  :demand t
  :straight auctex
  :config
  (add-to-list 'TeX-view-program-list '("mupdf" ("mupdf" " %o" (mode-io-correlate " %(outpage)"))))
  (setcdr (assq 'output-pdf TeX-view-program-selection) '("mupdf"))
  (TeX-PDF-mode t)
  (setq-default TeX-master nil)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t))

(defun mupdf-reload (file)
  (interactive)
  (TeX-revert-document-buffer file)
  (call-process-shell-command "pkill -HUP mupdf || true"))
(add-hook 'TeX-after-compilation-finished-functions #'mupdf-reload)

(use-package whitespace
  :config
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face lines-tail))
  :hook
  (prog-mode . whitespace-mode))

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))
