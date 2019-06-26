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
(setq use-package-always-demand t)

(straight-use-package 'git)

 (defun straight-x-clean-unused-repos ()
   (interactive)
   (dolist (repo (straight--directory-files (straight--repos-dir)))
     (unless (or (straight--checkhash repo straight--repo-cache)
		 (not (y-or-n-p (format "Delete repository %S? " repo))))
       (delete-directory (straight--repos-dir repo) 'recursive 'trash))))

(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))
      backup-directory-alist `(("." . "~/.emacs.d/backups"))
      byte-compile-warnings nil
      custom-file "~/.emacs.d/custom.el"
      delete-old-versions -1
      inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-screen t
      make-backup-files nil
      require-final-newline t
      ring-bell-function 'ignore
      sentence-end-double-space nil
      vc-follow-symlinks t
      version-control t
      x-stretch-cursor t)

(setq-default show-trailing-whitespace t
              indent-tabs-mode nil
              tab-width 4
              auto-fill-function 'do-auto-fill
              mode-line-format nil)

(bind-key (kbd "<escape>") 'keyboard-escape-quit)
(defalias 'yes-or-no-p 'y-or-n-p)
(defun display-startup-echo-area-message nil)
(load custom-file)
(set-language-environment "UTF-8")
(put 'dired-find-alternate-file 'disabled nil)

(electric-indent-mode 1)
(electric-pair-mode 1)
(menu-bar-mode -1)
(recentf-mode 1)
(show-paren-mode 1)
(global-auto-revert-mode t)
(fringe-mode 0)

(add-hook 'prog-mode-hook (lambda () (auto-fill-mode 1)))
(add-to-list 'recentf-exclude "/vendor/")
(add-to-list 'recentf-exclude "/sudo")

(defun switch-to-previous-buffer ()
  "Switch to the most recently used buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun hide-gui-elements (&optional frame)
  "Hides some GUI elements in FRAME."
  (unless frame
    (setq frame (selected-frame)))
  (when frame
    (with-selected-frame frame
      (when window-system
        (scroll-bar-mode -1)
        (tool-bar-mode -1)))))

(hide-gui-elements)
(add-hook 'after-make-frame-functions #'hide-gui-elements t)
(add-hook 'with-editor-mode-hook 'evil-insert-state)

(use-package general)

(use-package evil
  :after general
  :config
  (evil-mode t)
  (evil-select-search-module 'evil-search-module 'evil-search)

  (advice-add 'evil-ex-search-next :after
    (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos))))
  (advice-add 'evil-ex-search-previous :after
    (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos))))

  (with-eval-after-load 'dired
    (general-def
      :states 'normal
      :keymaps 'dired-mode-map
      "h" 'dired-up-directory
      "s" 'dired-find-alternate-file
      "t" 'evil-next-line
      "n" 'evil-previous-line))

  :general
  (:states '(normal visual)
    "t" 'evil-next-line
    "n" 'evil-previous-line
    "h" 'evil-backward-char
    "s" 'evil-forward-char
    "k" 'evil-ex-search-next
    "K" 'evil-ex-search-previous
    "C-n" 'evil-scroll-up
    "C-t" 'evil-scroll-down)
  (:states 'motion
    "l" 'evil-find-char-to))

(use-package helm
  :config
  (helm-mode 1)
  (defvar helm-M-x-fuzzy-match)
  (defvar helm-buffers-fuzzy-matching)
  (defvar helm-recentf-fuzzy-match)
  (defvar helm-semantic-fuzzy-match)
  (defvar helm-imenu-fuzzy-match)
  (defvar helm-apropos-fuzzy-match)
  (defvar helm-lisp-fuzzy-completion)

  :general
  ("M-x" 'helm-M-x)
  (:keymaps 'helm-map
    "TAB" 'helm-execute-persistent-action
    "C-t" 'helm-next-line
    "C-n" 'helm-previous-line))

(use-package base16-theme
  :config
  (defconst base16-theme-256-color-source "colors")
  (load-theme 'base16-chalk t))

(use-package hydra
  :config
  (setq hydra-cell-format "% -0s %% -8`%s")

  :general
  (:states '(normal visual)
    "SPC" 'hydra-menu/body
    "C-m" 'hydra-movement/body))

(defun hydra-major/body ()
  (interactive)
  (cl-case major-mode
    (org-mode (hydra-org/body))
    (latex-mode (hydra-tex/body))
    (snippet-mode (hydra-yasnippet/body))
    (t (error "%S not supported" major-mode))))

(defhydra hydra-buffers (:color blue)
  ("l" helm-mini "list")
  ("s" helm-do-ag-buffers "search")
  ("a" save-buffer "save")
  ("s" switch-to-previous-buffer "previous buffer")
  ("d" (kill-buffer (current-buffer)) "destroy")
  ("u" sudo-edit "open as sudo"))

(defhydra hydra-emacs (:color blue)
  ("r" (load-file "~/.emacs.d/init.el") "reload")
  ("l" list-packages "list packages")
  ("p" straight-x-clean-unused-repos "prune unused packages")
  ("u" auto-package-update-now "update packages")
  ("e" save-buffers-kill-terminal "save and exit"))

(defhydra hydra-files (:color blue)
  ("p" projectile-dired "project root")
  ("d" dired-jump "dired")
  ("f" helm-find-files "find")
  ("r" helm-recentf "recent"))

(defhydra hydra-projects (:color blue)
  ("w" helm-projectile-switch-project "switch")
  ("s" helm-do-ag-project-root "search")
  ("r" projectile-replace "replace")
  ("f" helm-projectile-find-file "files")
  ("F" helm-projectile-recentf "recent files")
  ("d" projectile-discover-projects-in-directory "discover")
  ("k" projectile-kill-buffers "kill buffers"))

(defhydra hydra-tex (:color blue)
  ("b" (lambda () (interactive) (save-buffer) (TeX-command "LaTeX" 'TeX-master-file)) "build")
  ("v" (lambda () (interactive) (save-buffer) (TeX-command-run-all ())) "build and view")
  ("i" latex-insert-item "insert item")
  ("l" latex-insert-block "insert block"))

(defhydra hydra-org (:color blue)
  ("t" org-todo "toggle todo status")
  ("i" org-clock-in "clock in")
  ("o" org-clock-out "clock out")
  ("d" org-update-all-dblocks "update dblocks"))

(defhydra hydra-window (:color blue)
  ("b" split-window-below "split below")
  ("r" split-window-right "split right")
  ("d" ace-delete-window "delete other")
  ("x" delete-window "delete current")
  ("a" ace-window "ace"))

(defhydra hydra-flycheck (:color red)
  ("n" flycheck-next-error "next error")
  ("p" flycheck-previous-error "previous error"))

(defhydra hydra-lsp (:color blue)
  ("r" lsp-find-references "references")
  ("d" lsp-find-definition "definition")
  ("e" lsp-describe-thing-at-point "describe"))

(defhydra hydra-snippet (:color blue)
  ("i" yas-insert-snippet "insert")
  ("c" yas-new-snippet "create")
  ("l" yas-describe-tables "list"))

(defhydra hydra-yasnippet (:color blue)
  ("s" yas-load-snippet-buffer-and-close "save and load"))

(defhydra hydra-movement (:color blue)
  ("m" evil-avy-goto-char-timer "timer")
  ("l" evil-avy-goto-line "line")
  ("c" evil-avy-goto-char-in-line "char in line")
  ("p" avy-pop-mark "avy pop mark"))

(defhydra hydra-menu (:color blue)
  ("b" hydra-buffers/body "buffers")
  ("e" hydra-emacs/body "emacs")
  ("p" hydra-projects/body "projects")
  ("f" hydra-files/body "files")
  ("w" hydra-window/body "windows")
  ("s" hydra-snippet/body "snippets")
  ("j" hydra-major/body "major")
  ("y" hydra-flycheck/body "flycheck")
  ("l" hydra-lsp/body "lsp")
  ("m" hydra-movement/body "lsp")
  ("r" switch-to-previous-buffer "previous buffer")
  ("u" helm-resume "helm resume")
  ("a" ace-window "ace"))

(use-package avy
  :config
  (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s ?c ?p ?k ?m)
        avy-timeout-seconds '0.3
        avy-all-windows nil)

  (general-unbind '(normal motion) "m")

  :general
  (:states '(motion normal)
   :prefix "m"
    "m" 'evil-avy-goto-char-timer
    "c" 'evil-avy-goto-char-in-line
    "l" 'evil-avy-goto-line))

(use-package dtrt-indent
  :config
  (setq dtrt-indent-verbosity 0)
  :hook
  (prog-mode . dtrt-indent-mode))

(use-package projectile
  :config
  (projectile-mode 1)
  (setq projectile-sort-order 'recently-active
        projectile-generic-command "fd . -0"))

(use-package helm-projectile
  :after helm projectile)

(use-package helm-ag
  :after helm
  :general
  (:states 'normal
   :keymaps 'helm-ag-edit-map
    "c" 'helm-ag--edit-commit
    "q" 'helm-ag--edit-abort)

  (:keymaps 'helm-ag-map
    "C-e" 'helm-ag-edit))

(use-package key-chord
  :after evil
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "hh" 'evil-normal-state))

(use-package super-save
  :config
  (setq auto-save-default nil)
  (super-save-mode +1))

(use-package ace-window
  :config
  (setq aw-keys '(?h ?a ?s ?p)))

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
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-args
  :after evil
  :general
  (:states '(normal motion)
    "T" 'evil-forward-arg
    "N" 'evil-backward-arg
    "H" 'evil-jump-out-args)
  (:keymaps '(evil-inner-text-objects-map evil-outer-text-objects-map)
    "a" 'evil-inner-arg
    "a" 'evil-outer-arg))

(use-package php-mode
  :hook
  (php . php-enable-symfony2-coding-style))

(use-package python-mode
  :init
  (require 'python-mode))

(use-package org
  :after evil
  :config
  (setq org-duration-format (quote h:mm)
        org-todo-keywords '((sequence "TODO" "STARTED" "PENDING" "DONE")))
  :general
  (:keymaps 'org-mode-map :states 'normal
    "N" 'org-timestamp-up
    "T" 'org-timestamp-down
    "S" 'org-clock-timestamps-up
    "H" 'org-clock-timestamps-down))

(use-package company
  :config
  (setq company-dabbrev-downcase 0
        company-show-numbers t
        company-idle-delay 0
        company-selection-wrap-around t
        company-dabbrev-char-regexp "[A-z:-]"
        company-minimum-prefix-length 3)
  (delete 'company-dabbrev company-backends)

  :hook
  (prog-mode . global-company-mode)

  :general
  (:keymaps 'company-active-map
    "C-t" 'company-select-next
    "C-n" 'company-select-previous))

(use-package tex
  :demand t
  :straight auctex
  :config
  (add-to-list 'TeX-view-program-list '("mupdf" ("mupdf" " %o" (mode-io-correlate " %(outpage)"))))
  (setcdr (assq 'output-pdf TeX-view-program-selection) '("mupdf"))
  (TeX-PDF-mode t)
  (setq-default TeX-master nil)
  (setq TeX-auto-save t
        TeX-parse-self t))

(defun mupdf-reload (file)
  "Sends SIGHUP to mupdf, reloading the output."
  (interactive)
  (TeX-revert-document-buffer file)
  (call-process-shell-command "pkill -HUP mupdf || true"))
(add-hook 'TeX-after-compilation-finished-functions #'mupdf-reload)

(use-package whitespace
  :config
  (setq whitespace-line-column 80
        whitespace-style '(face lines-tail))
  :hook
  (prog-mode . whitespace-mode))

(use-package yaml-mode
  :config
  :mode "\\.yml\\'")

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (setq js2-strict-missing-semi-warning nil))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package go-mode
  :mode "\\.go\\'")

(use-package web-mode
  :mode "\\.vue\\'")

(use-package flycheck
  :config
  (setq flycheck-indication-mode nil)
  (global-flycheck-mode))

(use-package expand-region
  :after evil
  :general
  (:states '(normal visual)
    "+" 'er/expand-region
    "-" 'er/contract-region))

(use-package shackle
  :config
  (setq shackle-default-alignment 'below
        shackle-default-size 0.3
        helm-display-function 'pop-to-buffer
        shackle-default-rule '(:select t :align t :other t)
        shackle-rules
          '((compilation-mode :align t :noselect t)
           (neotree-mode :align left)
           ("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.3)
           ("*Flycheck errors*" :regexp t :align t :size 0.3 :select t)))
  (shackle-mode 1))

(use-package git-commit)
(use-package git-modes)

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t))

(use-package lsp-mode
  :hook
  (c++-mode . lsp)
  (js2-mode . lsp))

(use-package company-lsp
  :after lsp-mode
  :config
  (push 'company-lsp company-backends)
  (setq company-lsp-cache-candidates t
        company-lsp-async t))

(use-package ebuild-mode
  :mode "\\.ebuild\\'")

(use-package sudo-edit
  :commands sudo-edit)

(provide 'init)
