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
(setq straight-use-package-by-default t
      use-package-always-demand t)

(straight-use-package 'git)

 (defun straight-x-clean-unused-repos ()
   (interactive)
   (dolist (repo (straight--directory-files (straight--repos-dir)))
     (unless (or (straight--checkhash repo straight--repo-cache)
		 (not (y-or-n-p (format "Delete repository %S? " repo))))
       (delete-directory (straight--repos-dir repo) 'recursive 'trash))))

(setq auto-save-file-name-transforms `((".*", temporary-file-directory t))
      backup-directory-alist `((".*" ., temporary-file-directory))
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
(menu-bar-mode -1)
(recentf-mode 1)
(show-paren-mode 1)
(global-auto-revert-mode t)
(fringe-mode 0)

(add-hook 'prog-mode-hook (lambda () (auto-fill-mode 1)))
(add-to-list 'recentf-exclude "/vendor/")
(add-to-list 'recentf-exclude "/sudo")

(defun previous-buffer ()
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

(defun split-and-focus-vertical ()
  "Splits window and switch focus."
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun split-and-focus-horizontal ()
  "Splits window and switch focus."
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(use-package general
  :config
  (general-define-key
    :prefix "SPC"
    :keymaps 'override
    :states 'normal
    "b" '(:ignore t :which-key "buffers")
    "bl" '(helm-mini :which-key "list")
    "bs" '(helm-do-ag-buffers :which-key "ag")
    "ba" '(save-buffer :which-key "save")
    "bd" '((lambda () (interactive) (kill-buffer (current-buffer))) :which-key "close")
    "bu" '(sudo-edit :which-key "sudo edit")
    "bf" '(auto-fill-mode :which-key "auto-fill")

    "e" '(:ignore t :which-key "emacs")
    "er" '((lambda () (interactive) (load-file "~/.emacs.d/init.el")) :which-key "reload")
    "el" '(list-packages :which-key "list packages")
    "ep" '(straight-x-clean-unused-repos :which-key "prune unused packages")
    "eu" '(straight-pull-all :which-key "update packages")
    "ee" '(save-buffers-kill-terminal :which-key "save and exit")

    "f" '(:ignore t :which-key "files")
    "fd" '(dired-jump :which-key "dired")
    "ff" '(helm-find-files :which-key "find")
    "fr" '(helm-recentf :which-key "recent")

    "p" '(:ignore t :which-key "projects")
    "pd" '(projectile-dired :which-key "dired")
    "pw" '(helm-projectile-switch-project :which-key "switch")
    "ps" '(helm-do-ag-project-root :which-key "ag")
    "pr" '(projectile-replace :which-key "search and replace")
    "pf" '(helm-projectile-find-file :which-key "find file")
    "pF" '(helm-projectile-recentf :which-key "recent files")
    "pD" '(projectile-discover-projects-in-directory :which-key "discover")
    "pk" '(projectile-kill-buffers :which-key "kill buffers")

    "w" '(:ignore t :which-key "windows")
    "wb" '(split-and-focus-vertical :which-key "split below")
    "wr" '(split-and-focus-horizontal :which-key "split right")
    "wd" '(ace-delete-window :which-key "ace delete")
    "wx" '(delete-window :which-key "close")

    "y" '(:ignore t :which-key "fly")
    "yn" '(flycheck-next-error :which-key "next")
    "yp" '(flycheck-previous-error :which-key "previous")
    "yd" '(ispell-change-dictionary :which-key "set dictionary")
    "ys" '(flyspell-correct-wrapper :which-key "helm flyspell")
    "yc" '(helm-flycheck :which-key "helm flycheck")

    "s" '(:ignore t :which-key "snippets")
    "si" '(yas-insert-snippet :which-key "insert")
    "sc" '(yas-new-snippet :which-key "create")
    "sl" '(yas-describe-tables :which-key "list")

    "k" '(helm-show-kill-ring :which-key "kill-ring")
    "g" '(helm-register :which-key "registers")
    "r" '(previous-buffer :which-key "previous buffer")
    "u" '(helm-resume :which-key "helm resume")
    "a" '(ace-window :which-key "ace window"))

  (general-define-key
    :prefix "SPC"
    :keymaps 'org-mode-map
    :states 'normal
    "m" '(:ignore t :which-key "major")
    "mt" 'org-todo
    "mi" 'org-clock-in
    "mo" 'org-clock-out
    "md" 'org-update-all-dblocks)

  (general-define-key
    :prefix "SPC"
    :keymaps 'LaTeX-mode-map
    :states 'normal
    "m" '(:ignore t :which-key "major")
    "mb" '((lambda () (interactive) (save-buffer) (TeX-command "LaTeX" 'TeX-master-file)) :which-key "build")
    "mv" '((lambda () (interactive) (save-buffer) (TeX-command-run-all ())) :which-key "build and view")
    "mi" 'latex-insert-item
    "ml" 'latex-insert-block)

  (general-define-key
    :prefix "SPC"
    :keymaps 'snippet-mode-map
    :states 'normal
    "m" '(:ignore t :which-key "major")
    "ms" 'yas-load-snippet-buffer-and-close)

  (general-define-key
    :states 'motion
    :prefix "d"
    "" '(:ignore t :which-key "evil-inner")))

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
    "C-t" 'evil-scroll-down
    "U" 'undo-tree-redo))

(use-package helm
  :config
  (setq helm-M-x-fuzzy-match t
        helm-mode-fuzzy-match t
        helm-completion-in-region-fuzzy-match t
        helm-recentf-fuzzy-match t)
  (helm-mode 1)

  :general
  ("M-x" 'helm-M-x)
  (:keymaps 'helm-map
    "TAB" 'helm-execute-persistent-action
    "C-t" 'helm-next-line
    "C-n" 'helm-previous-line))

(use-package helm-flx
  :config
  (helm-flx-mode t)
  (setq helm-flx-for-helm-locate t))

(use-package base16-theme
  :config
  (defconst base16-theme-256-color-source "colors")
  (load-theme 'base16-chalk t))

(use-package avy
  :config
  (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s ?c ?p ?k ?m)
        avy-timeout-seconds '0.3
        avy-all-windows nil)

  (general-unbind '(normal motion) "m")

  :general
  (:states '(motion normal operator visual)
   :prefix "m"
    "m" 'evil-avy-goto-char-timer
    "l" 'evil-avy-goto-line
    "p" 'avy-pop-mark))

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
  (yas-global-mode 1)
  (setq yas-indent-line 'auto
        yas-also-indent-empty-lines t))

(use-package go-mode
  :mode "\\.go\\'")

(use-package web-mode
  :config
  (setq web-mode-enable-comment-annotation t)
  :mode
  "\\.vue\\'")

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

(use-package which-key
  :config
  (setq which-key-idle-delay 0.4
        which-key-separator " "
        which-key-prefix-prefix nil
        which-key-allow-evil-operators t)
  (which-key-mode))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (sp-pair "(" nil :unless '(sp-point-before-word-p))
  (sp-pair "[" nil :unless '(sp-point-before-word-p))
  (sp-pair "{" nil :unless '(sp-point-before-word-p))
  (smartparens-global-mode))

(use-package evil-smartparens
  :hook
  (prog-mode . evil-smartparens-mode))

(use-package php-mode
  :hook
  (php . php-enable-symfony2-coding-style))

(use-package evil-snipe
  :config
  (evil-snipe-override-mode 1))

(use-package flyspell-correct-helm
  :config
  (defconst ispell-program-name "aspell")
  :hook
  (LaTex . flyspell-mode))

(use-package helm-flycheck)

(use-package evil-replace-with-register
  :general
  (:states 'normal
     "mr" 'evil-replace-with-register))

(use-package dired-narrow
  :general
  (:keymaps 'dired-mode-map
    "/" 'dired-narrow
    "c" 'find-file))

(provide 'init)
