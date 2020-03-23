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

(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir t)))))

(setq auto-save-file-name-transforms `((".*", temporary-file-directory t))
      backup-directory-alist `((".*" ., temporary-file-directory))
      byte-compile-warnings nil
      create-lockfiles nil
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
              sp-escape-quotes-after-insert nil
              ispell-dictionary "en"
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
(fringe-mode '(2 . 0))

(setq mouse-yank-at-point t)
(setq window-divider-default-places t
      window-divider-default-bottom-width 2
      window-divider-default-right-width 2)
(window-divider-mode 1)
(setq-default left-margin-width 1
              right-margin-width 1)
(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
(setq x-gtk-use-system-tooltips nil)
(setq split-width-threshold 160
      split-height-threshold nil)
(setq echo-keystrokes 0.02)
(setq-default display-line-numbers-width 3)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

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
    :states '(normal visual)
    "b" '(:ignore t :which-key "buffer")
    "bl" '(ivy-switch-buffer :which-key "list")
    "bs" '(save-buffer :which-key "save")
    "bS" '(save-some-buffers t :which-key "save all")
    "bd" '((lambda () (interactive) (kill-buffer (current-buffer))) :which-key "kill")
    "bu" '(sudo-edit :which-key "sudo")
    "bF" '(text-scale-adjust :which-key "adjust font size")

    "t" '(:ignore t :which-key "toggle")
    "ta" '(auto-fill-mode :which-key "auto-fill")
    "tw" '(whitespace-mode :which-key "whitespace visibility")

    "q" '(:ignore t :which-key "quit")
    "qr" '((lambda () (interactive) (load-file "~/.emacs.d/init.el")) :which-key "reload configuration")
    "qp" '(straight-x-clean-unused-repos :which-key "prune unused packages")
    "qu" '(straight-pull-all :which-key "update packages")
    "qe" '(save-buffers-kill-terminal :which-key "save and exit")

    "f" '(:ignore t :which-key "file")
    "fd" '(dired-jump :which-key "browse")
    "fD" '((lambda () (interactive) (dired-jump nil "~/")) :which-key "browse home")
    "ff" '(counsel-file-jump :which-key "find")
    "fr" '(counsel-recentf :which-key "recent")

    "p" '(:ignore t :which-key "project")
    "pd" '(projectile-dired :which-key "browse")
    "pw" '(counsel-projectile-switch-project :which-key "switch")
    "ps" '(counsel-projectile-ag :which-key "search")
    "pr" '(projectile-replace :which-key "replace")
    "pf" '(counsel-projectile-find-file :which-key "find file")
    "pD" '(projectile-discover-projects-in-directory :which-key "discover")
    "pb" '(counsel-projectile-switch-to-buffer :which-key "buffers")
    "pc" '(org-capture :which-key "capture")
    "po" '(org-projectile-project-todo-completing-read :which-key "org read")

    "w" '(:ignore t :which-key "window")
    "wb" '(split-and-focus-vertical :which-key "split below")
    "wr" '(split-and-focus-horizontal :which-key "split right")
    "wo" '(ace-delete-window :which-key "kill other")
    "wc" '(delete-window :which-key "kill current")

    "e" '(:ignore t :which-key "errors")
    "ed" '(ispell-change-dictionary :which-key "set spell check dictionary")
    "el" '(counsel-flycheck :which-key "list")
    "en" '(next-error :which-key "next")
    "ep" '(previous-error :which-key "previous")
    "es" '(ispell :which-key "spell check")

    "s" '(:ignore t :which-key "snippet")
    "si" '(yas-insert-snippet :which-key "insert")
    "sc" '(yas-new-snippet :which-key "create")
    "sl" '(yas-describe-tables :which-key "list")

    "h" '(:ignore t :which-key "help")
    "hb" '(counsel-descbinds :which-key "describe keybind")
    "hv" '(describe-variable :which-key "describe variable")

    "k" '(counsel-yank-pop :which-key "kill ring")
    "g" '(counsel-register :which-key "registers")
    "r" '(previous-buffer :which-key "previous buffer")
    "u" '(ivy-resume :which-key "ivy resume")
    "SPC" '(ace-window :which-key "ace window"))

  (general-define-key
    :prefix "SPC"
    :keymaps 'org-mode-map
    :states 'normal
    "m" '(:ignore t :which-key "major")
    "mi" '(org-insert-todo-heading-respect-content :which-key "insert todo")
    "ms" '(org-todo :which-key "rotate status")
    "mc" '(org-clock-in :which-key "clock in")
    "mC" '(org-clock-out :which-key "clock out")
    "mf" '(org-open-at-point :which-key "follow link")
    "md" '(org-update-all-dblocks :which-key "update dynamic blocks"))

  (general-define-key
    :prefix "SPC"
    :keymaps 'LaTeX-mode-map
    :states 'normal
    "m" '(:ignore t :which-key "major")
    "mb" '((lambda () (interactive) (save-buffer) (TeX-command "LaTeX" 'TeX-master-file)) :which-key "build")
    "mv" '((lambda () (interactive) (save-buffer) (TeX-command-run-all ())) :which-key "build and view")
    "mi" '(latex-insert-item :which-key "insert new item")
    "ml" '(latex-insert-block :which-key "insert new block"))

  (general-define-key
    :prefix "SPC"
    :keymaps 'go-mode-map
    :states 'normal
    "m" '(:ignore t :which-key "major")
    "mf" '(gofmt :which-key "format")
    "mi" '(go-import-add :which-key "import package"))

  (general-define-key
    :prefix "SPC"
    :keymaps 'snippet-mode-map
    :states 'normal
    "m" '(:ignore t :which-key "major")
    "ms" '(yas-load-snippet-buffer-and-close :which-key "save and load snippet"))

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

  (eval-after-load "evil-maps" (define-key evil-motion-state-map "\C-w" nil))

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

(use-package avy
  :config
  (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s ?c ?p ?k ?m)
        avy-timeout-seconds '0.3
        avy-all-windows nil)

  (general-unbind '(normal motion) "m")

  :general
  (:states '(motion normal operator visual)
   :prefix "m"
    "m" '(evil-avy-goto-char-timer :which-key "char timer")
    "l" '(evil-avy-goto-line :which-key "line")
    "p" 'avy-pop-mark :which-key "previous mark"))

(use-package dtrt-indent
  :config
  (setq dtrt-indent-verbosity 0)
  :hook
  (prog-mode . dtrt-indent-mode))

(use-package projectile
  :custom
  ;; exclude vendor/ from being searched
  projectile-git-command "git ls-files -zco --exclude-standard -- . ':!:vendor/*'"
  :config
  (projectile-mode 1)
  (setq projectile-sort-order 'recently-active
        projectile-generic-command "fd . -0")

  (general-define-key
    :prefix "SPC"
    :keymaps 'markdown-mode-map
    :states 'normal
    "m" '(:ignore t :which-key "major")
    "mp" '(markdown-preview :which-key "preview")))

(use-package key-chord
  :after evil
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "hh" 'evil-normal-state))

(use-package super-save
  :config
  (add-to-list 'super-save-triggers 'ace-window 'ivy-switch-buffer)
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
  (setq markdown-command "markdown_py -x tables"))

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
        TeX-engine 'luatex
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
  :mode "\\.go\\'"
  :config
  (add-hook 'go-mode-hook 'lsp-deferred))

(use-package web-mode
  :config (setq web-mode-enable-comment-annotation t)
  :mode "\\.vue\\'")

(use-package flycheck
  :config
  (setq flycheck-indication-mode nil
        flycheck-display-errors-function 'ignore)
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
        shackle-default-rule '(:select t :align t :other t)
        shackle-rules
          '((compilation-mode :align t :noselect t)
           ("*Flycheck errors*" :align t :size 0.3 :select t)
           ("*Warning*" :ignore t)
           ("*Warnings*" :ignore t)))
  (shackle-mode 1))

(use-package git-commit)
(use-package git-modes)

(use-package lsp-mode
  :config
  (add-to-list 'lsp-file-watch-ignored "vendor")
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
        which-key-prefix-prefix nil)
        ;;which-key-allow-evil-operators t)
  (which-key-mode))

(use-package smartparens
  :config
  (require 'smartparens-config)
  ;; Overlays are too distracting and not terribly helpful. show-parens does
  ;; this for us already (and is faster), so...
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  (setq sp-max-pair-length 4)
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

(use-package evil-replace-with-register
  :general
  (:states 'normal
     "mr" '(evil-replace-with-register :which-key "replace with register")))

(use-package dired-narrow
  :general
  (:keymaps 'dired-mode-map
    "/" 'dired-narrow
    "c" 'find-file))

(use-package highlight-parentheses)

(use-package ivy
  :config
  (ivy-mode 1)
  (projectile-completion-system 'ivy)
  (ivy-count-format "(%d/%d) ")
  (setq ivy-use-virtual-buffers t
        ;; Don't use ^ as initial input
        ivy-initial-inputs-alist nil
        ivy-display-style 'fancy
        enable-recursive-minibuffers t
        ivy-re-builders-alist
          '((counsel-ag . ivy--regex-plus)
            (counsel-projectile-find-file . ivy--regex-plus)
            (swiper . ivy--regex-plus)
            (t . ivy--regex-fuzzy)))
  :general
  (:keymaps '(ivy-minibuffer-map ivy-switch-buffer-map)
    "C-w" 'ivy-backward-kill-word
    "C-n" 'ivy-previous-line
    "C-t" 'ivy-next-line
    "C-s" 'ivy-alt-done))

(use-package ivy-rich
  :after ivy
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (add-hook 'minibuffer-setup-hook #'(lambda () (setq show-trailing-whitespace nil)))
  (ivy-rich-mode 1))

(use-package ivy-prescient
  :after counsel
  :config
  (setq ivy-prescient-sort-commands '(:not ivy-resumse))
  (ivy-prescient-mode 1))

(use-package counsel
  :config
  (counsel-mode 1)
  (setq counsel-ag-command "ag --nocolor --nogroup --hidden %s"
        counsel-ag-base-command "ag --nocolor --nogroup --hidden %s"))

(use-package swiper
  :config (setq swiper-action-recenter t)
  :general (:states 'normal "/" 'swiper))

(use-package counsel-projectile
  :config (setq counsel-projectile-ag-use-gitignore-only nil))

(use-package org-projectile
  :config
  (progn (setq org-projectile-projects-file "~/projects/notes/TODO.org")
         (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
         (push (org-projectile-project-todo-entry) org-capture-templates)))

(use-package beacon
  :config (beacon-mode 1))

(use-package flyspell
  :hook
  (prog-mode . flyspell-prog-mode)
  (text-mode . flyspell-mode))

(use-package undo-tree
  :config
  (setq undo-tree-visualizer-diff t
        undo-tree-auto-save-history t
        undo-tree-enable-undo-in-region t
        ;; Increase undo-limits by a factor of ten to avoid emacs prematurely
        ;; truncating the undo history and corrupting the tree. See
        ;; https://github.com/syl20bnr/spacemacs/issues/12110
        undo-limit 800000
        undo-strong-limit 12000000
        undo-outer-limit 120000000
        undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package diff-hl
  :config (global-diff-hl-mode 1))

(use-package doom-themes
  :config
  (load-theme 'doom-dracula t))

(use-package paren
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

(use-package terraform-mode)

(provide 'init)
