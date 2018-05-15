(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(auto-completion
     c-c++
     csv
     emacs-lisp
     evil-snipe
     git
     go
     gtags
     helm
     html
     javascript
     latex
     markdown
     org
     php
     shell-scripts
     syntax-checking
     version-control
     yaml
     )
   dotspacemacs-additional-packages '(evil-mc)
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '((recents . 10)
                                (projects . 3))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(base16-default-dark spacemacs-dark)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("xos4 Terminus"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ nil
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-switch-to-buffer-prefers-purpose nil
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers 'relative
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("rg" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  (setq base16-theme-use-shell-colors t)
  (setq base16-theme-256-color-source "colors")
)

(defun dotspacemacs/user-config ()
  (setq scroll-margin 5)
  (setq powerline-default-separator 'nil)
  (setq-default evil-escape-key-sequence "jk")
  (setq TeX-engine 'xetex)
  (setq yas-also-auto-indent-first-line t)
  (global-auto-complete-mode)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-major-mode-off)
  (spaceline-toggle-buffer-size-off)
)
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   (quote
    (((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Zathura")
     (output-html "xdg-open"))))
 '(package-selected-packages
   (quote
    (counsel powerline spinner skewer-mode simple-httpd json-snatcher json-reformat multiple-cursors hydra parent-mode projectile request haml-mode gitignore-mode fringe-helper git-gutter+ git-gutter pos-tip pkg-info epl flx smartparens iedit anzu highlight f web-completion-data dash-functional tern go-mode bind-map yasnippet s dash auto-complete popup let-alist which-key use-package mmm-mode highlight-indentation fill-column-indicator eyebrowse expand-region evil-surround evil-magit dumb-jump clang-format bind-key auto-compile ace-window auctex evil flycheck company helm helm-core avy markdown-mode org-plus-contrib magit magit-popup git-commit ghub with-editor php-mode js2-mode yaml-mode ws-butler winum web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen undo-tree toc-org tagedit spaceline smeargle slim-mode scss-mode sass-mode restart-emacs rainbow-delimiters pug-mode popwin phpunit phpcbf php-extras php-auto-yasnippets persp-mode pcre2el paradox packed orgit org-bullets open-junk-file neotree move-text markdown-toc magit-gitflow macrostep lorem-ipsum livid-mode linum-relative link-hint less-css-mode json-mode js2-refactor js-doc insert-shebang info+ indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gtags helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag goto-chg google-translate golden-ratio go-guru go-eldoc gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md ggtags fuzzy flycheck-pos-tip flx-ido fish-mode fancy-battery exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-snipe evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu emmet-mode elisp-slime-nav drupal-mode disaster diminish diff-hl define-word csv-mode company-web company-tern company-statistics company-shell company-go company-c-headers company-auctex column-enforce-mode coffee-mode cmake-mode clean-aindent-mode base16-theme auto-yasnippet auto-highlight-symbol async aggressive-indent ag adaptive-wrap ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   (quote
    (((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Zathura")
     (output-html "xdg-open"))))
 '(package-selected-packages
   (quote
    (powerline spinner skewer-mode simple-httpd json-snatcher json-reformat multiple-cursors hydra parent-mode projectile request haml-mode gitignore-mode fringe-helper git-gutter+ git-gutter pos-tip pkg-info epl flx smartparens iedit anzu highlight f web-completion-data dash-functional tern go-mode bind-map yasnippet s dash auto-complete popup let-alist which-key use-package mmm-mode highlight-indentation fill-column-indicator eyebrowse expand-region evil-surround evil-magit dumb-jump clang-format bind-key auto-compile ace-window auctex evil flycheck company helm helm-core avy markdown-mode org-plus-contrib magit magit-popup git-commit ghub with-editor php-mode js2-mode yaml-mode ws-butler winum web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen undo-tree toc-org tagedit spaceline smeargle slim-mode scss-mode sass-mode restart-emacs rainbow-delimiters pug-mode popwin phpunit phpcbf php-extras php-auto-yasnippets persp-mode pcre2el paradox packed orgit org-bullets open-junk-file neotree move-text markdown-toc magit-gitflow macrostep lorem-ipsum livid-mode linum-relative link-hint less-css-mode json-mode js2-refactor js-doc insert-shebang info+ indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gtags helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag goto-chg google-translate golden-ratio go-guru go-eldoc gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md ggtags fuzzy flycheck-pos-tip flx-ido fish-mode fancy-battery exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-snipe evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu emmet-mode elisp-slime-nav drupal-mode disaster diminish diff-hl define-word csv-mode company-web company-tern company-statistics company-shell company-go company-c-headers company-auctex column-enforce-mode coffee-mode cmake-mode clean-aindent-mode base16-theme auto-yasnippet auto-highlight-symbol async aggressive-indent ag adaptive-wrap ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
