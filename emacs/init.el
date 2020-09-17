(add-to-list 'term-file-aliases '("alacritty" . "xterm"))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(setq straight-recipes-gnu-elpa-use-mirror    t
      straight-repository-branch              "develop"
      straight-enable-use-package-integration t)

(defvar bootstrap-version)

(defconst default-snapshot-dir (expand-file-name "snapshots" user-emacs-directory))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-linum-mode t)

(setq auto-window-vscroll nil)
(global-auto-revert-mode t)
;; (menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 3)
(setq inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'text-mode
      make-backup-files nil
      scroll-error-top-bottom t
      ;; use-package-always-ensure t
      ring-bell-function 'ignore
      create-lockfiles nil
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setenv "LANG"   "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(setenv "GO111MODULE" "on")

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

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; Automatically :ensure each use-package.
;; (setq use-package-always-pin "melpa")
(use-package poet-theme :straight t)
(use-package ayu-theme :straight t)

;; (load-theme 'poet t)
;; (load-theme 'ayu-light t)
(load-theme 'modus-operandi t)

;; (straight-use-package 'deadgrep)
(use-package deadgrep :straight t :bind (("C-c C-s"  . deadgrep)))

;; (straight-use-package 'company)
(use-package company
  :straight t
  :diminish
  :config
  (global-company-mode t)
  (setq ;; Only 2 letters required for completion to activate.
        company-minimum-prefix-length 2
        ;; Search other buffers for completion candidates
        company-dabbrev-other-buffers t
        company-dabbrev-code-other-buffers t
        ;; Allow (lengthy) numbers to be eligible for completion.
        company-complete-number t
        ;; M-⟪num⟫ to select an option according to its number.
        company-show-numbers t
        ;; Edge of the completion list cycles around.
        company-selection-wrap-around t
        ;; Do not downcase completions by default.
        company-dabbrev-downcase nil
        ;; Even if I write something with the ‘wrong’ case,
        ;; provide the ‘correct’ casing.
        company-dabbrev-ignore-case t
        ;; Immediately activate completion.
        company-idle-delay 0))

(add-hook 'prog-mode-hook 'company-mode)

;; (straight-use-package 'prescient)
(use-package prescient :straight t :defer t)
;; (prescient-persist-mode +1)

;; (straight-use-package 'ivy-prescient)
(use-package ivy-prescient :straight t :defer t)
;; (ivy-prescient-mode +1)

;; (straight-use-package 'company-prescient)
(use-package company-prescient :straight t :defer t)
;;(company-prescient-mode +1)

(use-package protobuf-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode)))

;; (straight-use-package 'selectrum-prescient)
;; (use-package selectrum-prescient)

;; (straight-use-package 'ivy)
;; (straight-use-package 'smex)
;; (straight-use-package 'flx)
(use-package ivy :straight t :demand t :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (setq ivy-extra-directories nil)
  :config
  (define-key ivy-minibuffer-map (kbd "C-l") (kbd "DEL"))
  (use-package flx :straight t :ensure t)
  (ivy-mode)
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>"    . ivy-resume)
         ("s-b"     . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("TAB"     . ivy-alt-done)
         ("<escape>" . minibuffer-keyboard-quit)
         ("C-j"      . ivy-next-line)
         ("C-k"      . ivy-previous-line)))

(straight-use-package 'swiper)
(global-set-key "\C-s" 'swiper)

;; (straight-use-package 'magit)
(use-package magit
  :straight t
  :defer t
  :commands magit-status
  :hook ((magit-popup-mode-hook . no-trailing-whitespace)
	 (git-commit-mode . goto-address-mode)))

;; (straight-use-package 'magit-todos)
(use-package magit-todos :straight t :defer t)

(use-package magit-delta :straight t :defer t)

(require 'subr-x)
(straight-use-package 'git)

(defun org-git-version ()
  "The Git version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(use-package plantuml-mode :straight t
  :config
  (setq plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar"
        plantuml-default-exec-mode 'jar))

(use-package flycheck-plantuml :straight t
  :hook (plantuml . flycheck))

(use-package org-plus-contrib :straight t
  :mode (("\\.org$" . org-mode))
  :init
  (require 'ob-dot)
  (require 'ob-plantuml)
  (setq org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
  :config
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((clojure . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (java . t)
     (clojure . t)
     (dot . t)
     (C . t)
     (lilypond . t)
     (ruby . t)
     ;; (rust . t)
     ;; (rustic . t)
     (python . t)
     (dot . t)
     (screen . t)
     (fortran . t)
     ;; (prolog . t)
     ;; (kotlin . t)
     ;; (http . t)
     ;; (R . t)
     (ditaa . t)
     (js . t)
     (ammonite . t)
     (shell . t)
     (sql . t)
     (plantuml . t)
     (ocaml . t)))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)))

(use-package org-projectile
  :straight t
  :bind (("C-c n p" . org-projectile-project-todo-completing-read)
         ("C-c c" . org-capture))
  :config
  (progn
    (setq org-projectile-per-project-filepath ".notes/notes.org"
          org-agenda-files (append agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates)))

;; (use-package ob-ammonite :straight t)
;; ;; (use-package org
;; ;;   :straight org-plus-contrib
;; ;;   :mode (("\\.org$" . org-mode))
;; ;;   ("C-c l" . org-store-link)
;; ;;   ("C-c a" . org-agenda))


;; ;; (org-babel-do-load-languages
;; ;;  'org-babel-load-languages
;; ;;  '((clojure . t)
;; ;;    (emacs-lisp . t)
;; ;;    (gnuplot . t)
;; ;;    (java . t)
;; ;;    (clojure . t)
;; ;;    (dot . t)
;; ;;    (C . t)
;; ;;    (lilypond . t)
;; ;;    (ruby . t)
;; ;;    ;; (rust . t)
;; ;;    ;; (rustic . t)
;; ;;    (python . t)
;; ;;    (dot . t)
;; ;;    (screen . t)
;; ;;    (fortran . t)
;; ;;    ;; (prolog . t)
;; ;;    ;; (kotlin . t)
;; ;;    (http . t)
;; ;;    ;; (R . t)
;; ;;    (ditaa . t)
;; ;;    (js . t)
;; ;;    (ammonite . t)
;; ;;    (shell . t)
;; ;;    (sql . t)
;; ;;    (plantuml . t)
;; ;;    (ocaml . t)))

(use-package polymode :straight t)
(use-package poly-markdown :straight t)
(use-package poly-org :straight t)

(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.org" . poly-org-mode))

(use-package org-ql :straight t :defer t)
(use-package magit-org-todos :straight t :defer t :config (magit-org-todos-autoinsert))

(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(straight-use-package 'forge)

(straight-use-package 'projectile)
(use-package projectile :straight t :diminish projectile-mode
  :init
  (setq projectile-enable-caching t
	projectile-cache-file (expand-file-name "projectile.cache" default-snapshot-dir)
	projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" default-snapshot-dir)
	projectile-completion-system 'ivy
	projectile-switch-project-action 'projectile-dired)
  :config (projectile-mode)
  :bind (("C-c f" . projectile-find-file)
	 ("C-c p" . projectile-switch-project)))

(straight-use-package
 '(tree-sitter :host github
               :repo "ubolonton/emacs-tree-sitter"
               :files ("lisp/*.el")))

(straight-use-package
 '(tree-sitter-langs :host github
                     :repo "ubolonton/emacs-tree-sitter"
                     :files ("langs/*.el" "langs/queries")))

(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'tree-sitter-langs)
(add-to-list 'tree-sitter-major-mode-language-alist '(tuareg-mode . ocaml))

(require 'tree-sitter-debug)
(require 'tree-sitter-query)

(add-hook 'tree-sitter-mode-hook #'tree-sitter-hl-mode)

(global-tree-sitter-mode)

(straight-use-package 'go-mode)
(straight-use-package 'rust-mode)
(straight-use-package 'scala-mode)
(straight-use-package 'tuareg)

(use-package flycheck-mercury :straight t)

(straight-use-package 'verb)

(use-package racket-mode :straight t)

(use-package crystal-mode :straight t)

(use-package company-lsp :straight t)

(use-package lsp-metals :straight t)

(use-package lsp-mode
  :straight t
  :init
  (setq lsp-rust-server 'rust-analyzer)
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ;; if you want which-key integration
         ;; (racket-mode . lsp-racket-enable)
         (crystal-mode . lsp)
         (tuareg-mode . lsp)
         (kotlin-mode . lsp)
         (typescript-mode . lsp)
         (rust-mode . lsp)
         (go-mode . lsp)
         (scala-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :straight t :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp :straight t :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :straight t :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs :pin melpa :commands lsp-treemacs-errors-list)

(use-package rmsbolt :straight t)

;; optionally if you want to use debugger
(use-package dap-mode :straight t)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package treemacs :straight t)

;; optional if you want which-key integration
(use-package which-key
  :straight t
  :config
  (which-key-mode))

(use-package flycheck :straight t)

(use-package epg :straight t :config (setq epg-gpgconf-program "gpg"))

(use-package gradle-mode :straight t)
(use-package flycheck-gradle :straight t)

(use-package kotlin-mode :straight t)

(use-package persistent-scratch :straight t :config (persistent-scratch-setup-default))
(use-package dockerfile-mode :straight t)
(use-package docker-compose-mode :straight t)
(use-package dune :straight t)

(use-package pass :straight t)
(use-package password-store :straight t)
(use-package password-store-otp :straight t)
(use-package auth-source-pass :straight t :init (auth-source-pass-enable))

(use-package graphviz-dot-mode :straight t)

(use-package ripgrep :straight t)

(use-package bnf-mode :straight t)

(use-package emacsql-psql :straight t)
(use-package emacsql-mysql :straight t)
(use-package pcap-mode :straight t)

(use-package magithub :straight t)
(use-package ghub :straight t)
(use-package ghub+ :straight t)
(use-package github-browse-file :straight t)

(use-package typescript-mode :straight t)

(setq wl-copy-process nil)

(defun wl-copy (text)
  (setq wl-copy-process (make-process :name "wl-copy"
                                      :buffer nil
                                      :command '("wl-copy" "-f" "-n")
                                      :connection-type 'pipe))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))

(defun wl-paste ()
  (if (and wl-copy-process (process-live-p wl-copy-process))
      nil
    (shell-command-to-string "wl-paste -n | tr -d \r")))

(setq interprogram-cut-function 'wl-copy)
(setq interprogram-paste-function 'wl-paste)

(use-package gist :straight t
  :config
  (setq gist-list-format '((created "Created" 15 nil "%D" "%R")
                           (id      "Id"      10 nil identity)
                           (files   "Files"   35 nil (lambda (files) (mapconcat 'identity files ", ")))
                           (visibility "Private" 8 nil (lambda (public) (or (and public "") "   Y")))
                           (description "Description" 0 nil identity))))

(use-package browse-at-remote :straight t)
