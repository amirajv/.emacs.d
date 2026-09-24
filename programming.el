;;; programming.el --- language and tooling configuration  -*- lexical-binding: t; -*-

;; Packages are installed by `use-package' (:ensure t); built-ins use
;; :ensure nil.  Everything is deferred until the relevant mode/command is used.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :ensure t
  :defer t
  :custom
  (magit-refresh-status-buffer nil) ;; to make things faster
  :config
  ;; magit adds this hook when it loads, so it has to be removed afterwards
  (remove-hook 'server-switch-hook 'magit-commit-diff)) ;; to make things faster

;; git change markers in the fringe (and in dired / after magit refreshes)
(use-package diff-hl
  :ensure t
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package git-link
  :ensure t
  :commands (git-link git-link-commit git-link-homepage)
  :custom
  (git-link-use-single-line-number nil))

;; provides gitignore-mode, gitconfig-mode, gitattributes-mode, ...
(use-package git-modes
  :ensure t
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; c/c++: the "linux" style forces c-basic-offset to 8, so derive a 2-space one
(setq c-default-style '((c-mode . "my-linux")
                        (c++-mode . "my-linux")
                        (other . "gnu")))
(with-eval-after-load 'cc-mode
  (c-add-style "my-linux" '("linux" (c-basic-offset . 2))))

;; language server for C/C++ (clangd ships with the Xcode command line tools;
;; put a compile_commands.json at the project root)
(use-package eglot
  :ensure nil
  :hook ((c-mode c++-mode) . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  :init
  (setq read-process-output-max (* 1024 1024))) ;; bigger reads from language servers

(use-package web-mode
  :ensure t
  :mode ("\\.js\\'" "\\.html?\\'"))

(use-package sgml-mode
  :ensure nil
  :defer t
  :custom
  (sgml-quick-keys 'close)) ;; autoclose tags

(use-package nxml-mode
  :ensure nil
  :defer t
  :custom
  (nxml-slash-auto-complete-flag t)) ;; typing / after < closes the tag

;; README.md, CLAUDE.md, ...  (preview needs `markdown-command', e.g. pandoc)
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode))

(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" "\\.gitviews\\'"))

;; csv-mode: use csv-align-mode for aligning
(use-package csv-mode
  :ensure t
  :mode ("\\.[Cc][Ss][Vv]\\'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :ensure nil
  :defer t
  :hook ((org-mode . org-indent-mode)
         ;; make windmove work in Org mode
         (org-shiftup-final . windmove-up)
         (org-shiftleft-final . windmove-left)
         (org-shiftdown-final . windmove-down)
         (org-shiftright-final . windmove-right))
  :custom
  (org-support-shift-select 'always)
  (org-startup-truncated nil))

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; projectile 3.x has no `ivy' completion option: it uses `completing-read',
;; which `ivy-mode' already takes over.
(use-package projectile
  :ensure t
  :hook (after-init . projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-enable-caching t)
  (projectile-sort-order 'recentf)
  (projectile-switch-project-action (lambda () (projectile-run-vterm t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; python virtual environments: create one in ~/.venv/<name>, then M-x pyvenv-workon
(use-package pyvenv
  :ensure t
  :init
  (setenv "WORKON_HOME" (expand-file-name "~/.venv/"))
  :config
  (pyvenv-mode 1))

;; indentation guides (used by elpy)
(use-package highlight-indentation
  :ensure t
  :defer t
  :custom-face
  (highlight-indentation-face ((t (:background "gray20"))))
  (highlight-indentation-current-column-face ((t (:background "#c3b3b3")))))

(use-package python
  :ensure nil
  :defer t
  :config
  ;; ipython, from PATH or from the ~/.venv used by elpy
  (let ((ipython (or (executable-find "ipython")
                     (let ((venv-ipython (expand-file-name "~/.venv/bin/ipython")))
                       (and (file-executable-p venv-ipython) venv-ipython)))))
    (when ipython
      (setq python-shell-interpreter ipython
            python-shell-interpreter-args "-i --simple-prompt"))))

;; elpy: dependencies: s.el, pyvenv, company, highlight-indentation, yasnippet
;; config: M-x elpy-config
(use-package elpy
  :ensure t
  :commands elpy-enable
  :init
  (with-eval-after-load 'python (elpy-enable))
  :custom
  (elpy-rpc-virtualenv-path 'current)
  :config
  (let ((venv-python (expand-file-name "~/.venv/bin/python")))
    (when (file-executable-p venv-python)
      (setq elpy-rpc-python-command venv-python))))

;;; programming.el ends here
