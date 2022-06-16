(require 'magit)
(require 'git-link)
(require 'yaml-mode)
(require 'csv-mode)
(require 'nxml-mode)
(require 'sgml-mode)
(require 'org)
(require 'org-bullets)
(require 'elpy)
(require 'projectile)
(require 'web-mode)
(require 'gitignore-mode)
(require 'gitconfig-mode)
(require 'gitattributes-mode)

;; git-link
(setq git-link-use-single-line-number nil)

;; c/c++
(setq c-default-style
      '((c-mode . "linux") 
        (c++-mode . "linux"))
      c-basic-offset 2)

;; web mode 
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; html mode
(setq sgml-quick-keys 'close) ;; autoclose tags

;; yaml mode
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.gitviews\\'" . yaml-mode))

;; csv-mode: use csv-align-mode for aligning
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))

;; XML editing
(setq nxml-slash-auto-complete-flag t) ;; Typing / after < will close the tag automatically.

;; org-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Make windmove work in Org mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)
(setq org-support-shift-select 'always)

;; projectile
(projectile-mode +1)
(projectile-global-mode)
(setq projectile-completion-system 'ivy)
(setq projectile-enable-caching t)
(setq projectile-sort-order 'recentf)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-switch-project-action (lambda ()
                                         (projectile-run-eshell t)))


;; magit dependencies: dash, transient, with-editor, 
;; 11/11/21: libgit had some issues.
;; decided to remove it and indicate it in default.mk to not compile it

(setq magit-refresh-status-buffer nil) ;; to make things faster
(remove-hook 'server-switch-hook 'magit-commit-diff) ;; to make things faster

;; python
;; elpy: dependencies: s.el, pyvenv, company, projectile, yasnippet
;; note: to reinstall rpc venv:
;; M-x elpy-rpc-reinstall-virtualenv

(load "highlight-indentation")
(set-face-background 'highlight-indentation-face "gray20")
(set-face-background 'highlight-indentation-current-column-face "#c3b3b3")

(load "elpy")
(load "elpy-rpc")
(load "elpy-shell")
(load "elpy-profile")
(load "elpy-refactor")
(load "elpy-django")
(elpy-enable)

(setq elpy-rpc-virtualenv-path 'system) ;; system virtual env is designated using WORKON_HOME variable
(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

;; ipython
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")


