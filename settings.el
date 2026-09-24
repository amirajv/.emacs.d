;;; settings.el --- general settings and package configuration  -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; GUI launches (Dock/Spotlight) don't inherit the shell's PATH, so tools in
;; ~/.local/bin (claude, ...) and ~/.venv/bin would not be found.  PATH is set
;; in ~/.zshrc, so the shell has to be interactive (-i) to see it.  Must run
;; before anything calls `executable-find'.
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :custom
  (exec-path-from-shell-arguments '("-l" "-i"))
  :config
  (exec-path-from-shell-initialize))

;; open maximized on startup.  The initial frame already exists by the time
;; this file is loaded, so `default-frame-alist' only covers later frames.
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(when (display-graphic-p)
  (modify-frame-parameters nil '((fullscreen . maximized))))

;; GUI launches (Dock/Spotlight) start in "/", so start in the home directory
(setq default-directory "~/")

;; misc
(setq-default tab-width 2
              indent-tabs-mode nil)
(setq inhibit-startup-screen t
      use-short-answers t
      require-final-newline t
      ;; run new async shell commands in a fresh buffer without asking
      async-shell-command-buffer 'new-buffer)
(show-paren-mode 1)
(delete-selection-mode 1)

;; scroll, tool, menu bar.  early-init.el already hides them on GUI frames; these
;; calls keep the mode variables in sync (so the first toggle does what it says)
;; and cover terminal frames.
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; show the available keys after a prefix (C-c p, C-c w, C-x, ...)
(use-package which-key
  :ensure nil
  :hook (after-init . which-key-mode))

;; smooth scrolling
(pixel-scroll-precision-mode 1)
(setq scroll-margin 5
      scroll-conservatively 101 ;; never recenter the point when scrolling
      auto-window-vscroll nil
      mouse-wheel-progressive-speed nil
      hscroll-step 1
      hscroll-margin 1)

;; no backup files; auto-save files go to the temp directory, not next to the
;; original
(setq make-backup-files nil
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; automatically make files that start with #! executable when saving
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; improve performance in files with long lines
(global-so-long-mode 1)

;; remember minibuffer history, point position in files, and pick up
;; changes made to files on disk
(savehist-mode 1)
(save-place-mode 1)
(setopt global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

;; mac os: modifier keys.  The NS port (Emacs.app, homebrew emacs-app) uses the
;; `ns-*' variables; the emacs-mac port uses the `mac-*' ones.
;; Cmd is Meta.  Option is the (otherwise unused) Alt modifier, which means it no
;; longer types macOS special characters; set it to `none' to get those back.
(when (eq system-type 'darwin)
  (cond ((featurep 'ns)
         (setq ns-command-modifier 'meta
               ns-alternate-modifier 'alt))
        ((boundp 'mac-command-modifier)
         (setq mac-command-modifier 'meta
               mac-option-modifier 'alt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; buffer-move
(use-package buffer-move
  :ensure t
  :bind (("<C-S-up>"    . buf-move-up)
         ("<C-S-down>"  . buf-move-down)
         ("<C-S-left>"  . buf-move-left)
         ("<C-S-right>" . buf-move-right)))

;; windmove: shift+arrows jump between windows
(windmove-default-keybindings 'shift)

;; winner: undo/redo window configurations (C-c <left> / C-c <right>)
(winner-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completion: ivy, counsel, swiper, company, yasnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ivy
  :ensure t
  :demand t
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  :config
  (ivy-mode 1))

;; `counsel-mode' remaps M-x, C-x C-f, describe-*, yank-pop, etc. to their
;; counsel versions; `ivy-mode' already remaps C-x b to `ivy-switch-buffer'.
(use-package counsel
  :ensure t
  :demand t
  :after ivy
  :config
  (counsel-mode 1))

(use-package swiper
  :ensure t
  :bind ("C-s" . swiper-isearch))

;; hydra: also used by the defhydra menus in custom.el and django.el
(use-package hydra
  :ensure t
  :demand t)

;; company: complete anything
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :bind ("C-." . company-complete)
  :custom
  ;; completion popups make no sense in a terminal buffer
  (company-global-modes '(not vterm-mode)))

(use-package yasnippet
  :ensure t
  :demand t
  :custom
  (yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  :config
  (yas-global-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recentf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package recentf
  :ensure nil
  :demand t
  :custom
  (recentf-max-menu-items 25)
  (recentf-max-saved-items 200)
  :bind ("C-x C-r" . recentf-open-files)
  :config
  (recentf-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ediff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-kill-ediff-buffers ()
  "Kill the leftover *Ediff Registry* buffer after an ediff session."
  (when-let* ((buf (get-buffer "*Ediff Registry*")))
    (kill-buffer buf)))

(use-package ediff
  :ensure nil
  :defer t
  :custom
  (ediff-diff-options "-w") ;; ignore white space changes
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  :config
  ;; restore the window layout from before ediff started
  (add-hook 'ediff-after-quit-hook-internal #'winner-undo)
  (add-hook 'ediff-quit-hook #'my-kill-ediff-buffers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `--group-directories-first' needs GNU ls (coreutils' gls).  Without it, fall
;; back to the built-in ls emulation, which supports a subset of the switches.
(if (executable-find "gls")
    (setq insert-directory-program (executable-find "gls")
          dired-listing-switches "-aBhl --group-directories-first")
  (setopt ls-lisp-use-insert-directory-program nil
          ls-lisp-dirs-first t)
  (setq dired-listing-switches "-aBhl"))

(use-package dired
  :ensure nil
  :defer t
  :custom
  ;; reuse the same buffer when going up (^) and down (RET)
  (dired-kill-when-opening-new-dired-buffer t)
  :hook (dired-mode . dired-hide-details-mode) ;; show details using "("
  :bind (:map dired-mode-map
              ("RET" . dired-find-alternate-file))
  :config
  (put 'dired-find-alternate-file 'disabled nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; terminal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; vterm compiles a native module on first use (needs cmake: brew install cmake).
(use-package vterm
  :ensure t
  :commands vterm
  :custom
  (vterm-max-scrollback 10000)
  (vterm-kill-buffer-on-exit t)
  ;; keys Emacs keeps instead of sending them to the shell: vterm's defaults
  ;; plus the function-key menus from custom.el (<f2> new terminal, <f3>/<f4> hydras)
  (vterm-keymap-exceptions '("C-c" "C-x" "C-u" "C-g" "C-h" "C-l" "M-x" "M-o"
                             "C-y" "M-y" "<f2>" "<f3>" "<f4>")))

;; `git commit' etc. in vterm open the file in this Emacs instead of starting
;; a nested one (with-editor comes with magit).
(use-package with-editor
  :ensure t
  :hook (vterm-mode . with-editor-export-editor))

;;; settings.el ends here
