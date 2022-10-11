(require 'buffer-move)
(require 'ivy)
(require 'counsel)
(require 'swiper)
(require 'hydra)
(require 'dired )
(require 'yasnippet)
(require 'company)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; open maximized on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; open .emacs.d on startup
(setq default-directory "~")

;; misc
(setq default-tab-width 2)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-screen t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq visible-bell 1)
(show-paren-mode 1)
(delete-selection-mode 1)

;; scroll, tool, menu bar
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; smooth scroll
;; Vertical Scroll
(setq scroll-step 1)
(setq scroll-margin 5)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq hscroll-step 1)
(setq hscroll-margin 1)

;; turn off backup files
(setq make-backup-files nil)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; It automatically makes files executable that look like they are scripts. (Start with #!/some/interpreter)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; improve performance in files with long lines
(global-so-long-mode 1)

;; mac os configs
;; key bindings
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'alt)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general package configs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; buffer-move
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; wind move: Now you can use shift+arrows to jump between windows.
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; swiper
;; needs a compile. 

;; ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-s") 'swiper-isearch)

;; recentf
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; ediff
(winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)
(setq ediff-diff-options "-w") ;; ignore white space changes
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(defun my-kill-ediff-buffers ()
  ;; (kill-buffer ediff-buffer-A)
  ;; (kill-buffer ediff-buffer-B)
  ;; (kill-buffer ediff-buffer-C)
  (kill-buffer "*Ediff Registry*"))
  ;; (kill-buffer "*ediff-errors*")
  ;; (kill-buffer "*ediff-diff*"))
(add-hook 'ediff-quit-hook 'my-kill-ediff-buffers)

;; company: complete anything
(setq company-global-modes '(not eshell-mode)) ;; specially since company is interfering with ivy in eshell mode and slowing things down
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "C-.") 'company-complete)

;; yasnippet
(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq dired-listing-switches "-aBhl  --group-directories-first")

;; use same buffer when going up and down
(put 'dired-find-alternate-file 'disabled nil)

;; Automatically run new async shell command without asking
(setq async-shell-command-buffer 'new-buffer)

;; hide details (details can be shown using "(")
(add-hook 'dired-mode-hook
      (lambda ()
        (dired-hide-details-mode)))

(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ;; go down
(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))) ;; go up

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eshell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook
 'eshell-mode-hook
 (lambda ()
   (setq pcomplete-cycle-completions nil)))


