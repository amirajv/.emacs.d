(require 'doom-modeline)
(require 'doom-themes)
(require 'doom-themes-ext-org)
(require 'doom-themes-ext-visual-bell)
(require 'all-the-icons)
(require 'all-the-icons-dired)

;; all the icons
;; note: fonts should be installed too

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; doom-modeline
(doom-modeline-mode 1)
(setq doom-modeline-bar-width 4)
(setq doom-modeline-buffer-encoding nil)
(setq doom-modeline-buffer-file-name-style 'truncate-with-project)


;; doom-themes
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
(load-theme 'doom-dark+ t)
(setq doom-themes-treemacs-theme "doom-atom") 
(doom-themes-visual-bell-config) ;; Enable flashing mode-line on errors
(setq org-fontify-done-headline t) ;; make any DONE heading grey
(doom-themes-org-config) ;; Corrects (and improves) org-mode's native fontification.

;; eshell prompt
(defun my-eshell-prompt-function ()
  (concat "\xf07c " (eshell/pwd) "\n└─ $ " ))
(setq eshell-prompt-function 'my-eshell-prompt-function)

