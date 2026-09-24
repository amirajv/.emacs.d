;;; theme.el --- fonts, theme, icons, modeline  -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; font
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-font-candidates '("Source Code Pro" "JetBrains Mono" "Menlo" "Monaco")
  "Monospace fonts to try, in order of preference.")

(defvar my-font-height 140
  "Default face height, in 1/10 pt (140 = 14pt).")

(defun my-apply-font (&optional frame)
  "Use the first available font of `my-font-candidates' on FRAME."
  (when (display-graphic-p frame)
    (let ((font (seq-find (lambda (f) (find-font (font-spec :name f) frame))
                          my-font-candidates)))
      (when font
        (set-face-attribute 'default frame :family font :height my-font-height)))))

;; Fonts can't be resolved without a graphical frame (a daemon has none at
;; startup), so also apply the font to every frame created later, e.g. by
;; `emacsclient -c'.  `my-apply-font' is a no-op on terminal frames.
(my-apply-font)
(add-hook 'after-make-frame-functions #'my-apply-font)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; icons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; note: run M-x nerd-icons-install-fonts once to install the icon font
;; ("Symbols Nerd Font Mono").
(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; doom-themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)    ; if nil, bold is universally disabled
  (doom-themes-enable-italic t)  ; if nil, italics is universally disabled
  :config
  (load-theme 'doom-dark+ t)
  (doom-themes-visual-bell-config) ; flash the mode-line on errors
  (doom-themes-org-config))        ; corrects (and improves) org-mode's fontification

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window borders
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; doom-dark+ draws window borders in `bg-alt', which is nearly the same as
;; the buffer background, so splits are hard to see.  Draw them in a visible
;; colour instead (the width only affects graphical frames).
(defvar my-window-divider-width 1
  "Width in pixels of the borders between windows.")

(setq window-divider-default-places t ; both right and bottom borders
      window-divider-default-right-width my-window-divider-width
      window-divider-default-bottom-width my-window-divider-width)
(window-divider-mode 1)

(defun my-window-border-faces (&optional theme)
  "Give window borders a visible colour taken from the active doom theme.
`window-divider' and its first/last pixel faces inherit `vertical-border',
which is also what terminal frames use, so setting it covers both.
THEME is the theme being enabled (nil when called directly); other
themes' borders are left alone."
  (when (and (fboundp 'doom-color)
             (or (null theme) (string-prefix-p "doom-" (symbol-name theme))))
    (let ((color (doom-color 'base4)))
      (when (stringp color)
        (set-face-attribute 'vertical-border nil
                            :foreground color :background color)))))

;; Reapply whenever a theme is enabled, and once now for the theme loaded above.
(add-hook 'enable-theme-functions #'my-window-border-faces)
(my-window-border-faces)

;; make any DONE heading grey (the doom org extension honours this).  A plain
;; `setq': a `use-package org' block would `require' org (~100 ms) at startup.
(setq org-fontify-done-headline t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; doom-modeline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-bar-width 4)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-file-name-style 'truncate-with-project))

;;; theme.el ends here
