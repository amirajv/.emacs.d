;;; init-packages.el --- package archives and use-package setup  -*- lexical-binding: t; -*-

;; Packages are declared with `use-package' in the file that configures them
;; (theme.el, settings.el, programming.el, ...), not in a central list.

(require 'package)

;; Emacs runs `package-initialize' itself before init.el (see early-init.el),
;; so it must not be called again here.
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))

;; Prefer the curated ELPAs; fall back to MELPA for everything else.
(setq package-archive-priorities
      '(("gnu" . 90) ("nongnu" . 80) ("melpa" . 70)))

;; The startup activation doesn't load the cached archive indexes, so read
;; them, and only fetch on first run.  Don't abort startup when offline
;; (packages that are already installed keep working).
(package-read-all-archive-contents)
(unless package-archive-contents
  (condition-case err
      (package-refresh-contents)
    (error (message "Could not refresh package archives: %s"
                    (error-message-string err)))))

;; Note: doom-modeline 4.2.1 (MELPA Stable) crashed on Emacs 31 with "Defining
;; as dynamic an already lexical var: byte-compile-warnings".  Current MELPA
;; builds don't, which is why this config uses MELPA rather than MELPA Stable.
;; If that error ever returns, `(require 'bytecomp)' here works around it.

;; use-package (built in since Emacs 29).
;; `use-package-always-ensure' stays nil on purpose: built-in features such as
;; dired or ediff are configured with use-package too and must not be looked
;; up on the archives.  Packages from an archive say `:ensure t' explicitly.
(setq use-package-expand-minimally t
      use-package-compute-statistics nil)
(require 'use-package)

;;; init-packages.el ends here
