;;; early-init.el --- early startup settings  -*- lexical-binding: t; -*-

;; Runs before the package system and the first frame are initialised.

;;;; macOS native-compilation fix

;; On very new macOS releases libgccjit derives a bogus deployment target from
;; the kernel version (macOS 27 / Darwin 27.0 => "-mmacosx-version-min=18.0")
;; and clang rejects it, so *every* native compilation fails.  That used to
;; crash startup as soon as something advised a primitive (trampoline
;; compilation, e.g. doom-modeline advising `handle-switch-frame').
;; Giving the compiler an explicit deployment target avoids the bad guess.
;; The .eln files only need to run on this machine, so any sane value works.
(when (and (eq system-type 'darwin)
           (native-comp-available-p)
           (not (getenv "MACOSX_DEPLOYMENT_TARGET")))
  (setenv "MACOSX_DEPLOYMENT_TARGET" "15.0"))

;; Don't pop up the *Warnings* buffer for every package compiled in the
;; background; real errors still land in *Async-native-compile-log*.
(setq native-comp-async-report-warnings-errors 'silent)

;;;; Startup speed

;; Defer garbage collection while starting up, then restore a sane limit.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 32 1024 1024))))

;; Prefer newer source over stale byte code.
(setq load-prefer-newer t)

;;;; Frame

;; Set these here so the frame never flashes with a menu/tool/scroll bar.
;; (`package-enable-at-startup' is left at its default: Emacs runs
;; `package-initialize' itself right before init.el, so init.el must not.)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq frame-inhibit-implied-resize t)

;; Open maximized without the window visibly resizing after startup
;; (settings.el applies the same to frames created later).
(push '(fullscreen . maximized) initial-frame-alist)

;;; early-init.el ends here
