;;; init.el --- Emacs configuration entry point  -*- lexical-binding: t; -*-

;; Keep Customize output out of the hand-written files.
(setq custom-file (expand-file-name "custom-vars.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

(dolist (file '("init-packages"
                "theme"
                "settings"
                "programming"
                "custom"
                "django"))
  (load (expand-file-name file user-emacs-directory) nil 'nomessage))

;;; init.el ends here
