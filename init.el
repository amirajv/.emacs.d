(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-startup-truncated nil)
 '(warning-suppress-types '(((unlock-file)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; libraries
(let ((default-directory  "~/.emacs.d/lib/")) 
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))


;; load other el files
(load-file "~/.emacs.d/init-packages.el")
(load-file "~/.emacs.d/theme.el")
(load-file "~/.emacs.d/settings.el")
(load-file "~/.emacs.d/programming.el")
(load-file "~/.emacs.d/custom.el")
(load-file "~/.emacs.d/django.el")

;; load company related el files
(load-file "~/.emacs.d/apple.el")
