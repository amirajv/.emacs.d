;;; django.el --- helpers for Django projects  -*- lexical-binding: t; -*-

;;; Commentary:

;; Depends on `my-python-command', `run-command-in-directory' and hydra, all set
;; up in custom.el (which is loaded first).

;;; Code:

(require 'comint)
(require 'subr-x)

(defun find-django-root ()
  "Return the closest parent directory of `default-directory' with a manage.py."
  (locate-dominating-file default-directory "manage.py"))

(defun django--root ()
  "Like `find-django-root', but signal a `user-error' if there is none."
  (or (find-django-root)
      (user-error "No manage.py found in or above %s" default-directory)))

(defun django-manage (args &optional buffer-name)
  "Run \"manage.py ARGS...\" in the Django root in an async shell buffer.
ARGS is a list of strings.  If BUFFER-NAME is non-nil, use a new buffer
of that name for the output."
  (run-command-in-directory
   (mapconcat #'shell-quote-argument
              (cons (my-python-command) (cons "manage.py" args))
              " ")
   (django--root)
   buffer-name))

(defun django--manage-comint (name args)
  "Run \"manage.py ARGS...\" in a comint buffer called *NAME* and show it.
Unlike `async-shell-command' this runs on a pty, so interactive commands
such as `shell' or `createsuperuser' work."
  (let ((default-directory (django--root)))
    (pop-to-buffer
     (apply #'make-comint name (my-python-command) nil "manage.py" args))))

(defun django-create-project (name)
  "Create a new Django project called NAME in `default-directory'."
  (interactive "sProject name: ")
  (async-shell-command
   (concat "django-admin startproject " (shell-quote-argument name))))

(defun django-start-app (name)
  "Create a new Django app called NAME."
  (interactive "sApp name: ")
  (django-manage (list "startapp" name)))

(defun django-runserver ()
  "Start the Django development server."
  (interactive)
  (django-manage '("runserver") "*Django: Development server*"))

(defun django-shell ()
  "Start a Django shell."
  (interactive)
  (django--manage-comint "Django: shell" '("shell")))

(defun django-createsuperuser ()
  "Create a Django admin user."
  (interactive)
  (django--manage-comint "Django: createsuperuser" '("createsuperuser")))

(defun django-migrate ()
  "Apply Django migrations."
  (interactive)
  (django-manage '("migrate")))

(defun django-makemigrations (app)
  "Make migrations for APP (all apps if APP is empty)."
  (interactive "sApp name (empty for all): ")
  (django-manage (cons "makemigrations"
                       (unless (string-empty-p app) (list app)))))

(defhydra django-operations-menu (:color blue
                                  :hint nil)
  "
^Basics^                   ^Database^
─────────────────────────────────────────────────────────────────────────────────────────
_c_: create project        _M_: make migrations
_a_: create app            _m_: migrate
_@_: development server
_$_: shell
_A_: create admin user
"
  ("c" django-create-project)
  ("a" django-start-app)
  ("@" django-runserver)
  ("$" django-shell)
  ("A" django-createsuperuser)
  ("m" django-migrate)
  ("M" django-makemigrations))

(keymap-global-set "<f4>" #'django-operations-menu/body)

;;; django.el ends here
