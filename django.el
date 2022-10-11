(defun django-create-project ()
  (async-shell-command (concat "django-admin startproject " (read-string "Project name: "))))

(defun find-root-in-django-heirarchy (current-dir)
  "find django root" 
  (if (file-exists-p (concat current-dir "manage.py"))
      current-dir
    (when (parent-directory (expand-file-name current-dir))
      (find-root-in-django-heirarchy (parent-directory (expand-file-name current-dir))))))

(defun find-django-root ()
  (find-root-in-django-heirarchy default-directory))

(defun run-command-in-django-root (command &optional buffer-name)
  (run-command-in-directory command (find-django-root) buffer-name))

(defun django-manage (command &optional buffer-name)
  (run-command-in-django-root (concat "python manage.py " command) buffer-name))


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
  ("c" (django-create-project))
  ("a" (django-manage (concat "startapp " (read-string "App name: "))))
  ("@" (django-manage "runserver" "*Django: Development server*"))
  ("$" (django-manage "shell" "*Django: shell*"))
  ("A" (django-manage "createsuperuser"))

  ("m" (django-manage "migrate"))
  ("M" (django-manage "makemigrations" (read-string "App name: ")))
  
)

(global-set-key (kbd "<f4>") 'django-operations-menu/body)
