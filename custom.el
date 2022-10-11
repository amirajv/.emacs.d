;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq python-web-server "~/code/http-server/server.py")

(defun run-web-server ()
  (interactive)
  (let ((port) (output-buffer))
    (setq port (read-string "Set a port: " "8080"))
    (setq output-buffer (generate-new-buffer (format "*Web Server --- Port %s*" port)))
    (async-shell-command (concat "python " python-web-server " --port " port) output-buffer)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; finding files and roots
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parent-directory (dir)
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

(defun find-root-in-heirarchy (current-dir)
  "template founction to walk up and search for a number of files \
   through the directory hierarchy, starting from CURRENT-DIR" 
  (if (and (file-exists-p (concat current-dir "fname1"))
           (file-exists-p (concat current-dir "fname2")))
      current-dir
    (when (parent-directory (expand-file-name current-dir))
      (find-root-in-heirarchy (parent-directory (expand-file-name current-dir))))))

(defun run-command-in-directory (command directory &optional buffer-name)
  (if buffer-name
      (async-shell-command (concat "cd " directory "; " command) (generate-new-buffer buffer-name))
    (async-shell-command (concat "cd " directory "; " command))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-get-file-name ()
  (or (buffer-file-name) (dired-file-name-at-point) (read-file-name "Enter file name:")))

(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

(defun eshell/f (filename &optional dir try-count)
  "Searches for files matching FILENAME in either DIR or the
current directory. Just a typical wrapper around the standard
`find' executable.

Since any wildcards in FILENAME need to be escaped, this wraps the shell command.

If not results were found, it calls the `find' executable up to
two more times, wrapping the FILENAME pattern in wildcat
matches. This seems to be more helpful to me."
  (let* ((cmd (concat
               (executable-find "find")
               " " (or dir ".")
               "      -not -path '*/.git*'"
               " -and -not -path '*.o'"
               " -and -not -path '*.o.sh'"
               " -and -not -path '*.complog'"
               " -and "
               " -type f -and "
               "-iname '" filename "'"))
         (results (shell-command-to-string cmd)))

    (if (not (s-blank-str? results))
        results
      (cond
       ((or (null try-count) (= 0 try-count))
        (eshell/f (concat filename "*") dir 1))
       ((or (null try-count) (= 1 try-count))
        (eshell/f (concat "*" filename) dir 2))
       (t "")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hydra
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defhydra general-operations-menu (:color blue
                                   :hint nil)
  "
^Projectile^          ^Scripting^              ^Files^
──────────────────────────────────────────────────────────────────────────────────────
_p_: project          _w_: web server          _c_: copy file path   
_f_: file             _@_: git link            _t_: del trailing whitespace
_d_: directory        _j_: jupyter-lab         _!_: load elisp buffer
_g_: grep             _y_: ipython             _i_: .emacs.d
_q_: kill buffers     _v_: virtual env         _R_: revert buffer
_r_: reset cache      ^ ^                      ^ ^
"
  ("p" projectile-switch-project)
  ("f" projectile-find-file)
  ("d" projectile-find-dir)
  ("g" projectile-grep)
  ("q" projectile-kill-buffers)
  ("r" projectile-invalidate-cache)
  ("w" run-web-server)
  ("@" git-link)
  ("y" (async-shell-command "ipython --simple-prompt -i" (generate-new-buffer "*iPython*")))
  ("j" (async-shell-command "jupyter-lab" (generate-new-buffer "jupyter-lab")))
  ("v" pyvenv-workon)
  ("c" my-put-file-name-on-clipboard)
  ("t" delete-trailing-whitespace)
  ("!" (load-file (buffer-file-name)))
  ("i" (find-file "~/.emacs.d"))
  ("R" revert-buffer-no-confirm)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; some general key bindings
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-q") 'ido-kill-buffer)

;; While the M-( binding to insert-pair is great, I often nee to wrap with other characters:
(global-set-key (kbd "M-[") 'insert-pair)
(global-set-key (kbd "M-{") 'insert-pair)
(global-set-key (kbd "M-<") 'insert-pair)
(global-set-key (kbd "M-'") 'insert-pair)
(global-set-key (kbd "M-`") 'insert-pair)
(global-set-key (kbd "M-\"") 'insert-pair)

;; org
(define-key global-map "\C-cl" 'org-store-link)

;; other
(global-set-key (kbd "<f2>") (lambda () (interactive) (eshell 'N)))
(global-set-key (kbd "<f3>") 'general-operations-menu/body)

