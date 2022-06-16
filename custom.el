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
^Projectile^          ^Config files^             ^Web^             
─────────────────────────────────────────────────────────────────────────────────────────
_p_: project          _._: .cshrc.local          _y_: ipython      
_f_: file             _i_: .emacs.d              _@_: git link     
_d_: directory        _a_: eshell aliases        _j_: jupyter-lab  
_g_: grep             _s_: eshell snippets       _c_: copy file path
_q_: kill buffers     _!_: load elisp buffer     _o_: org documents
_r_: reset cache      ^ ^                        
"
  ("p" projectile-switch-project)
  ("f" projectile-find-file)
  ("d" projectile-find-dir)
  ("g" projectile-grep)
  ("q" projectile-kill-buffers)
  ("r" projectile-invalidate-cache)
  ("." (find-file "~/.cshrc.local")) 
  ("i" (find-file "~/.emacs.d"))
  ("a" (find-file "~/.emacs.d/eshell/alias"))
  ("s" (find-file "~/.emacs.d/snippets/eshell-mode"))
  ("!" (load-file (buffer-file-name)))
  ("o" (find-file "~/org/"))
  ("y" (async-shell-command "ipython --simple-prompt -i"))
  ("j" (async-shell-command "jupyter-lab" (generate-new-buffer "jupyter-lab")))
  ("@" git-link)
  ("c" my-put-file-name-on-clipboard)
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

