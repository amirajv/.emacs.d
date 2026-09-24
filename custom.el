;;; custom.el --- personal commands, hydras and key bindings  -*- lexical-binding: t; -*-

;;; Commentary:

;; Loaded after programming.el and before django.el.  The hydra below refers to
;; commands from projectile, git-link and pyvenv, which are configured in
;; programming.el.

;;; Code:

(require 'subr-x)
(require 'cl-lib)

(use-package hydra
  :ensure t
  :demand t)

(defgroup my-config nil
  "Personal commands and settings."
  :group 'convenience)

(declare-function org-store-link "ol")
(declare-function vterm "vterm")
(declare-function vterm-send-string "vterm")
(declare-function vterm-send-return "vterm")

;;;; Helpers

(defun my-python-command ()
  "Return the python executable to use.
Prefer the active `pyvenv' virtual environment, then python3/python on
`exec-path'."
  (let ((venv-python (and (bound-and-true-p pyvenv-virtual-env)
                          (expand-file-name "bin/python" pyvenv-virtual-env))))
    (or (and venv-python (file-executable-p venv-python) venv-python)
        (executable-find "python3")
        (executable-find "python")
        "python3")))

(defun run-command-in-directory (command directory &optional buffer-name)
  "Run shell COMMAND asynchronously with DIRECTORY as the working directory.
If BUFFER-NAME is non-nil, show the output in a new buffer of that name."
  (let ((default-directory (file-name-as-directory (expand-file-name directory))))
    (async-shell-command command (and buffer-name
                                      (generate-new-buffer buffer-name)))))

;;;; HTTP server

(defcustom my-web-server-script "~/code/http-server/server.py"
  "Python script started by `run-web-server'."
  :type 'file
  :group 'my-config)

(defun run-web-server (port)
  "Run `my-web-server-script' on PORT in an async shell buffer."
  (interactive (list (read-string "Set a port: " "8080")))
  (let ((script (expand-file-name my-web-server-script)))
    (unless (file-exists-p script)
      (user-error "Web server script not found: %s" script))
    (async-shell-command
     (mapconcat #'shell-quote-argument
                (list (my-python-command) script "--port" port)
                " ")
     (generate-new-buffer (format "*Web Server --- Port %s*" port)))))

;;;; General commands

(defun my-put-file-name-on-clipboard ()
  "Put the current file name (or dired directory) on the kill ring/clipboard."
  (interactive)
  (let ((filename (if (derived-mode-p 'dired-mode)
                      default-directory
                    buffer-file-name)))
    (if filename
        (progn (kill-new filename)
               (message "%s" filename))
      (message "Buffer is not visiting a file"))))

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

;;;; Hydra

(defun my-ipython ()
  "Run an interactive ipython in an async shell buffer."
  (interactive)
  (async-shell-command "ipython --simple-prompt -i"
                       (generate-new-buffer "*iPython*")))

(defun my-jupyter-lab ()
  "Run jupyter-lab in an async shell buffer."
  (interactive)
  (async-shell-command "jupyter-lab" (generate-new-buffer "jupyter-lab")))

(defun my-load-current-buffer ()
  "Load the file visited by the current buffer."
  (interactive)
  (load-file (or buffer-file-name (user-error "Buffer is not visiting a file"))))

;;;; vterm: Claude sessions and buffer titles

(defcustom my-claude-command "claude"
  "Shell command that starts a Claude Code session."
  :type 'string
  :group 'my-config)

(defun my-vterm--lsof-command (proc)
  "Return the `lsof' command line that prints the working directory of PROC."
  (list "lsof" "-a" "-d" "cwd" "-Fn" "-p" (number-to-string (process-id proc))))

(defun my-vterm--parse-lsof (output)
  "Return the existing directory named in `lsof -Fn' OUTPUT, or nil."
  (when (string-match "^n\\(/.*\\)$" output)
    (let ((dir (match-string 1 output)))
      (when (file-directory-p dir)
        (file-name-as-directory dir)))))

(defun my-vterm-shell-directory ()
  "Return the current working directory of the shell in this vterm buffer.
vterm only updates `default-directory' when the shell is configured to report
it, so ask the OS instead (`lsof' ships with macOS).  Return nil if it is
unknown: no live process, `lsof' failing, or a remote (TRAMP) buffer.
This blocks for the duration of `lsof' (~30 ms); the title timer uses the
asynchronous `my-vterm-update-title' instead."
  (let ((proc (get-buffer-process (current-buffer))))
    (when (and (process-live-p proc)
               (not (file-remote-p default-directory)))
      ;; Run locally from "/": `default-directory' may have been deleted.
      (let ((default-directory "/")
            (coding-system-for-read 'utf-8))
        (with-temp-buffer
          (when (eq 0 (ignore-errors
                        (apply #'process-file (car (my-vterm--lsof-command proc))
                               nil t nil (cdr (my-vterm--lsof-command proc)))))
            (my-vterm--parse-lsof (buffer-string))))))))

(defvar my-vterm-title-timer nil
  "Timer that keeps vterm buffer names in sync with their shell's directory.")

(defvar-local my-vterm-tracked-dir nil
  "Last shell directory seen for this vterm buffer.")

(defvar-local my-vterm-auto-name nil
  "Buffer name this vterm was given automatically.
`my-vterm--apply-directory' only renames the buffer while its name is still
this, so a name chosen by hand (`rename-buffer') sticks.")

(defvar-local my-vterm-lsof-process nil
  "The pending `lsof' process for this vterm buffer, if any.")

(defun my-vterm--apply-directory (dir)
  "Make the current vterm buffer reflect that its shell is in DIR.
Set `default-directory' and rename the buffer to `*vterm: PROJECT*', where
PROJECT is the projectile project name, or the directory outside a project.
Claude sessions (`*claude: ...*') and buffers renamed by hand keep their name."
  (unless (equal dir my-vterm-tracked-dir)
    (unless my-vterm-auto-name
      (setq my-vterm-auto-name (buffer-name)))
    (setq my-vterm-tracked-dir dir
          default-directory dir)
    (when (and (equal (buffer-name) my-vterm-auto-name)
               (not (string-prefix-p "*claude" (buffer-name))))
      (rename-buffer
       (format "*vterm: %s*"
               (or (and (projectile-project-root) (projectile-project-name))
                   (abbreviate-file-name (directory-file-name dir))))
       t)
      (setq my-vterm-auto-name (buffer-name)))))

(defun my-vterm-update-title ()
  "Asynchronously sync the current vterm buffer with its shell's directory.
Starts `lsof' in the background (skipped while a previous one is still
running) and calls `my-vterm--apply-directory' with the result, so Emacs
never blocks on it."
  (let ((buf (current-buffer))
        (proc (get-buffer-process (current-buffer))))
    (when (and (process-live-p proc)
               (not (file-remote-p default-directory))
               (not (process-live-p my-vterm-lsof-process)))
      (let ((default-directory "/"))
        (setq my-vterm-lsof-process
              (ignore-errors
                (make-process
                 :name "vterm-cwd"
                 :command (my-vterm--lsof-command proc)
                 :buffer (generate-new-buffer " *vterm-cwd*" t)
                 :coding 'utf-8
                 :noquery t
                 :sentinel
                 (lambda (p _event)
                   (unless (process-live-p p)
                     (let ((out (with-current-buffer (process-buffer p)
                                  (buffer-string)))
                           (dir nil))
                       (kill-buffer (process-buffer p))
                       (setq dir (my-vterm--parse-lsof out))
                       (when (and dir (buffer-live-p buf))
                         (with-current-buffer buf
                           (condition-case nil
                               (my-vterm--apply-directory dir)
                             (error nil))))))))))))))

(defun my-vterm-update-all-titles ()
  "Run `my-vterm-update-title' in every vterm buffer.
Cancel the timer when there are none left; opening a vterm restarts it."
  (let ((found nil))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (derived-mode-p 'vterm-mode)
          (setq found t)
          (my-vterm-update-title))))
    (unless found
      (when (timerp my-vterm-title-timer)
        (cancel-timer my-vterm-title-timer))
      (setq my-vterm-title-timer nil))))

(defun my-vterm-start-title-timer ()
  "Start the (single, global) vterm title timer."
  (unless (timerp my-vterm-title-timer)
    (setq my-vterm-title-timer
          (run-with-timer 1 1 #'my-vterm-update-all-titles))))

(add-hook 'vterm-mode-hook #'my-vterm-start-title-timer)

(defun my-new-vterm (&optional new)
  "Switch to a vterm for the current project, creating one if needed.
With prefix argument NEW, always create a new vterm.  Claude sessions
(`*claude: ...*') are never reused.  A vterm is reused when its shell is
inside the current project (or the current directory outside a project)."
  (interactive "P")
  (require 'vterm)
  (let* ((root (or (projectile-project-root) default-directory))
         (existing
          (and (not new)
               (seq-find
                (lambda (buf)
                  (with-current-buffer buf
                    (and (derived-mode-p 'vterm-mode)
                         (not (string-prefix-p "*claude" (buffer-name)))
                         (file-in-directory-p default-directory root))))
                (buffer-list)))))
    (if existing
        (pop-to-buffer-same-window existing)
      (let ((default-directory root))
        (vterm t)))))

(defvar-local my-claude-root nil
  "Project root a `my-claude-in-project' session was started for.")

(defun my-claude--start (&optional args)
  "Open Claude Code in a vterm at the root of the current project.
ARGS is an optional string of extra command-line arguments.  The project is
the one containing the current directory (for a vterm buffer, the directory
its shell is in; otherwise `default-directory'), falling back to that
directory outside a project.  Switch to the existing session for the project
if there is one; ARGS only apply when a new session is started."
  (require 'projectile)
  (require 'vterm)
  (let* ((dir (or (and (derived-mode-p 'vterm-mode) (my-vterm-shell-directory))
                  default-directory))
         (root (or (let ((default-directory dir)) (projectile-project-root))
                   dir))
         (existing (seq-find (lambda (buf)
                               (and (buffer-live-p buf)
                                    (equal (buffer-local-value 'my-claude-root buf)
                                           root)))
                             (buffer-list))))
    (if existing
        (pop-to-buffer-same-window existing)
      (let ((default-directory root)
            (name (format "*claude: %s*"
                          (file-name-nondirectory (directory-file-name root)))))
        ;; `vterm' displays and selects the new buffer
        (vterm (generate-new-buffer-name name))
        (setq my-claude-root root)
        (vterm-send-string (if args
                               (concat my-claude-command " " args)
                             my-claude-command))
        (vterm-send-return)))))

(defun my-claude-in-project ()
  "Start (or switch to) a Claude Code session in the current project."
  (interactive)
  (my-claude--start))

(defun my-claude-continue-in-project ()
  "Start (or switch to) a Claude Code session in the current project.
A new session resumes the most recent conversation (`claude --continue')."
  (interactive)
  (my-claude--start "--continue"))

(defun my-open-emacs-dir ()
  "Open `user-emacs-directory' in dired."
  (interactive)
  (find-file user-emacs-directory))

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
_r_: reset cache      _C_: claude              _X_: restart emacs
                    _L_: claude --continue
"
  ("p" projectile-switch-project)
  ("f" projectile-find-file)
  ("d" projectile-find-dir)
  ("g" projectile-grep)
  ("q" projectile-kill-buffers)
  ("r" projectile-invalidate-cache)
  ("w" run-web-server)
  ("@" git-link)
  ("y" my-ipython)
  ("j" my-jupyter-lab)
  ("v" pyvenv-workon)
  ("C" my-claude-in-project)
  ("L" my-claude-continue-in-project)
  ("c" my-put-file-name-on-clipboard)
  ("t" delete-trailing-whitespace)
  ("!" my-load-current-buffer)
  ("i" my-open-emacs-dir)
  ("R" revert-buffer-no-confirm)
  ("X" restart-emacs))

;;;; Key bindings

(keymap-global-set "C-;" #'comment-line)
(keymap-global-set "C-+" #'text-scale-increase)
(keymap-global-set "C--" #'text-scale-decrease)
(keymap-global-set "C-x C-b" #'ibuffer)
;; NOTE: shadows `quoted-insert'.  With ivy this prompts for the buffer to kill.
(keymap-global-set "C-q" #'kill-buffer)

;; `M-(' already inserts a pair of parentheses; wrap with other characters too.
;; `M-<', `M-{' and `M-[' are deliberately not used: they shadow
;; `beginning-of-buffer' and `backward-paragraph', and `M-[' is the escape
;; prefix for arrow keys in terminal frames.  Those live under `C-c w' instead.
(keymap-global-set "M-'" #'insert-pair)
(keymap-global-set "M-`" #'insert-pair)
(keymap-global-set "M-\"" #'insert-pair)
(keymap-global-set "C-c w [" #'insert-pair)
(keymap-global-set "C-c w {" #'insert-pair)
(keymap-global-set "C-c w <" #'insert-pair)

(keymap-global-set "C-c l" #'org-store-link)

;; terminal for the current project (C-u: always a new one)
(keymap-global-set "<f2>" #'my-new-vterm)
(keymap-global-set "<f3>" #'general-operations-menu/body)

;;; custom.el ends here
