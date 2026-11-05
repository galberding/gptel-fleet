;;; gptel-fleet-forge.el --- Turn tool calls into dedicated tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Georg Alberding
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The forge turns a concrete tool call (usually a Bash command the model
;; keeps issuing) into a dedicated, parameterised command tool that lives
;; in a package file and has its own approval level.
;;
;; Entry points:
;;   `gptel-fleet-forge-from-call-at-point'  a tool call block in a chat buffer
;;   `gptel-fleet-forge-from-log'            a logged call, most frequent first
;;   `gptel-fleet-bash-stats'                Bash usage across all logs, grouped
;;                                           by command signature; RET forges
;;   `gptel-fleet-forge-from-command'        type a command
;;   `gptel-fleet-forge-edit-tool'           re-open an existing fleet tool
;;
;; A draft opens in a forge buffer (Emacs Lisp, one
;; `gptel-fleet-define-tool' form).  There:
;;   C-c C-r  render: show the argv each example would execute (nothing runs)
;;   C-c C-t  test: run an example (or args you type) in a test environment
;;   C-c C-e  run all :examples (regression check)
;;   C-c C-c  validate, save into tools/<package>.el, load, register,
;;            optionally add the tool to agents
;;   C-c C-k  abort
;;
;; Test environments (`gptel-fleet-forge-test-env'):
;;   bwrap-ro  read-only root, private /tmp, no network: nothing can change
;;   bwrap     read-only root, writable cwd, no network
;;   tmpdir    run in a fresh empty temp directory
;;   direct    run for real in the tool's cwd
;; The original output (from the chat or log) is shown next to the test
;; output when it is known.

;;; Code:

(require 'gptel-fleet-core)
(require 'gptel-fleet-policy)
(require 'gptel-fleet-log)
(require 'gptel-fleet-tools)

(declare-function gptel-fleet-apply "gptel-fleet")
(declare-function org-unescape-code-in-string "org-src")

(defcustom gptel-fleet-forge-test-env 'bwrap-ro
  "Default test environment for the forge bench."
  :group 'gptel-fleet
  :type '(choice (const bwrap-ro) (const bwrap) (const tmpdir) (const direct)))

(defcustom gptel-fleet-forge-default-level 'confirm
  "Level given to newly forged tools."
  :group 'gptel-fleet
  :type 'symbol)

(defvar-local gptel-fleet-forge--origin nil
  "Plist (:tool :args :result :source) of the call the draft came from.")

;;;; Parsing commands

(defun gptel-fleet-forge--strip (cmd)
  "Split CMD into (CORE . PROPS) recognising cd/2>&1/tail/head wrappers."
  (let ((props nil) (core (string-trim cmd)))
    (when (string-match "\\`cd\\s-+\\(\\S-+\\)\\s-*&&\\s-*" core)
      (setq props (plist-put props :cwd (match-string 1 core))
            core (substring core (match-end 0))))
    (when (string-match "\\s-*|\\s-*tail\\s-+-n?\\s-*\\([0-9]+\\)\\s-*\\'" core)
      (setq props (plist-put props :tail (string-to-number (match-string 1 core)))
            core (substring core 0 (match-beginning 0))))
    (when (string-match "\\s-*|\\s-*head\\s-+-n?\\s-*\\([0-9]+\\)\\s-*\\'" core)
      (setq props (plist-put props :head (string-to-number (match-string 1 core)))
            core (substring core 0 (match-beginning 0))))
    (when (string-match "\\s-*2>&1\\s-*\\'" core)
      (setq core (substring core 0 (match-beginning 0))))
    (when (string-match "\\s-*2>/dev/null\\s-*\\'" core)
      (setq props (plist-put props :stderr 'drop)
            core (substring core 0 (match-beginning 0))))
    (cons (string-trim core) props)))

(defun gptel-fleet-forge-signature (cmd)
  "Short signature of shell CMD: program plus subcommand, per segment."
  (mapconcat
   (lambda (seg)
     (let* ((toks (ignore-errors (split-string-shell-command seg)))
            (toks (cl-remove-if (lambda (t1) (string-match-p "\\`[A-Z_]+=" t1)) toks))
            (prog (file-name-nondirectory (or (car toks) "")))
            (sub (cadr toks)))
       (if (and sub (string-match-p "\\`[a-z][a-z0-9-]*\\'" sub))
           (concat prog " " sub) prog)))
   (gptel-fleet-shell-segments (car (gptel-fleet-forge--strip cmd)))
   " | "))

(defun gptel-fleet-forge--flag-name (flag)
  "Arg name derived from FLAG (\"--package\" -> \"package\")."
  (let ((n (replace-regexp-in-string "\\`-+" "" flag)))
    (replace-regexp-in-string "[^a-z0-9_]" "_" (downcase n))))

(defun gptel-fleet-forge-draft (cmd params)
  "Build (ID . SPEC) for shell CMD.
PARAMS is an alist (TOKEN-INDEX . ARG-NAME) of tokens (0-based, in the
first segment after stripping) that become arguments.  A parameterised
token directly after an option flag absorbs the flag as (:opt FLAG ARG)."
  (let* ((stripped (gptel-fleet-forge--strip cmd))
         (core (car stripped))
         (segments (gptel-fleet-shell-segments core))
         (toks (split-string-shell-command (car segments)))
         (prog (file-name-nondirectory (car toks)))
         (sub (and (cadr toks) (string-match-p "\\`[a-z][a-z0-9-]*\\'" (cadr toks))
                   (not (assq 1 params)) (cadr toks)))
         (id (intern (if sub (format "%s-%s" prog sub) prog)))
         (template nil) (args nil) (example nil)
         (i 0))
    (while (< i (length toks))
      (let* ((tok (nth i toks))
             (next-param (assq (1+ i) params))
             (param (assq i params)))
        (cond
         ((and (string-match-p "\\`--?[A-Za-z]" tok) next-param (not param))
          (let ((name (cdr next-param)))
            (push `(:opt ,tok ,(intern name)) template)
            (push (list :name name :type 'string :optional t
                        :description (format "Value for %s" tok))
                  args)
            (setq example (plist-put example (intern (concat ":" name)) (nth (1+ i) toks)))
            (cl-incf i)))
         (param
          (let ((name (cdr param)))
            (push (intern name) template)
            (push (list :name name :type 'string
                        :description (format "Replaces %S in the original call" tok))
                  args)
            (setq example (plist-put example (intern (concat ":" name)) tok))))
         (t (push tok template))))
      (cl-incf i))
    (cons id
          (append
           (list :package (intern prog)
                 :name (gptel-fleet--camel id)
                 :description (format "Runs `%s'.  Describe here, for the model, what this \
does and when to use it instead of Bash." (car segments))
                 :args (nreverse args)
                 :level gptel-fleet-forge-default-level
                 :command (nreverse template))
           (cdr stripped)
           (unless (plist-get (cdr stripped) :cwd) (list :cwd 'project))
           (list :timeout gptel-fleet-command-timeout
                 :examples (list example))
           (when (cdr segments)
             (list :forge-note (format "Original was a pipeline; only the first segment \
was converted: %s" core)))))))

;;;; Forge buffer

(defvar-keymap gptel-fleet-forge-mode-map
  "C-c C-r" #'gptel-fleet-forge-render
  "C-c C-t" #'gptel-fleet-forge-test
  "C-c C-e" #'gptel-fleet-forge-run-examples
  "C-c C-c" #'gptel-fleet-forge-save
  "C-c C-k" #'gptel-fleet-forge-abort)

(define-minor-mode gptel-fleet-forge-mode
  "Minor mode for forge drafts.
\\{gptel-fleet-forge-mode-map}"
  :lighter " Forge")

(defun gptel-fleet-forge--form-string (id spec)
  "Pretty `gptel-fleet-define-tool' form for ID and SPEC."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert (format "(gptel-fleet-define-tool %s" id))
    (cl-loop for (k v) on spec by #'cddr
             unless (memq k '(:file :id :forge-note))
             do (insert (format "\n  %s %s" k
                                (string-trim-right
                                 (let ((print-length nil) (print-level nil)
                                       (print-escape-newlines t))
                                   (if (memq k '(:args :examples))
                                       (concat "("
                                               (mapconcat #'prin1-to-string v "\n   ")
                                               ")")
                                     (prin1-to-string v)))))))
    (insert ")\n")
    (buffer-string)))

(defun gptel-fleet-forge-open (id spec &optional origin)
  "Open a forge buffer for ID with SPEC.  ORIGIN describes the source call."
  (let ((buf (generate-new-buffer (format "*gptel-fleet forge: %s*" id))))
    (with-current-buffer buf
      (emacs-lisp-mode)
      (insert ";; gptel-fleet forge.  C-c C-r render · C-c C-t test · C-c C-e examples\n"
              ";;                     C-c C-c save to tools/<package>.el · C-c C-k abort\n"
              ";; Template: \"lit\" ARG (:opt \"-x\" ARG) (:flag \"--y\" ARG) (:split ARG) (:fmt \"%s\" ARG)\n"
              ";; Keys: :cwd project|default|\"dir\"|ARG :timeout :tail :head :max-output\n"
              ";;       :stderr merge|drop :env (\"K=V\") :inherit-env nil :sandbox t\n"
              ";; Args: add :allow-dash t to let a value start with \"-\".\n")
      (when-let* ((note (plist-get spec :forge-note)))
        (insert ";; NOTE: " note "\n"))
      (when origin
        (insert (format ";; Original %s call: %s\n" (plist-get origin :tool)
                        (gptel-fleet--one-line (prin1-to-string (plist-get origin :args)) 300))))
      (insert "\n" (gptel-fleet-forge--form-string id spec))
      (goto-char (point-min))
      (gptel-fleet-forge-mode 1)
      (setq gptel-fleet-forge--origin origin))
    (pop-to-buffer buf)
    buf))

(defun gptel-fleet-forge--read ()
  "Read the draft in the current buffer: (ID . SPEC)."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^(gptel-fleet-define-tool\\_>" nil t)
      (user-error "No gptel-fleet-define-tool form in buffer"))
    (goto-char (match-beginning 0))
    (let ((form (read (current-buffer))))
      (cons (nth 1 form) (cddr form)))))

(defun gptel-fleet-forge-validate (id spec)
  "Problems with draft ID / SPEC (strings, \"warning:\" ones are soft)."
  (let* ((errs nil)
         (args (plist-get spec :args))
         (names (mapcar (lambda (a) (plist-get a :name)) args))
         (template (plist-get spec :command))
         (used nil))
    (cl-flet ((err (fmt &rest a) (push (apply #'format fmt a) errs)))
      (unless (symbolp id) (err "tool id must be a symbol"))
      (unless (stringp (plist-get spec :name)) (err ":name must be a string"))
      (unless (string-match-p "\\`[A-Za-z][A-Za-z0-9_]*\\'" (or (plist-get spec :name) ""))
        (err ":name should be alphanumeric (models are picky)"))
      (let ((d (plist-get spec :description)))
        (cond ((not (stringp d)) (err ":description missing"))
              ((string-match-p "Describe here" d)
               (err "warning: :description still has the placeholder text"))))
      (unless (memq (plist-get spec :level) gptel-fleet-levels)
        (err ":level must be one of %S" gptel-fleet-levels))
      (unless (and (plist-get spec :package) (symbolp (plist-get spec :package)))
        (err ":package must be a symbol"))
      (dolist (a args)
        (unless (and (stringp (plist-get a :name)) (plist-get a :type))
          (err "arg needs :name and :type: %S" a))
        (unless (plist-get a :description)
          (err "warning: arg %s has no :description" (plist-get a :name))))
      (cond
       (template
        (unless (stringp (car template)) (err "first :command element must be the program"))
        (when (and (stringp (car template)) (not (executable-find (car template))))
          (err "warning: program %s not found on this machine" (car template)))
        (dolist (el template)
          (pcase el
            ((pred stringp))
            ((pred symbolp) (push (symbol-name el) used))
            (`(,(or :opt :flag) ,_ ,s) (push (symbol-name s) used))
            (`(:split ,s) (push (symbol-name s) used))
            (`(:fmt ,_ . ,ss) (dolist (s ss) (push (symbol-name s) used)))
            (_ (err "bad template element %S" el))))
        (dolist (u used) (unless (member u names) (err "template uses unknown arg %s" u)))
        (dolist (n names)
          (unless (or (member n used) (equal (format "%s" (plist-get spec :cwd)) n))
            (err "warning: arg %s is never used" n))))
       ((not (plist-get spec :function)) (err "need :command or :function"))))
    (nreverse errs)))

(defun gptel-fleet-forge--example-values (spec example)
  "Alist name -> value from EXAMPLE plist for SPEC."
  (mapcar (lambda (a)
            (let ((n (plist-get a :name)))
              (cons n (plist-get example (intern (concat ":" n))))))
          (plist-get spec :args)))

(defun gptel-fleet-forge-render ()
  "Show the argv every example would execute."
  (interactive)
  (pcase-let ((`(,id . ,spec) (gptel-fleet-forge--read)))
    (with-help-window "*gptel-fleet forge render*"
      (princ (format "%s\n\n" id))
      (dolist (ex (or (plist-get spec :examples) '(nil)))
        (princ (format "example %S\n  => %s\n\n" ex
                       (condition-case e
                           (mapconcat #'shell-quote-argument
                                      (gptel-fleet-render-command
                                       (plist-get spec :command)
                                       (gptel-fleet-forge--example-values spec ex)
                                       (plist-get spec :args))
                                      " ")
                         (error (format "ERROR: %s" (error-message-string e))))))))))

(defun gptel-fleet-forge--bwrap-ro (argv _cwd)
  "Wrap ARGV in a fully read-only bubblewrap."
  (append (list "bwrap" "--ro-bind" "/" "/" "--dev" "/dev" "--proc" "/proc"
                "--tmpfs" "/tmp" "--unshare-net" "--die-with-parent" "--")
          argv))

(defun gptel-fleet-forge--test-spec (spec env)
  "SPEC adjusted for test environment ENV."
  (let ((s (copy-sequence spec)))
    (pcase env
      ('bwrap-ro (plist-put s :sandbox #'gptel-fleet-forge--bwrap-ro))
      ('bwrap (plist-put s :sandbox #'gptel-fleet-sandbox-bwrap))
      ('tmpdir (plist-put s :cwd (make-temp-file "gptel-fleet-forge" t)))
      (_ s))))

(defun gptel-fleet-forge--read-example (spec)
  "Pick an example of SPEC or read values interactively."
  (let* ((exs (plist-get spec :examples))
         (cands (append (mapcar (lambda (e) (cons (prin1-to-string e) e)) exs)
                        '(("<enter values>" . :enter))))
         (pick (cdr (assoc (completing-read "Example: " cands nil t) cands))))
    (if (not (eq pick :enter)) pick
      (cl-loop for a in (plist-get spec :args)
               for v = (read-string (format "%s: " (plist-get a :name)))
               unless (string-empty-p v)
               append (list (intern (concat ":" (plist-get a :name))) v)))))

(defun gptel-fleet-forge--run (spec example env cb)
  "Run SPEC with EXAMPLE in ENV; CB receives the output string."
  (let ((s (gptel-fleet-forge--test-spec spec env)))
    (condition-case e
        (if (plist-get s :command)
            (gptel-fleet-run-command s (gptel-fleet-forge--example-values s example) cb)
          (funcall cb (format "%s" (apply (eval (plist-get s :function) t)
                                          (mapcar #'cdr (gptel-fleet-forge--example-values
                                                         s example))))))
      (error (funcall cb (format "ERROR: %s" (error-message-string e)))))))

(defun gptel-fleet-forge-test (&optional env)
  "Run one example of the draft in test environment ENV."
  (interactive
   (list (intern (completing-read "Test environment: " '("bwrap-ro" "bwrap" "tmpdir" "direct")
                                  nil t nil nil (symbol-name gptel-fleet-forge-test-env)))))
  (pcase-let* ((`(,id . ,spec) (gptel-fleet-forge--read))
               (example (gptel-fleet-forge--read-example spec))
               (origin gptel-fleet-forge--origin)
               (errs (cl-remove-if (lambda (p) (string-prefix-p "warning:" p))
                                   (gptel-fleet-forge-validate id spec))))
    (when errs (user-error "Fix first: %s" (string-join errs "; ")))
    (gptel-fleet-forge--run
     spec example env
     (lambda (out)
       (with-current-buffer (get-buffer-create "*gptel-fleet forge test*")
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert (format "%s  env=%s  example=%S\n\n" id env example)
                   "──── test output ────\n" out "\n")
           (when-let* ((orig (plist-get origin :result)))
             (insert "\n──── original output ────\n" orig "\n"))
           (goto-char (point-min))
           (special-mode))
         (display-buffer (current-buffer)))))))

(defun gptel-fleet-forge-run-examples (&optional env)
  "Run all :examples of the draft in ENV and show the results."
  (interactive (list gptel-fleet-forge-test-env))
  (pcase-let* ((`(,id . ,spec) (gptel-fleet-forge--read))
               (buf (get-buffer-create "*gptel-fleet forge examples*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t)) (erase-buffer) (insert (format "%s env=%s\n" id env))))
    (display-buffer buf)
    (dolist (ex (plist-get spec :examples))
      (gptel-fleet-forge--run
       spec ex env
       (lambda (out)
         (with-current-buffer buf
           (let ((inhibit-read-only t))
             (goto-char (point-max))
             (insert (format "\n──── %S ────\n%s\n" ex out)))))))))

(defun gptel-fleet-forge--package-file (pkg)
  "Package file for PKG, existing or new in the tools dir."
  (or (plist-get (gptel-fleet-package pkg) :file)
      (expand-file-name (concat (gptel-fleet--safe-name pkg) ".el") (gptel-fleet-tools-dir))))

(defun gptel-fleet-forge-write (id spec)
  "Write tool ID with SPEC into its package file; return the file."
  (let* ((pkg (plist-get spec :package))
         (file (gptel-fleet-forge--package-file pkg))
         (form (gptel-fleet-forge--form-string id spec)))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (when (= (buffer-size) 0)
          (insert (format ";;; %s --- gptel-fleet tool package  -*- lexical-binding: t; -*-\n\n"
                          (file-name-nondirectory file))
                  (format "(gptel-fleet-define-package %s\n  :description %S\n  :level %s)\n"
                          pkg (format "%s tools" pkg) gptel-fleet-forge-default-level)))
        (goto-char (point-min))
        (if (re-search-forward (format "^(gptel-fleet-define-tool %s\\_>"
                                       (regexp-quote (symbol-name id)))
                               nil t)
            (let ((beg (match-beginning 0)))
              (goto-char beg)
              (forward-sexp)
              (delete-region beg (point))
              (insert (string-trim-right form)))
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert "\n" form)))
      (save-buffer)
      file)))

(defun gptel-fleet-forge-save ()
  "Validate, save into the package file, load and register."
  (interactive)
  (pcase-let* ((`(,id . ,spec) (gptel-fleet-forge--read))
               (problems (gptel-fleet-forge-validate id spec))
               (errs (cl-remove-if (lambda (p) (string-prefix-p "warning:" p)) problems)))
    (when errs (user-error "Not saved: %s" (string-join errs "; ")))
    (when (and problems
               (not (y-or-n-p (format "Warnings: %s.  Save anyway? "
                                      (string-join problems "; ")))))
      (user-error "Aborted"))
    (let ((file (gptel-fleet-forge-write id spec)))
      (load file nil t)
      (let ((agents (completing-read-multiple
                     "Add tool to agents (empty: none): "
                     (mapcar #'car (bound-and-true-p gptel-agent--agents)))))
        (dolist (a agents)
          (let ((pkgs (gptel-fleet-config-get a :tool-packages)))
            (unless (memq (plist-get spec :package) pkgs)
              (gptel-fleet-config-set a :tool-packages
                                      (append pkgs (list (plist-get spec :package))))))))
      (when (fboundp 'gptel-fleet-apply) (gptel-fleet-apply))
      (message "Saved %s to %s" (plist-get spec :name) (abbreviate-file-name file)))))

(defun gptel-fleet-forge-abort ()
  "Discard the draft."
  (interactive)
  (when (y-or-n-p "Discard draft? ")
    (kill-buffer)))

;;;; Entry points

(defun gptel-fleet-forge--choose-params (cmd)
  "Ask which tokens of CMD become arguments; return alist index -> name."
  (let* ((core (car (gptel-fleet-forge--strip cmd)))
         (toks (split-string-shell-command (car (gptel-fleet-shell-segments core))))
         (cands (cl-loop for tok in toks for i from 0
                         collect (cons (format "%d: %s" i tok) i)))
         (picked (completing-read-multiple
                  (format "Tokens that become arguments [%s]: "
                          (gptel-fleet--one-line core 60))
                  (cdr cands) nil t)))
    (mapcar (lambda (p)
              (let* ((i (cdr (assoc p cands)))
                     (prev (and (> i 0) (nth (1- i) toks)))
                     (default (if (and prev (string-match-p "\\`--?[A-Za-z]" prev))
                                  (gptel-fleet-forge--flag-name prev)
                                (format "arg%d" i))))
                (cons i (read-string (format "Name for %S: " (nth i toks)) default))))
            picked)))

;;;###autoload
(defun gptel-fleet-forge-from-command (cmd &optional origin)
  "Draft a command tool from shell CMD.  ORIGIN is the source call plist."
  (interactive "sShell command: ")
  (let ((draft (gptel-fleet-forge-draft cmd (gptel-fleet-forge--choose-params cmd))))
    (gptel-fleet-forge-open (car draft) (cdr draft)
                            (or origin (list :tool "Bash" :args (list :command cmd))))))

(defun gptel-fleet-forge--call-at-point ()
  "Parse the gptel tool call block at point: (:name :args :result)."
  (let* ((prop (get-text-property (point) 'gptel)))
    (unless (eq (car-safe prop) 'tool)
      (user-error "Point is not on a tool call block"))
    (let* ((beg (or (previous-single-property-change (1+ (point)) 'gptel) (point-min)))
           (end (or (next-single-property-change (point) 'gptel) (point-max)))
           (text (buffer-substring-no-properties beg end))
           (text (if (derived-mode-p 'org-mode)
                     (progn (require 'org-src) (org-unescape-code-in-string text))
                   text))
           (read-res (read-from-string text))
           (call (car read-res)))
      (list :name (plist-get call :name) :args (plist-get call :args)
            :result (string-trim (substring text (cdr read-res)))))))

;;;###autoload
(defun gptel-fleet-forge-from-call-at-point ()
  "Forge a tool from the tool-call block at point in a gptel buffer."
  (interactive)
  (let* ((call (gptel-fleet-forge--call-at-point))
         (args (plist-get call :args))
         (origin (list :tool (plist-get call :name) :args args
                       :result (plist-get call :result) :source (buffer-name))))
    (if-let* ((cmd (plist-get args :command)))
        (gptel-fleet-forge-from-command cmd origin)
      (gptel-fleet-forge--from-generic call origin))))

(defun gptel-fleet-forge--from-generic (call origin)
  "Draft an Elisp tool that pins some args of a non-shell CALL."
  (let* ((tool (plist-get call :name))
         (id (intern (concat (downcase tool) "-preset")))
         (args (plist-get call :args)))
    (gptel-fleet-forge-open
     id
     (list :package 'presets
           :name (concat tool "Preset")
           :description (format "Calls %s with fixed arguments.  Describe here, for \
the model, when to use it." tool)
           :args nil
           :level gptel-fleet-forge-default-level
           :function `(lambda ()
                        (let ((tool (gptel-fleet-find-tool ,tool))
                              (result nil))
                          ;; Adopted tools are async: collect synchronously.
                          (apply (gptel-tool-function tool)
                                 (lambda (r) (setq result r))
                                 (list ,@(cl-loop for (_k v) on args by #'cddr
                                                  collect `',v)))
                          (or result "started")))
           :examples '(nil))
     origin)))

(defun gptel-fleet--logged-calls (&optional tool)
  "All logged tool-call events (optionally only TOOL) with their results."
  (let (calls)
    (dolist (e (gptel-fleet-log-index))
      (let ((events (gptel-fleet--read-jsonl (alist-get 'file e))) pending)
        (dolist (ev events)
          (pcase (alist-get 'ev ev)
            ("tool-call"
             (when (or (null tool) (equal (alist-get 'tool ev) tool))
               (setq pending (list :tool (alist-get 'tool ev)
                                   :args (alist-get 'args ev)
                                   :agent (alist-get 'agent ev)
                                   :ts (alist-get 'ts ev)))
               (push pending calls)))
            ("tool-result"
             (when (and pending (equal (alist-get 'tool ev) (plist-get pending :tool)))
               (plist-put pending :result (alist-get 'result ev))
               (setq pending nil)))))))
    (nreverse calls)))

(defun gptel-fleet--bash-groups ()
  "Hash signature -> list of logged Bash call plists."
  (let ((h (make-hash-table :test #'equal)))
    (dolist (c (gptel-fleet--logged-calls "Bash"))
      (when-let* ((cmd (alist-get 'command (plist-get c :args))))
        (push (plist-put c :command cmd)
              (gethash (gptel-fleet-forge-signature cmd) h))))
    h))

;;;###autoload
(defun gptel-fleet-forge-from-log ()
  "Forge from a logged Bash call, grouped by signature, most frequent first."
  (interactive)
  (let* ((groups (gptel-fleet--bash-groups))
         (sigs (sort (hash-table-keys groups)
                     (lambda (a b) (> (length (gethash a groups)) (length (gethash b groups))))))
         (cands (mapcar (lambda (s) (cons (format "%4d  %s" (length (gethash s groups)) s) s))
                        sigs))
         (sig (cdr (assoc (completing-read "Signature: " cands nil t) cands)))
         (calls (gethash sig groups))
         (ccands (mapcar (lambda (c) (cons (gptel-fleet--one-line (plist-get c :command) 150) c))
                         calls))
         (call (cdr (assoc (completing-read "Example call: " ccands nil t) ccands))))
    (gptel-fleet-forge-from-command
     (plist-get call :command)
     (list :tool "Bash" :args (list :command (plist-get call :command))
           :result (plist-get call :result) :source "log"))))

;;;###autoload
(defun gptel-fleet-forge-edit-tool (name)
  "Open fleet tool NAME in the forge."
  (interactive (list (completing-read "Fleet tool: " (hash-table-keys gptel-fleet--tool-specs))))
  (let ((spec (gethash name gptel-fleet--tool-specs)))
    (cond ((null spec) (user-error "%s is not a fleet tool; forge from one of its calls instead" name))
          ((plist-get spec :function)
           (find-file (plist-get spec :file))
           (message "Elisp tools are edited in their package file"))
          (t (gptel-fleet-forge-open (plist-get spec :id) spec)))))

;;;###autoload
(defun gptel-fleet-forge-test-tool (name)
  "Open fleet tool NAME in the forge and test it."
  (interactive (list (completing-read "Fleet tool: " (hash-table-keys gptel-fleet--tool-specs))))
  (with-current-buffer (gptel-fleet-forge-edit-tool name)
    (call-interactively #'gptel-fleet-forge-test)))

;;;; Bash statistics

(defvar-keymap gptel-fleet-bash-stats-mode-map
  :parent tabulated-list-mode-map
  "RET" #'gptel-fleet-bash-stats-forge)

(define-derived-mode gptel-fleet-bash-stats-mode tabulated-list-mode "Fleet-Bash"
  "Bash calls across all logs, grouped by command signature.
RET drafts a tool from the group's most recent call.
\\{gptel-fleet-bash-stats-mode-map}"
  (setq tabulated-list-format [("Count" 6 (lambda (a b) (< (string-to-number (aref (cadr a) 0))
                                                           (string-to-number (aref (cadr b) 0))))
                                :right-align t)
                               ("Signature" 30 t) ("Agents" 20 t) ("Latest" 0 nil)])
  (setq tabulated-list-sort-key '("Count" . t))
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;###autoload
(defun gptel-fleet-bash-stats ()
  "Show which Bash commands agents use most: candidates for dedicated tools."
  (interactive)
  (let ((groups (gptel-fleet--bash-groups))
        (buf (get-buffer-create "*gptel-fleet bash stats*")))
    (with-current-buffer buf
      (gptel-fleet-bash-stats-mode)
      (setq tabulated-list-entries
            (cl-loop for sig being the hash-keys of groups using (hash-values calls)
                     collect (list (car calls)
                                   (vector (format "%d" (length calls)) sig
                                           (string-join (delete-dups
                                                         (mapcar (lambda (c) (plist-get c :agent))
                                                                 calls))
                                                        ",")
                                           (gptel-fleet--one-line
                                            (plist-get (car calls) :command) 200)))))
      (tabulated-list-print))
    (pop-to-buffer buf)))

(defun gptel-fleet-bash-stats-forge ()
  "Forge from the group at point."
  (interactive)
  (let ((call (tabulated-list-get-id)))
    (gptel-fleet-forge-from-command
     (plist-get call :command)
     (list :tool "Bash" :args (list :command (plist-get call :command))
           :result (plist-get call :result) :source "log"))))

(provide 'gptel-fleet-forge)
;;; gptel-fleet-forge.el ends here
