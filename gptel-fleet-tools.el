;;; gptel-fleet-tools.el --- Tool packages, guarded tools, command tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Georg Alberding
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Three kinds of tools pass through the approval engine:
;;
;; 1. Tools defined with `gptel-fleet-define-tool' in package files
;;    (<gptel-fleet-directory>/tools/*.el and `gptel-fleet-tool-directories').
;;    Either an Elisp :function, or a :command template that runs as an
;;    argv list via `make-process' -- no shell, no quoting bugs, no
;;    `&&'-chains the guard has to parse.
;;
;; 2. Adopted tools: every other gptel tool (gptel-agent's Read/Bash/...,
;;    MCP tools).  Adopting swaps the struct's :function for a guarded
;;    wrapper and sets :confirm nil (gptel's own confirmation is replaced,
;;    not stacked).  The level defaults to what the tool's original
;;    :confirm slot said, so adopting changes nothing until you add rules.
;;
;; 3. `gptel-fleet-builtin-packages' groups gptel-agent's tools into
;;    packages so rules can say `:package fs-write' instead of listing names.
;;
;; A package file looks like:
;;
;;   (gptel-fleet-define-package cargo
;;     :description "Rust builds and tests"
;;     :level confirm)
;;
;;   (gptel-fleet-define-tool cargo-test
;;     :package cargo
;;     :name "CargoTest"
;;     :description "Run cargo test.  Output is the last 200 lines."
;;     :args ((:name "package" :type string :optional t
;;             :description "Crate to test (-p)")
;;            (:name "filter" :type string :optional t
;;             :description "Test name filter"))
;;     :level notify
;;     :command ("cargo" "test" (:opt "-p" package) "--" filter)
;;     :cwd project
;;     :timeout 900
;;     :tail 200
;;     :examples ((:package "fleet-core" :filter "policy")))
;;
;; Everything except :function is literal data (not evaluated), so the
;; forge can read, edit and write these forms back.
;;
;; Command template elements:
;;   "lit"               literal argument
;;   ARG                 value of the arg of that name; omitted if nil/""
;;                       lists are spliced; a value starting with "-" is
;;                       rejected unless the arg spec has :allow-dash t
;;   (:opt "-p" ARG)     "-p" VALUE, only if ARG is set
;;   (:flag "--x" ARG)   "--x" if ARG is true
;;   (:split ARG)        ARG split like a shell would (no expansion)
;;   (:fmt "f%s" ARG..)  one argument from a format string
;;
;; Command tool keys:
;;   :cwd     project | default | "dir" | ARG     (working directory)
;;   :timeout seconds (default `gptel-fleet-command-timeout')
;;   :tail N  keep last N lines   :head N  keep first N lines
;;   :max-output CHARS
;;   :stderr  merge (default) | drop
;;   :env     ("K=V" ...)     added to the environment
;;   :inherit-env nil         start from `gptel-fleet-minimal-env' only
;;                            (keeps tokens away from tools that need none)
;;   :sandbox t | FUNCTION    wrap argv (default `gptel-fleet-sandbox-function')

;;; Code:

(require 'gptel-fleet-core)
(require 'gptel-fleet-policy)
(require 'gptel-fleet-log)
(require 'gptel-fleet-approval)
(require 'project)

(defcustom gptel-fleet-adopt-tools 'all
  "Which existing gptel tools are put under fleet approval.
all: every tool (including MCP tools registered later).
A list of names: only those."
  :group 'gptel-fleet
  :type '(choice (const all) (repeat string)))

(defcustom gptel-fleet-builtin-packages
  '((fs-read :description "Read-only file access"
             :tools ("Read" "Glob" "Grep"))
    (fs-write :description "Create and modify files"
              :tools ("Write" "Edit" "Insert" "Mkdir"))
    (web :description "Web search and fetch (prompt-injection surface)"
         :tools ("WebSearch" "WebFetch"))
    (shell :description "Unrestricted shell"
           :tools ("Bash"))
    (emacs :description "Evaluate Elisp, read diagnostics"
           :tools ("Eval" "Diagnostics"))
    (emacs-introspection
     :description "Read-only Emacs introspection"
     :tools ("symbol_exists" "load_paths" "manual_names" "manual_nodes"
             "manual_node_contents" "features" "library_source"
             "symbol_manual_section" "function_source" "variable_source"
             "variable_value" "function_documentation" "variable_documentation"
             "function_completions" "command_completions" "variable_completions"))
    (orchestration :description "Delegation, todo list, skills"
                   :tools ("Agent" "TodoWrite" "Skill")))
  "Packages grouping existing tools.  A :level here overrides the
per-tool default derived from the tool's original :confirm slot."
  :group 'gptel-fleet
  :type '(alist :key-type symbol :value-type plist))

(defcustom gptel-fleet-command-timeout 300
  "Default timeout in seconds for command tools."
  :group 'gptel-fleet
  :type 'integer)

(defcustom gptel-fleet-minimal-env
  '("PATH" "HOME" "LANG" "LC_ALL" "USER" "TERM" "TMPDIR")
  "Variables passed to command tools that set :inherit-env nil."
  :group 'gptel-fleet
  :type '(repeat string))

(defcustom gptel-fleet-sandbox-function #'gptel-fleet-sandbox-bwrap
  "Function (ARGV CWD) -> ARGV used for tools with :sandbox t."
  :group 'gptel-fleet
  :type 'function)

(defcustom gptel-fleet-bwrap-extra-args '("--unshare-net")
  "Extra bubblewrap arguments for `gptel-fleet-sandbox-bwrap'."
  :group 'gptel-fleet
  :type '(repeat string))

;;;; Registry of fleet-defined tools

(defvar gptel-fleet--tool-specs (make-hash-table :test #'equal)
  "Tool name -> spec plist (as written in the package file, plus :file).")

(defvar gptel-fleet--creating nil
  "Non-nil while fleet creates a tool (suppresses auto-adoption).")

;;;; Guard

(defun gptel-fleet--deny-message (agent tool-name source reason)
  "Error string for a denied call."
  (format "Error: tool call %s was denied for agent \"%s\" by policy (%s).%s \
Do not retry the same call; choose another approach or explain in your answer \
what you would need."
          tool-name agent (gptel-fleet-describe-source source)
          (if reason (concat " Reason: " reason) "")))

(defun gptel-fleet--escalate-message (agent tool-name args)
  "Text telling a subagent to hand the call to its parent."
  (format "Not executed: agent \"%s\" may not call %s on its own (escalate). \
Continue without it where possible.  If the call is essential, finish your \
task and append to your final report exactly one block per needed call:
<escalation>
tool: %s
args: %s
why: <one sentence>
</escalation>
The orchestrating agent will decide."
          agent tool-name tool-name (prin1-to-string args)))

(defun gptel-fleet--execute (ctx buffer impl cb arg-values)
  "Run IMPL with CB and ARG-VALUES in BUFFER under CTX."
  (with-current-buffer (if (buffer-live-p buffer) buffer (current-buffer))
    (let ((gptel-fleet--context (unless (plist-get ctx :main) ctx))
          (gptel-fleet--acting ctx))
      (condition-case err
          (apply impl cb arg-values)
        (error (funcall cb (let ((msg (error-message-string err)))
                             ;; gptel-agent's tools already say "Error: ..."
                             (if (string-prefix-p "Error" msg) msg
                               (format "Error: %s" msg)))))))))

(defun gptel-fleet-guard (tool-name impl)
  "Return an async tool function enforcing policy for TOOL-NAME.
IMPL is called as (IMPL CALLBACK &rest ARG-VALUES).

When `gptel-fleet--decide-hook' already decided this exact call (tool
name + argument values) before gptel ran it, that decision is used,
including the agent identity: calls approved through gptel's inline
confirmation run later, outside the subagent's dynamic context."
  (lambda (cb &rest arg-values)
    (let* ((token (gptel-fleet--take-token tool-name arg-values))
           (ctx (or (plist-get token :ctx) (gptel-fleet-current-context)))
           (agent (plist-get ctx :agent))
           (buffer (current-buffer))
           (tool (gptel-fleet-find-tool tool-name))
           (args (if tool (gptel-fleet--arg-plist tool arg-values) arg-values))
           (decision (if token
                         (cons (plist-get token :level) (plist-get token :source))
                       (gptel-fleet-decide agent tool-name args (plist-get ctx :main))))
           (level (car decision))
           (source (cdr decision))
           (inline-ok (and (eq level 'confirm) (plist-get token :inline))))
      (gptel-fleet-log-event ctx "decision"
                             (list :tool tool-name :args args
                                   :level (if inline-ok "confirm→approved inline" level)
                                   :source (gptel-fleet-describe-source source)))
      (pcase (if inline-ok 'allow level)
        ('allow (gptel-fleet--execute ctx buffer impl cb arg-values))
        ('notify
         (message "gptel-fleet: %s → %s %s" agent tool-name
                  (gptel-fleet--one-line (prin1-to-string args) 100))
         (gptel-fleet--execute ctx buffer impl cb arg-values))
        ('dry-run
         (funcall cb (format "[dry-run] %s would have been called with %s. \
It was NOT executed; continue as if you had to plan without its result."
                             tool-name (prin1-to-string args))))
        ('deny (funcall cb (gptel-fleet--deny-message agent tool-name source nil)))
        ('escalate (funcall cb (gptel-fleet--escalate-message agent tool-name args)))
        ('confirm
         (gptel-fleet-enqueue
          (gptel-fleet-request-create
           :id (gptel-fleet-new-id "req-") :time (current-time) :ctx ctx
           :tool-name tool-name :args args :arg-values arg-values :source source
           :run-fn (lambda (vals &optional note)
                     (gptel-fleet--execute
                      ctx buffer impl
                      (if note (lambda (r) (funcall cb (concat note (format "%s" r)))) cb)
                      vals))
           :deny-fn (lambda (reason)
                      (funcall cb (gptel-fleet--deny-message
                                   agent tool-name '(human) reason))))))))))

;;;; Deciding before gptel runs a call (inline confirmation)

(defun gptel-fleet--managed-p (name)
  "Non-nil if tool NAME is guarded by fleet."
  (or (gethash name gptel-fleet--adopted) (gethash name gptel-fleet--tool-specs)))

(defun gptel-fleet--decide-hook (call)
  "`gptel-pre-tool-call-functions' entry: decide CALL with the right agent.
Records the decision as a token for the guard.  For `confirm', with
`gptel-fleet-approval-ui' set to `inline', asks gptel to show its own
inline confirmation (:confirm t); otherwise tells gptel not to ask, since
fleet decides (:confirm nil)."
  (let* ((name (plist-get call :name))
         (tool (gptel-fleet-find-tool name)))
    (when (and tool (gptel-fleet--managed-p name))
      (let* ((args (plist-get call :args))
             (values (gptel-fleet--values-from-args tool args))
             (ctx (gptel-fleet-current-context))
             (decision (gptel-fleet-decide (plist-get ctx :agent) name
                                           (gptel-fleet--arg-plist tool values)
                                           (plist-get ctx :main)))
             (question (and (equal name "AskUser")
                            (memq (car decision) '(confirm notify allow))))
             (inline (and (or question (eq (car decision) 'confirm))
                          (eq gptel-fleet-approval-ui 'inline))))
        ;; A question the agent may ask is answered, not approved: it runs
        ;; as `allow', and gptel's inline prompt only hosts the answer UI.
        (when question (setq decision (cons 'allow (cdr decision))))
        (gptel-fleet--put-token name values
                                (list :ctx ctx :level (car decision) :source (cdr decision)
                                      :inline (and inline (not question))
                                      :question (and inline question)
                                      :time (current-time)
                                      :buffer (current-buffer)))
        (when inline (run-at-time 0 nil #'gptel-fleet-update-indicator))
        (list :confirm (and inline t))))))

(defun gptel-fleet--forget-calls-advice (&optional tool-calls &rest _)
  "After gptel rejects/steers TOOL-CALLS: drop their tokens, refresh."
  (dolist (tc tool-calls)
    (when (and (consp tc) (gptel-tool-p (car tc)))
      (gptel-fleet--take-token
       (gptel-tool-name (car tc))
       (gptel-fleet--values-from-args (car tc) (cadr tc)))))
  (run-at-time 0 nil #'gptel-fleet-update-indicator))

(defun gptel-fleet--refresh-advice (&rest _)
  "After gptel accepts calls: refresh queue and mode line."
  (run-at-time 0 nil #'gptel-fleet-update-indicator))

;;;; Adoption of existing tools

(defun gptel-fleet--adoptable-p (tool)
  "Non-nil if TOOL should be adopted."
  (let ((name (gptel-tool-name tool)))
    (and (not (gethash name gptel-fleet--tool-specs))
         (not (gethash name gptel-fleet--adopted))
         (or (eq gptel-fleet-adopt-tools 'all)
             (member name gptel-fleet-adopt-tools)))))

(defun gptel-fleet-adopt-tool (tool)
  "Put TOOL (a struct) under fleet approval."
  (when (gptel-fleet--adoptable-p tool)
    (let* ((name (gptel-tool-name tool))
           (fn (gptel-tool-function tool))
           (async (gptel-tool-async tool))
           (impl (if async fn
                   (lambda (cb &rest args) (funcall cb (apply fn args))))))
      (puthash name (list :function fn :async async :confirm (gptel-tool-confirm tool)
                          :struct tool)
               gptel-fleet--adopted)
      (setf (gptel-tool-function tool) (gptel-fleet-guard name impl)
            (gptel-tool-async tool) t
            (gptel-tool-confirm tool) nil)
      tool)))

(defun gptel-fleet-unadopt-all ()
  "Restore all adopted tools to their original functions."
  (maphash (lambda (_name orig)
             (let ((tool (plist-get orig :struct)))
               (setf (gptel-tool-function tool) (plist-get orig :function)
                     (gptel-tool-async tool) (plist-get orig :async)
                     (gptel-tool-confirm tool) (plist-get orig :confirm))))
           gptel-fleet--adopted)
  (clrhash gptel-fleet--adopted))

(defcustom gptel-fleet-auto-packages t
  "Put tools that belong to no package into a package named after their
gptel category.  MCP tools carry their server name as category, so a
GitLab MCP server becomes a `gitlab' package you can hand to one agent."
  :group 'gptel-fleet
  :type 'boolean)

(defun gptel-fleet--auto-package (tool)
  "Add TOOL to the package named after its category, if it has none."
  (when (and gptel-fleet-auto-packages
             (not (gptel-fleet-tool-package (gptel-tool-name tool))))
    (let ((pkg (intern (or (gptel-tool-category tool) "misc"))))
      (unless (gptel-fleet-package pkg)
        (gptel-fleet-register-package
         pkg :origin 'category
         :description (format "Tools of gptel category \"%s\"" pkg)))
      (gptel-fleet-package-add-tool pkg (gptel-tool-name tool)))))

(defun gptel-fleet-adopt-all ()
  "Adopt every registered tool allowed by `gptel-fleet-adopt-tools'."
  (mapc #'gptel-fleet-adopt-tool (gptel-fleet-all-tools))
  (pcase-dolist (`(,pkg . ,props) gptel-fleet-builtin-packages)
    (apply #'gptel-fleet-register-package pkg :origin 'builtin props))
  (mapc #'gptel-fleet--auto-package (gptel-fleet-all-tools)))

(defun gptel-fleet--make-tool-advice (tool)
  "Filter-return advice for `gptel-make-tool': adopt new TOOL."
  (when (and (not gptel-fleet--creating) (gptel-tool-p tool))
    ;; A redefinition replaces the struct: forget the stale adoption.
    (when-let* ((old (gethash (gptel-tool-name tool) gptel-fleet--adopted)))
      (unless (eq (plist-get old :struct) tool)
        (remhash (gptel-tool-name tool) gptel-fleet--adopted)))
    (gptel-fleet-adopt-tool tool)
    (gptel-fleet--auto-package tool))
  tool)

;;;; Command tools

(defun gptel-fleet--arg-value (sym values)
  "Value of arg SYM in VALUES alist (name-string . value)."
  (cdr (assoc (symbol-name sym) values)))


(defun gptel-fleet-render-command (template values arg-specs)
  "Expand TEMPLATE with VALUES (alist name . value) into an argv list.
ARG-SPECS is the tool's :args, consulted for :allow-dash."
  (cl-flet ((check (sym v)
              (when (and (stringp v) (string-prefix-p "-" v)
                         (not (plist-get (cl-find (symbol-name sym) arg-specs
                                                  :key (lambda (s) (plist-get s :name))
                                                  :test #'equal)
                                         :allow-dash)))
                (error "Argument %s may not start with \"-\": %S" sym v))
              v))
    (cl-loop
     for el in template
     append
     (pcase el
       ((pred stringp) (list el))
       ((pred symbolp)
        (let ((v (gptel-fleet--arg-value el values)))
          (cond ((not (gptel-fleet--set-p v)) nil)
                ((or (vectorp v) (consp v))
                 (mapcar (lambda (x) (check el (format "%s" x))) (append v nil)))
                (t (list (check el (format "%s" v)))))))
       (`(:opt ,flag ,sym)
        (let ((v (gptel-fleet--arg-value sym values)))
          (and (gptel-fleet--set-p v) (list flag (format "%s" v)))))
       (`(:flag ,flag ,sym)
        (and (gptel-fleet--set-p (gptel-fleet--arg-value sym values)) (list flag)))
       (`(:split ,sym)
        (let ((v (gptel-fleet--arg-value sym values)))
          (and (gptel-fleet--set-p v) (split-string-shell-command (format "%s" v)))))
       (`(:fmt ,fmt . ,syms)
        (list (apply #'format fmt (mapcar (lambda (s) (or (gptel-fleet--arg-value s values) ""))
                                          syms))))
       (_ (error "Bad command template element: %S" el))))))

(defun gptel-fleet--command-cwd (spec values)
  "Working directory for command SPEC with VALUES."
  (let ((cwd (plist-get spec :cwd)))
    (expand-file-name
     (pcase cwd
       ((or 'nil 'default) default-directory)
       ('project (or (and-let* ((p (project-current))) (project-root p))
                     default-directory))
       ((pred stringp) cwd)
       ((pred symbolp) (or (gptel-fleet--arg-value cwd values) default-directory))
       (_ default-directory)))))

(defun gptel-fleet--command-env (spec)
  "Environment for command SPEC."
  (append (plist-get spec :env)
          (if (eq (plist-get spec :inherit-env) nil)
              (if (plist-member spec :inherit-env)
                  (cl-loop for v in gptel-fleet-minimal-env
                           for val = (getenv v)
                           when val collect (concat v "=" val))
                process-environment)
            process-environment)))

(defun gptel-fleet-sandbox-bwrap (argv cwd)
  "Wrap ARGV in bubblewrap: read-only root, writable CWD and /tmp."
  (unless (executable-find "bwrap")
    (error "bwrap not found; install bubblewrap or change gptel-fleet-sandbox-function"))
  (append (list "bwrap" "--ro-bind" "/" "/" "--dev" "/dev" "--proc" "/proc"
                "--tmpfs" "/tmp" "--bind" cwd cwd "--chdir" cwd
                "--die-with-parent")
          gptel-fleet-bwrap-extra-args
          (list "--") argv))

(defun gptel-fleet--shape-output (spec out)
  "Apply :head/:tail/:max-output of SPEC to OUT."
  (let* ((lines (split-string out "\n"))
         (n (length lines)))
    (when-let* ((tail (plist-get spec :tail)))
      (when (> n tail)
        (setq lines (cons (format "…[%d lines omitted]" (- n tail))
                          (last lines tail)))))
    (when-let* ((head (plist-get spec :head)))
      (when (> (length lines) head)
        (setq lines (append (cl-subseq lines 0 head)
                            (list (format "…[%d lines omitted]" (- (length lines) head)))))))
    (gptel-fleet--truncate (string-join lines "\n") (plist-get spec :max-output))))

(defun gptel-fleet-run-command (spec values cb)
  "Run command SPEC with VALUES (alist name . value); call CB with the result."
  (let* ((argv (gptel-fleet-render-command (plist-get spec :command) values
                                           (plist-get spec :args)))
         (cwd (file-name-as-directory (gptel-fleet--command-cwd spec values)))
         (sandbox (plist-get spec :sandbox))
         (argv (cond ((functionp sandbox) (funcall sandbox argv cwd))
                     (sandbox (funcall gptel-fleet-sandbox-function argv cwd))
                     (t argv)))
         (buf (generate-new-buffer " *gptel-fleet-cmd*"))
         (start (float-time))
         (done nil)
         (timer nil)
         (default-directory cwd)
         (process-environment (gptel-fleet--command-env spec))
         (finish
          (lambda (status)
            (unless done
              (setq done t)
              (when timer (cancel-timer timer))
              (let ((out (with-current-buffer buf (buffer-string))))
                (kill-buffer buf)
                (funcall cb (format "$ %s\n[%s, %.1fs, cwd %s]\n%s"
                                    (mapconcat #'shell-quote-argument argv " ")
                                    status (- (float-time) start)
                                    (abbreviate-file-name cwd)
                                    (gptel-fleet--shape-output spec out))))))))
    (unless (executable-find (car argv))
      (kill-buffer buf)
      (error "Program not found: %s" (car argv)))
    (let ((proc (make-process
                 :name "gptel-fleet-cmd" :buffer buf :command argv
                 :connection-type 'pipe :noquery t
                 :stderr (when (eq (plist-get spec :stderr) 'drop)
                           (make-pipe-process :name "gptel-fleet-null"
                                              :buffer nil :noquery t
                                              :filter #'ignore))
                 :sentinel (lambda (p _e)
                             (unless (process-live-p p)
                               (funcall finish (format "exit %d" (process-exit-status p))))))))
      (setq timer (run-at-time (or (plist-get spec :timeout) gptel-fleet-command-timeout)
                               nil (lambda ()
                                     (when (process-live-p proc)
                                       (delete-process proc)
                                       (funcall finish "TIMEOUT, killed")))))
      proc)))

;;;; Defining tools

(defun gptel-fleet--camel (sym)
  "CamelCase name from SYM (cargo-test -> CargoTest)."
  (mapconcat #'capitalize (split-string (symbol-name sym) "[-_]") ""))

(defun gptel-fleet--impl (spec)
  "Async implementation (CB &rest ARG-VALUES) for SPEC."
  (let ((names (mapcar (lambda (a) (plist-get a :name)) (plist-get spec :args))))
    (cond
     ((plist-get spec :command)
      (lambda (cb &rest vals)
        (gptel-fleet-run-command spec (cl-mapcar #'cons names vals) cb)))
     ((plist-get spec :async) (plist-get spec :function))
     ((plist-get spec :function)
      (let ((fn (plist-get spec :function)))
        (lambda (cb &rest vals) (funcall cb (apply fn vals)))))
     (t (error "Tool spec needs :command or :function")))))

(defun gptel-fleet--gptel-args (args)
  "Strip fleet-only keys from ARGS for gptel."
  (mapcar (lambda (a)
            (cl-loop for (k v) on a by #'cddr
                     unless (memq k '(:allow-dash)) append (list k v)))
          args))

(defun gptel-fleet-register-tool (id spec)
  "Create and register the gptel tool ID described by SPEC.  Return the struct."
  (let* ((name (or (plist-get spec :name) (gptel-fleet--camel id)))
         (pkg (or (plist-get spec :package) 'misc))
         (spec (plist-put (copy-sequence spec) :name name))
         (spec (plist-put spec :id id))
         (gptel-fleet--creating t)
         (tool (apply #'gptel-make-tool
                      :name name
                      :description (or (plist-get spec :description) name)
                      :args (gptel-fleet--gptel-args (plist-get spec :args))
                      :category (symbol-name pkg)
                      :async t
                      :confirm nil
                      :function (gptel-fleet-guard name (gptel-fleet--impl spec))
                      (and (plist-member spec :include)
                           (list :include (plist-get spec :include))))))
    (puthash name spec gptel-fleet--tool-specs)
    (unless (gptel-fleet-package pkg)
      (gptel-fleet-register-package pkg :description "" :origin 'file))
    (gptel-fleet-package-add-tool pkg name (plist-get spec :level))
    tool))

(defmacro gptel-fleet-define-package (name &rest props)
  "Define tool package NAME with literal PROPS (:description :level ...)."
  (declare (indent 1))
  `(gptel-fleet-register-package ',name :origin 'file
                                 :file (or load-file-name buffer-file-name)
                                 ,@(cl-loop for (k v) on props by #'cddr
                                            append (list k `',v))))

(defmacro gptel-fleet-define-tool (id &rest spec)
  "Define fleet tool ID.  SPEC is literal data except :function (evaluated).
See the commentary of gptel-fleet-tools.el for the keys."
  (declare (indent 1))
  `(gptel-fleet-register-tool
    ',id
    (list ,@(cl-loop for (k v) on spec by #'cddr
                     append (list k (if (eq k :function) v `',v)))
          :file (or load-file-name buffer-file-name))))

(defun gptel-fleet-tool-directories ()
  "All directories scanned for tool packages."
  (cons (gptel-fleet-tools-dir) gptel-fleet-tool-directories))

(defun gptel-fleet-load-tool-packages ()
  "Load every package file."
  (dolist (dir (gptel-fleet-tool-directories))
    (when (file-directory-p dir)
      (dolist (f (directory-files dir t "\\`[^.#].*\\.el\\'"))
        (condition-case err
            (load f nil t)
          (error (display-warning 'gptel-fleet
                                  (format "Loading %s failed: %S" f err))))))))

(provide 'gptel-fleet-tools)
;;; gptel-fleet-tools.el ends here
