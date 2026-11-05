;;; gptel-fleet-log.el --- Per-agent JSONL logs and run reconstruction  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Georg Alberding
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Every agent run gets its own JSONL file:
;;
;;   <log-dir of agent>/<YYYY-MM-DD>/<run-id>.jsonl
;;
;; where the log dir is the agent's :log-dir override or
;; <log-root>/<agent>/.  A run is one subagent task, or one chat buffer
;; for a main agent.  <log-root>/index.jsonl gets one line per run start
;; (run, parent, agent, file), so runs are found even in custom dirs.
;;
;; Event types (key "ev"):
;;   run-start   agent parent description prompt backend model tools system_sha
;;   prompt      text                       (main agents, on `gptel-send')
;;   spawn       child agent description    (in the parent's file)
;;   decision    tool args level source     (managed tools)
;;   tool-call   tool args                  (every tool, via gptel hooks)
;;   tool-result tool result                (every tool)
;;   request     data                       (optional full payloads)
;;   response    text                       (main agents)
;;   spawn-end   child status               (parent's file)
;;   run-end     status result              (subagent)
;;
;; System prompts are stored once, content-addressed, in
;; <log-root>/prompts/<sha1>.md, and referenced by system_sha.
;;
;; How identity reaches the log: `gptel-agent--task' is advised to
;; create a child context, and `gptel-request' is advised to stamp that
;; context into the new FSM's info plist (:gptel-fleet).  The handlers in
;; `gptel-agent-request--handlers' are wrapped so that each one runs with
;; `gptel-fleet--context' bound from the FSM.  Tool functions, the
;; pre/post tool-call hooks and nested Agent calls all see it.  In a
;; main chat buffer, with nothing bound, the buffer's own context is used.

;;; Code:

(require 'gptel-fleet-core)
(require 'json)

(defcustom gptel-fleet-log-enabled t
  "Global switch for logging.  Agents can opt out with :log nil."
  :group 'gptel-fleet
  :type 'boolean)

(defcustom gptel-fleet-log-payloads nil
  "If non-nil, log the full request payload sent on each subagent turn.
Complete but large: the payload grows with every tool round trip."
  :group 'gptel-fleet
  :type 'boolean)

(defcustom gptel-fleet-log-max-result 20000
  "Truncate logged tool results and texts to this many characters.  nil: never."
  :group 'gptel-fleet
  :type '(choice (const nil) integer))

(defvar gptel-fleet--context nil
  "Dynamically bound plist describing the running agent:
:agent NAME :run RUN-ID :parent RUN-ID :file LOG-FILE :main BOOL.")

(defvar-local gptel-fleet--buffer-context nil
  "Context of the main agent run in this chat buffer.")

(defvar gptel-fleet--acting nil
  "Context of the agent whose tool call is executing right now, main or sub.
Bound only while a guarded tool runs, so API functions can tell an agent
request from your own interactive call.")

(defvar gptel-fleet--spawning nil
  "Cons cell (CONTEXT) consumed by the next `gptel-request' to tag its FSM.")

;;;; Context

(defun gptel-fleet--buffer-agent ()
  "Name of the main agent driving the current buffer."
  (let ((p (bound-and-true-p gptel--preset)))
    (cond ((null p) "main")
          ((symbolp p) (symbol-name p))
          ((stringp p) p)
          (t "main"))))

(defun gptel-fleet--agent-log-dir (agent)
  "Log directory for AGENT."
  (file-name-as-directory
   (expand-file-name
    (or (gptel-fleet-config-get agent :log-dir)
        (expand-file-name (gptel-fleet--safe-name agent) (gptel-fleet-log-root))))))

(defun gptel-fleet--make-context (agent &optional parent main extra)
  "New run context for AGENT with PARENT run id.  MAIN marks top level.
EXTRA is a plist added to the run-start event."
  (let* ((run (gptel-fleet-new-id))
         (dir (expand-file-name (format-time-string "%Y-%m-%d")
                                (gptel-fleet--agent-log-dir agent)))
         (file (expand-file-name (concat run ".jsonl") dir))
         (ctx (list :agent agent :run run :parent parent :file file :main main)))
    (gptel-fleet-log-event ctx "run-start"
                           (append (list :agent agent :parent parent) extra))
    (gptel-fleet--index ctx extra)
    ctx))

(defun gptel-fleet-current-context ()
  "The context of whoever is running now; create a main one if needed."
  (or gptel-fleet--context
      gptel-fleet--buffer-context
      (setq gptel-fleet--buffer-context
            (gptel-fleet--make-context
             (gptel-fleet--buffer-agent) nil t
             (list :buffer (buffer-name)
                   :buffer_file (buffer-file-name)
                   :backend (and (boundp 'gptel-backend) gptel-backend
                                 (gptel-backend-name gptel-backend))
                   :model (and (boundp 'gptel-model) (format "%s" gptel-model)))))))

(defun gptel-fleet-new-buffer-run ()
  "Start a fresh main-agent run (new log file) for this buffer."
  (interactive)
  (setq gptel-fleet--buffer-context nil)
  (message "gptel-fleet: next event starts a new run"))

;;;; Writing

(defun gptel-fleet--log-enabled-p (ctx)
  "Non-nil if CTX's agent should be logged."
  (and gptel-fleet-log-enabled ctx
       (gptel-fleet-config-get (plist-get ctx :agent) :log t)))

(defun gptel-fleet--jsonable (v)
  "Turn V into something `json-encode' handles predictably."
  (cond ((or (null v) (eq v t) (numberp v) (stringp v)) v)
        ((keywordp v) (substring (symbol-name v) 1))
        ((symbolp v) (symbol-name v))
        ((and (consp v) (keywordp (car v)) (plistp v))
         (cl-loop for (k val) on v by #'cddr
                  collect (cons (substring (symbol-name k) 1)
                                (gptel-fleet--jsonable val))))
        ((proper-list-p v) (vconcat (mapcar #'gptel-fleet--jsonable v)))
        ((vectorp v) (vconcat (mapcar #'gptel-fleet--jsonable v)))
        (t (format "%S" v))))

(defun gptel-fleet--append-line (file obj)
  "Append OBJ as one JSON line to FILE."
  (make-directory (file-name-directory file) t)
  (let ((line (concat (let ((json-encoding-pretty-print nil))
                        (json-encode obj))
                      "\n"))
        (coding-system-for-write 'utf-8-unix)
        (inhibit-message t)
        (message-log-max nil))
    (write-region line nil file 'append 'silent)))

(defun gptel-fleet-log-event (ctx type &optional data)
  "Log event TYPE with DATA (plist) into CTX's run file."
  (when (gptel-fleet--log-enabled-p ctx)
    (condition-case err
        (gptel-fleet--append-line
         (plist-get ctx :file)
         (append (list (cons "ts" (gptel-fleet--now))
                       (cons "ev" type)
                       (cons "run" (plist-get ctx :run))
                       (cons "agent" (plist-get ctx :agent)))
                 (gptel-fleet--jsonable data)))
      (error (message "gptel-fleet: log error %S" err)))))

(defun gptel-fleet--index (ctx extra)
  "Add CTX to the run index, with EXTRA (plist) fields."
  (when (gptel-fleet--log-enabled-p ctx)
    (gptel-fleet--append-line
     (expand-file-name "index.jsonl" (gptel-fleet-log-root))
     (append (list (cons "ts" (gptel-fleet--now))
                   (cons "run" (plist-get ctx :run))
                   (cons "parent" (plist-get ctx :parent))
                   (cons "agent" (plist-get ctx :agent))
                   (cons "file" (plist-get ctx :file)))
             (gptel-fleet--jsonable
              (cl-loop for (k v) on extra by #'cddr
                       when (memq k '(:description :buffer)) append (list k v)))))))

(defun gptel-fleet--store-prompt (text)
  "Store system prompt TEXT content-addressed; return its sha1."
  (when (stringp text)
    (let* ((sha (secure-hash 'sha1 text))
           (file (expand-file-name (concat sha ".md")
                                   (expand-file-name "prompts" (gptel-fleet-log-root)))))
      (unless (file-exists-p file)
        (make-directory (file-name-directory file) t)
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region text nil file nil 'silent)))
      sha)))

;;;; Advice: subagent tasks

(defun gptel-fleet--task-advice (orig main-cb agent-type description prompt)
  "Around advice for `gptel-agent--task'.
ORIG is called with a logging MAIN-CB and the original AGENT-TYPE,
DESCRIPTION and PROMPT, under a fresh child context."
  (let* ((parent (gptel-fleet-current-context))
         (spec (cdr (assoc agent-type (bound-and-true-p gptel-agent--agents))))
         (child (gptel-fleet--make-context
                 agent-type (plist-get parent :run) nil
                 (list :description description
                       :prompt prompt
                       :backend (let ((b (plist-get spec :backend)))
                                  (cond ((stringp b) b)
                                        ((and b (gptel-backend-p b)) (gptel-backend-name b))
                                        ((and (boundp 'gptel-backend) gptel-backend)
                                         (gptel-backend-name gptel-backend))))
                       :model (format "%s" (or (plist-get spec :model)
                                               (bound-and-true-p gptel-model)))
                       :model_override (plist-get gptel-fleet--model-override :requested)
                       :tools (plist-get spec :tools)
                       :system_sha (gptel-fleet--store-prompt (plist-get spec :system)))))
         (logged-cb
          (lambda (result)
            (let ((status (if (and (stringp result) (string-prefix-p "Error:" result))
                              "error" "ok")))
              (gptel-fleet-log-event
               child "run-end"
               (list :status status
                     :result (gptel-fleet--truncate result gptel-fleet-log-max-result)))
              (gptel-fleet-log-event parent "spawn-end"
                                     (list :child (plist-get child :run) :status status)))
            (funcall main-cb result))))
    (setq child (plist-put child :parent-agent (plist-get parent :agent)))
    (gptel-fleet-log-event parent "spawn"
                           (list :child (plist-get child :run)
                                 :child_agent agent-type
                                 :description description))
    (let ((gptel-fleet--spawning (list child))
          (gptel-fleet--context child))
      (funcall orig logged-cb agent-type description prompt))))

(defun gptel-fleet--request-advice (orig &rest args)
  "Around advice for `gptel-request' (ORIG, ARGS): tag subagent FSMs."
  (let* ((tag (and gptel-fleet--spawning (car gptel-fleet--spawning)))
         (fsm (progn (when tag (setcar gptel-fleet--spawning nil))
                     (apply orig args))))
    (when (and tag (gptel-fsm-p fsm))
      (let ((info (gptel-fsm-info fsm)))
        (if info
            (plist-put info :gptel-fleet tag)
          (setf (gptel-fsm-info fsm) (list :gptel-fleet tag)))))
    fsm))

;;;; Handler wrapping

(defvar gptel-fleet--orig-agent-handlers nil
  "Value of `gptel-agent-request--handlers' before wrapping.")

(defun gptel-fleet--wrap-handler (fn)
  "Return a handler running FN with the FSM's fleet context bound."
  (lambda (fsm)
    (let ((gptel-fleet--context
           (or (plist-get (gptel-fsm-info fsm) :gptel-fleet) gptel-fleet--context)))
      (funcall fn fsm))))

(defun gptel-fleet--log-wait (fsm)
  "WAIT handler: optionally log the request payload of FSM."
  (when gptel-fleet-log-payloads
    (let ((info (gptel-fsm-info fsm)))
      (when-let* ((ctx (plist-get info :gptel-fleet)))
        (gptel-fleet-log-event ctx "request" (list :data (plist-get info :data)))))))

(defun gptel-fleet-install-handlers ()
  "Wrap `gptel-agent-request--handlers' so subagent handlers know their agent."
  (when (and (boundp 'gptel-agent-request--handlers)
             (not gptel-fleet--orig-agent-handlers))
    (setq gptel-fleet--orig-agent-handlers gptel-agent-request--handlers)
    (setq gptel-agent-request--handlers
          (cl-loop for (state . fns) in gptel-agent-request--handlers
                   collect (cons state
                                 (append
                                  (and (eq state 'WAIT) (list #'gptel-fleet--log-wait))
                                  (mapcar #'gptel-fleet--wrap-handler fns)))))))

(defun gptel-fleet-uninstall-handlers ()
  "Restore the original subagent handlers."
  (when gptel-fleet--orig-agent-handlers
    (setq gptel-agent-request--handlers gptel-fleet--orig-agent-handlers
          gptel-fleet--orig-agent-handlers nil)))

;;;; Hooks

(defun gptel-fleet--pre-tool-hook (call)
  "Log a tool CALL (plist from `gptel-pre-tool-call-functions')."
  (gptel-fleet-log-event (gptel-fleet-current-context) "tool-call"
                         (list :tool (plist-get call :name)
                               :args (plist-get call :args)))
  nil)

(defun gptel-fleet--post-tool-hook (call)
  "Log a tool result (CALL plist from `gptel-post-tool-call-functions')."
  (gptel-fleet-log-event (gptel-fleet-current-context) "tool-result"
                         (list :tool (plist-get call :name)
                               :result (gptel-fleet--truncate
                                        (plist-get call :result)
                                        gptel-fleet-log-max-result)))
  nil)

(defun gptel-fleet--post-response-hook (beg end)
  "Log the main agent response between BEG and END."
  (when (and (bound-and-true-p gptel-mode) (< beg end) (not gptel-fleet--context))
    (gptel-fleet-log-event (gptel-fleet-current-context) "response"
                           (list :text (gptel-fleet--truncate
                                        (gptel-fleet--response-text beg end)
                                        gptel-fleet-log-max-result)))))

(defun gptel-fleet--response-text (beg end)
  "Text between BEG and END without tool-call blocks (those are logged as events)."
  (let ((pos beg) (out nil))
    (while (< pos end)
      (let ((next (next-single-property-change pos 'gptel nil end))
            (prop (get-text-property pos 'gptel)))
        (unless (or (eq (car-safe prop) 'tool) (eq prop 'ignore))
          (push (buffer-substring-no-properties pos next) out))
        (setq pos next)))
    (string-trim (apply #'concat (nreverse out)))))

(defun gptel-fleet--send-advice (&rest _)
  "Before `gptel-send' in a chat buffer: log the user's latest turn."
  (when (bound-and-true-p gptel-mode)
    (let* ((end (if (use-region-p) (region-end) (point)))
           (beg (if (use-region-p) (region-beginning)
                  (or (previous-single-property-change end 'gptel nil (point-min))
                      (point-min))))
           (text (string-trim (buffer-substring-no-properties beg end))))
      (unless (string-empty-p text)
        (gptel-fleet-log-event (gptel-fleet-current-context) "prompt"
                               (list :text (gptel-fleet--truncate
                                            text gptel-fleet-log-max-result)))))))

;;;; Reading logs

(defun gptel-fleet--read-jsonl (file)
  "Parse FILE into a list of alists (string keys)."
  (when (file-readable-p file)
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8))
        (insert-file-contents file))
      (goto-char (point-min))
      (let (out)
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (unless (string-empty-p (string-trim line))
              (push (condition-case nil
                        (json-parse-string line :object-type 'alist
                                           :null-object nil :false-object nil)
                      (error (list (cons 'ev "corrupt") (cons 'raw line))))
                    out)))
          (forward-line 1))
        (nreverse out)))))

(defun gptel-fleet-log-index ()
  "All run index entries, oldest first."
  (gptel-fleet--read-jsonl (expand-file-name "index.jsonl" (gptel-fleet-log-root))))

(defun gptel-fleet--get (alist key)
  "Value of KEY (a symbol) in ALIST from `json-parse-string'."
  (alist-get key alist))

;;;; Reconstruction

(defun gptel-fleet--run-table ()
  "Hash run-id -> index entry, plus children lists under (children . RUN)."
  (let ((tbl (make-hash-table :test #'equal)))
    (dolist (e (gptel-fleet-log-index))
      (let ((run (gptel-fleet--get e 'run))
            (parent (gptel-fleet--get e 'parent)))
        (puthash run e tbl)
        (when parent
          (push run (gethash (cons 'children parent) tbl)))))
    tbl))

(defun gptel-fleet-run-root (run &optional tbl)
  "Topmost ancestor of RUN using TBL."
  (let ((tbl (or tbl (gptel-fleet--run-table))) (cur run) parent)
    (while (and (setq parent (gptel-fleet--get (gethash cur tbl) 'parent))
                (gethash parent tbl))
      (setq cur parent))
    cur))

(defun gptel-fleet--org-block (kind text)
  "Org block of KIND containing TEXT, escaped."
  (format "#+begin_%s\n%s\n#+end_%s\n" kind
          (replace-regexp-in-string "^\\([*#]\\)" ",\\1" (string-trim-right (or text "")))
          kind))

(defun gptel-fleet--render-run (run tbl depth)
  "Insert RUN and (recursively, at their spawn points) its children."
  (let* ((entry (gethash run tbl))
         (file (gptel-fleet--get entry 'file))
         (events (gptel-fleet--read-jsonl file))
         (stars (make-string depth ?*))
         (rendered nil))
    (dolist (ev events)
      (let ((type (gptel-fleet--get ev 'ev))
            (ts (gptel-fleet--get ev 'ts)))
        (pcase type
          ("run-start"
           (insert (format "%s %s :: %s\n" stars (gptel-fleet--get ev 'agent)
                           (or (gptel-fleet--get ev 'description)
                               (gptel-fleet--get ev 'buffer) run)))
           (insert ":PROPERTIES:\n"
                   (format ":RUN: %s\n:STARTED: %s\n" run ts)
                   (format ":BACKEND: %s\n:MODEL: %s\n"
                           (or (gptel-fleet--get ev 'backend) "")
                           (or (gptel-fleet--get ev 'model) ""))
                   (if-let* ((sha (gptel-fleet--get ev 'system_sha)))
                       (format ":SYSTEM: [[file:%s][%s]]\n"
                               (expand-file-name (concat sha ".md")
                                                 (expand-file-name "prompts" (gptel-fleet-log-root)))
                               (substring sha 0 10))
                     "")
                   (format ":LOG: [[file:%s]]\n" file)
                   ":END:\n")
           (when-let* ((p (gptel-fleet--get ev 'prompt)))
             (insert "Task prompt:\n" (gptel-fleet--org-block "quote" p))))
          ("prompt" (insert (format "- [%s] *User*:\n" ts)
                            (gptel-fleet--org-block "quote" (gptel-fleet--get ev 'text))))
          ("response" (insert (format "- [%s] *Response*:\n" ts)
                              (gptel-fleet--org-block "example" (gptel-fleet--get ev 'text))))
          ("decision"
           (insert (format "- [%s] decision ~%s~ → *%s* (%s)\n" ts
                           (gptel-fleet--get ev 'tool) (gptel-fleet--get ev 'level)
                           (gptel-fleet--get ev 'source))))
          ("tool-call"
           (insert (format "- [%s] call ~%s~ %s\n" ts (gptel-fleet--get ev 'tool)
                           (gptel-fleet--one-line (json-encode (gptel-fleet--get ev 'args)) 200))))
          ("tool-result"
           (insert (format "  result of ~%s~:\n" (gptel-fleet--get ev 'tool))
                   (gptel-fleet--org-block "example" (format "%s" (gptel-fleet--get ev 'result)))))
          ("spawn"
           (let ((child (gptel-fleet--get ev 'child)))
             (insert (format "- [%s] spawn → %s: %s\n" ts
                             (gptel-fleet--get ev 'child_agent)
                             (gptel-fleet--get ev 'description)))
             (when (gethash child tbl)
               (push child rendered)
               (gptel-fleet--render-run child tbl (1+ depth)))))
          ("spawn-end" (insert (format "- [%s] subagent %s finished: %s\n" ts
                                       (gptel-fleet--get ev 'child)
                                       (gptel-fleet--get ev 'status))))
          ("run-end"
           (insert (format "- [%s] *Final report* (%s):\n" ts (gptel-fleet--get ev 'status))
                   (gptel-fleet--org-block "example" (gptel-fleet--get ev 'result))))
          ("request" (insert (format "- [%s] request payload (%d chars)\n" ts
                                     (length (json-encode (gptel-fleet--get ev 'data))))))
          (_ (insert (format "- [%s] %s\n" ts type))))))
    ;; Children whose spawn event is missing still show up.
    (dolist (child (reverse (gethash (cons 'children run) tbl)))
      (unless (member child rendered)
        (gptel-fleet--render-run child tbl (1+ depth))))))

;;;###autoload
(defun gptel-fleet-log-reconstruct (run &optional whole-tree)
  "Show RUN with all subagent runs spliced in at their spawn points.
With WHOLE-TREE (prefix arg), start from RUN's topmost ancestor."
  (interactive
   (list (let* ((idx (reverse (gptel-fleet-log-index)))
                (cands (mapcar (lambda (e)
                                 (cons (format "%s  %-14s %s"
                                               (gptel-fleet--get e 'run)
                                               (gptel-fleet--get e 'agent)
                                               (or (gptel-fleet--get e 'description)
                                                   (gptel-fleet--get e 'buffer) ""))
                                       (gptel-fleet--get e 'run)))
                               idx)))
           (cdr (assoc (completing-read "Run: " cands nil t) cands)))
         current-prefix-arg))
  (let* ((tbl (gptel-fleet--run-table))
         (root (if whole-tree (gptel-fleet-run-root run tbl) run))
         (buf (get-buffer-create (format "*gptel-fleet run %s*" root))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (when (fboundp 'org-mode) (org-mode))
        (insert (format "#+title: gptel-fleet run %s\n\n" root))
        (gptel-fleet--render-run root tbl 1)
        (goto-char (point-min))
        (when (fboundp 'org-content) (ignore-errors (org-content 2)))))
    (pop-to-buffer buf)
    buf))

(provide 'gptel-fleet-log)
;;; gptel-fleet-log.el ends here
