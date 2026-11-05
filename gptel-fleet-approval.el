;;; gptel-fleet-approval.el --- Human-in-the-loop approval queue  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Georg Alberding
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Calls at level `confirm' are approved in one of two places, never via
;; popups:
;;
;; 1. Inline, gptel style (`gptel-fleet-approval-ui' = inline, default):
;;    fleet decides the level before gptel runs the call and asks gptel to
;;    show its own confirmation in the chat buffer, also for subagents
;;    (inside their task overlay):
;;      C-c C-c run   C-c C-k cancel   C-c C-r steer   C-c C-i inspect/edit
;;
;; 2. The queue buffer `gptel-fleet-approvals' (open it when you like):
;;    lists gptel's inline pending calls *and* fleet's own requests
;;    (agent-API changes with diffs; calls made outside gptel's flow).
;;      y run      n reject       d reject with reason / steer
;;      s S a A    run and learn (session / saved; exact args / any args)
;;      e          edit (fleet: edit buffer; inline: gptel inspect)
;;      RET        show the call / jump to it in the chat buffer
;;
;; Nothing pops up and nothing times out.  The mode line shows
;; " Fleet:N" while N calls wait; click it to open the queue.

;;; Code:

(require 'gptel-fleet-core)
(require 'gptel-fleet-policy)
(require 'gptel-fleet-log)
(require 'tabulated-list)

(declare-function gptel--accept-tool-calls "gptel")
(declare-function gptel--reject-tool-calls "gptel")
(declare-function gptel--steer-tool-calls "gptel")
(declare-function gptel--inspect-tool-calls "gptel")
(declare-function gptel-fleet-ask-answer-inline "gptel-fleet-ask")
(declare-function gptel-fleet-ask-answer-request "gptel-fleet-ask")

(defcustom gptel-fleet-approval-ui 'inline
  "Where `confirm' calls are approved.
inline: gptel's inline confirmation in the chat buffer (the queue buffer
        lists them too).
queue:  only in the queue buffer (`gptel-fleet-approvals').
Neither pops up windows or prompts on its own."
  :group 'gptel-fleet
  :type '(choice (const inline) (const queue)))

(defcustom gptel-fleet-approvals-auto-close t
  "Close the queue window once the last waiting call has been answered."
  :group 'gptel-fleet
  :type 'boolean)

(defcustom gptel-fleet-approval-notify t
  "Show an echo-area message when a call starts waiting for approval."
  :group 'gptel-fleet
  :type 'boolean)

(cl-defstruct (gptel-fleet-request (:constructor gptel-fleet-request-create))
  id time ctx tool-name args arg-values source run-fn deny-fn deferred
  detail no-learn edited)

(defvar gptel-fleet--queue nil "Pending `gptel-fleet-request' objects, oldest first.")

;;;; Decision tokens (hook -> guard)

(defvar gptel-fleet--tokens (make-hash-table :test #'equal)
  "(TOOL-NAME . ARG-VALUES) -> decision plist recorded before gptel ran it.")

(defun gptel-fleet--values-from-args (tool args)
  "Positional values of TOOL for the ARGS plist gptel parsed from the model."
  (mapcar (lambda (spec) (plist-get args (intern (concat ":" (plist-get spec :name)))))
          (gptel-tool-args tool)))

(defun gptel-fleet--put-token (name values plist)
  "Record decision PLIST for the call NAME with VALUES (replacing older ones)."
  (puthash (cons name values) plist gptel-fleet--tokens))

(defun gptel-fleet--take-token (name values)
  "Remove and return the token for NAME with VALUES, or nil."
  (let* ((key (cons name values))
         (tok (gethash key gptel-fleet--tokens)))
    (when tok (remhash key gptel-fleet--tokens))
    tok))

;;;; Indicator

(defvar gptel-fleet--pending-count 0 "Calls waiting for approval (inline + queue).")

(defun gptel-fleet--inline-pending ()
  "List of (BUFFER OVERLAY CALLS) for gptel's inline pending tool calls."
  (cl-loop for buf in (buffer-list)
           when (buffer-local-value 'gptel-mode buf)
           append (with-current-buffer buf
                    (cl-loop for ov in (overlays-in (point-min) (point-max))
                             for calls = (overlay-get ov 'gptel-tool)
                             when (and calls (consp calls))
                             collect (list buf ov calls)))))

(defun gptel-fleet-update-indicator ()
  "Recount pending approvals and refresh the queue buffer and mode line."
  (setq gptel-fleet--pending-count
        (+ (length gptel-fleet--queue)
           (cl-loop for (_b _o calls) in (gptel-fleet--inline-pending) sum (length calls))))
  (gptel-fleet--refresh-queue-buffer)
  (force-mode-line-update t))

(defvar gptel-fleet-mode-line-map
  (let ((m (make-sparse-keymap)))
    (define-key m [mode-line mouse-1] #'gptel-fleet-approvals)
    m))

(defconst gptel-fleet-mode-line
  '(:eval (when (> gptel-fleet--pending-count 0)
            (propertize (format " Fleet:%d" gptel-fleet--pending-count)
                        'face 'warning 'mouse-face 'mode-line-highlight
                        'help-echo "Calls waiting for approval: mouse-1 opens the queue"
                        'local-map gptel-fleet-mode-line-map)))
  "Mode-line construct added to `global-mode-string' by `gptel-fleet-mode'.")
(put 'gptel-fleet-mode-line 'risky-local-variable t)

;;;; Fleet's own queue

(defun gptel-fleet-enqueue (req)
  "Add REQ to the queue.  No window, no prompt: see the queue or mode line."
  (setq gptel-fleet--queue (append gptel-fleet--queue (list req)))
  (gptel-fleet-update-indicator)
  (when gptel-fleet-approval-notify
    (message "gptel-fleet: %s wants %s — waiting in M-x gptel-fleet-approvals (%d pending)"
             (plist-get (gptel-fleet-request-ctx req) :agent)
             (gptel-fleet-request-tool-name req) gptel-fleet--pending-count)))

(defun gptel-fleet--learn-rule (req kind)
  "Build a learned rule for REQ.  KIND is exact or any."
  (let ((tool (gptel-fleet-request-tool-name req)))
    (if (eq kind 'any)
        `(allow :tool ,tool)
      `(allow :tool ,tool
              :args ,(cl-loop for (k v) on (gptel-fleet-request-args req) by #'cddr
                              collect (list k :equal v))))))

(defun gptel-fleet--resolve (req answer &optional reason)
  "Resolve REQ with ANSWER (run or deny), deny REASON optional."
  (when (memq req gptel-fleet--queue)
    (setq gptel-fleet--queue (delq req gptel-fleet--queue))
    (gptel-fleet--close-call-view req)
    (gptel-fleet-log-event (gptel-fleet-request-ctx req) "approval"
                           (list :tool (gptel-fleet-request-tool-name req)
                                 :answer answer :reason reason))
    (gptel-fleet-update-indicator)
    (if (eq answer 'run)
        (funcall (gptel-fleet-request-run-fn req) (gptel-fleet-request-arg-values req)
                 (when (gptel-fleet-request-edited req)
                   ;; The chat and the model's history still show the original
                   ;; call; say what actually ran.
                   (format "[The user edited this call before it ran.  Arguments \
actually used: %s]\n" (prin1-to-string (gptel-fleet-request-args req)))))
      (funcall (gptel-fleet-request-deny-fn req) reason))))

(defun gptel-fleet-answer (req key)
  "Apply answer KEY (a character) to REQ."
  (if (equal (gptel-fleet-request-tool-name req) "AskUser")
      (pcase key
        ((or ?y ?e ?s ?S ?a ?A) (gptel-fleet-ask-answer-request req))
        ((or ?n ?d) (gptel-fleet-ask-answer-request req t)))
    (gptel-fleet--answer-change-or-call req key)))

(defun gptel-fleet--answer-change-or-call (req key)
  "Apply answer KEY to REQ, a tool call or fleet change."
  (let ((agent (plist-get (gptel-fleet-request-ctx req) :agent)))
    (when (and (gptel-fleet-request-no-learn req) (memq key '(?s ?S ?a ?A ?e)))
      (message "gptel-fleet: this request cannot be learned or edited; treated as `y'")
      (setq key ?y))
    (pcase key
      (?y (gptel-fleet--resolve req 'run))
      (?s (gptel-fleet-learn agent (gptel-fleet--learn-rule req 'exact))
          (gptel-fleet--resolve req 'run))
      (?S (gptel-fleet-learn agent (gptel-fleet--learn-rule req 'any))
          (gptel-fleet--resolve req 'run))
      (?a (gptel-fleet-learn agent (gptel-fleet--learn-rule req 'exact) t)
          (gptel-fleet--resolve req 'run))
      (?A (gptel-fleet-learn agent (gptel-fleet--learn-rule req 'any) t)
          (gptel-fleet--resolve req 'run))
      (?n (gptel-fleet--resolve req 'deny nil))
      (?d (gptel-fleet--resolve req 'deny (read-string "Reason (sent to the model): ")))
      (?e (gptel-fleet--edit-args req))
      (_ nil))))

;;;; Editing a pending call in a proper buffer

(defcustom gptel-fleet-edit-modes
  '((("Bash" . "command") . gptel-fleet--shell-mode)
    (("Eval" . "expression") . emacs-lisp-mode)
    (("FleetDefineTool" . "spec") . lisp-data-mode)
    (("FleetSet" . "value") . lisp-data-mode)
    (("FleetAssign" . "names") . lisp-data-mode)
    (("FleetAddRule" . "rule") . lisp-data-mode)
    (("FleetExplain" . "args") . lisp-data-mode)
    (("FleetNewAgent" . "system_prompt") . markdown-mode))
  "Major modes for editing tool arguments, keyed by (TOOL . ARG).
Arguments that hold file content (Edit new_str, Write content, Insert
new_str) get the mode of the target file; Edit in diff mode gets
`diff-mode'.  Everything else falls back to `text-mode'."
  :group 'gptel-fleet
  :type '(alist :key-type (cons string string) :value-type function))

(defcustom gptel-fleet-edit-run-mode-hooks nil
  "If nil, edit buffers enable their major mode without running its hooks.
The buffers are not visiting files, so hooks like `eglot-ensure' or
LSP startup would misfire there.  Font lock is enabled either way."
  :group 'gptel-fleet
  :type 'boolean)

(defconst gptel-fleet--primary-args
  '(("Bash" . "command") ("Eval" . "expression") ("Edit" . "new_str")
    ("Write" . "content") ("Insert" . "new_str"))
  "Argument opened first when editing a call of these tools.")

(defvar-local gptel-fleet--edit-req nil "Request edited in this buffer.")
(defvar-local gptel-fleet--edit-arg nil "Name of the argument edited here.")
(defvar-local gptel-fleet--edit-values nil "Working copy of all argument values.")

(defun gptel-fleet--shell-mode ()
  "`bash-ts-mode' when its grammar is installed, else `sh-mode' for bash."
  (if (and (fboundp 'bash-ts-mode) (fboundp 'treesit-ready-p)
           (treesit-ready-p 'bash t))
      (bash-ts-mode)
    (sh-mode)
    (when (fboundp 'sh-set-shell) (let ((inhibit-message t)) (sh-set-shell "bash")))))

(defun gptel-fleet--arg-names (req)
  "Argument names of REQ's tool, in order."
  (let ((tool (gptel-fleet-find-tool (gptel-fleet-request-tool-name req))))
    (mapcar (lambda (a) (plist-get a :name)) (and tool (gptel-tool-args tool)))))

(defun gptel-fleet--value-of (names values name)
  "Value of argument NAME given NAMES and VALUES."
  (nth (cl-position name names :test #'equal) values))

(defun gptel-fleet--edit-mode-for (req arg names values)
  "Major mode function for editing ARG of REQ."
  (let* ((tool (gptel-fleet-request-tool-name req))
         (path (cond ((member tool '("Edit" "Insert"))
                      (gptel-fleet--value-of names values "path"))
                     ((equal tool "Write")
                      (let ((dir (gptel-fleet--value-of names values "path"))
                            (fn (gptel-fleet--value-of names values "filename")))
                        (and (stringp dir) (stringp fn) (expand-file-name fn dir)))))))
    (or (cdr (assoc (cons tool arg) gptel-fleet-edit-modes))
        (and (equal tool "Edit") (equal arg "new_str")
             (gptel-fleet--set-p (gptel-fleet--value-of names values "diff"))
             #'diff-mode)
        (and (stringp path)
             (member arg '("new_str" "content" "old_str"))
             (let ((m (assoc-default path auto-mode-alist #'string-match)))
               (and (functionp m) m)))
        #'text-mode)))

(defvar-keymap gptel-fleet-edit-call-mode-map
  "C-c C-c" #'gptel-fleet-edit-submit
  "C-c '"   #'gptel-fleet-edit-submit
  "C-c C-'" #'gptel-fleet-edit-submit
  "C-c C-k" #'gptel-fleet-edit-abort
  "C-c C-a" #'gptel-fleet-edit-other-arg)

(define-minor-mode gptel-fleet-edit-call-mode
  "Editing one argument of a pending tool call.
\\{gptel-fleet-edit-call-mode-map}"
  :lighter " Fleet-Edit")

(defun gptel-fleet--edit-header ()
  "Header line for an edit buffer."
  (let ((ctx (gptel-fleet-request-ctx gptel-fleet--edit-req)))
    (format " %s → %s · %s   C-c C-c / C-c ' submit  C-c C-a other arg  C-c C-k back to queue"
            (plist-get ctx :agent) (gptel-fleet-request-tool-name gptel-fleet--edit-req)
            gptel-fleet--edit-arg)))

(defun gptel-fleet--edit-args (req &optional arg values)
  "Edit argument ARG of REQ in a buffer with a fitting major mode.
VALUES is the working copy of all argument values (default: REQ's)."
  (let* ((names (gptel-fleet--arg-names req))
         (values (or values (copy-sequence (gptel-fleet-request-arg-values req))))
         (tool (gptel-fleet-request-tool-name req))
         (arg (or arg
                  (let ((p (cdr (assoc tool gptel-fleet--primary-args))))
                    (and (member p names) p))
                  (cl-loop for n in names for v in values when (stringp v) return n)
                  (car names)))
         (val (gptel-fleet--value-of names values arg))
         (mode (gptel-fleet--edit-mode-for req arg names values))
         (buf (get-buffer-create (format "*gptel-fleet edit: %s %s*" tool arg))))
    (unless names (user-error "Tool %s has no arguments to edit" tool))
    (setf (gptel-fleet-request-deferred req) t) ; no minibuffer re-prompt meanwhile
    (with-current-buffer buf
      (let ((inhibit-read-only t)) (erase-buffer))
      (if gptel-fleet-edit-run-mode-hooks
          (funcall mode)
        (delay-mode-hooks (funcall mode)))
      (font-lock-mode 1)
      (insert (cond ((stringp val) val)
                    ((null val) "")
                    (t (prin1-to-string val))))
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (setq gptel-fleet--edit-req req
            gptel-fleet--edit-arg arg
            gptel-fleet--edit-values values)
      (gptel-fleet-edit-call-mode 1)
      (setq header-line-format '(:eval (gptel-fleet--edit-header))))
    (pop-to-buffer buf)
    buf))

(defun gptel-fleet--edit-store ()
  "Write the buffer text back into the working copy; return the values."
  (let* ((req gptel-fleet--edit-req)
         (names (gptel-fleet--arg-names req))
         (i (cl-position gptel-fleet--edit-arg names :test #'equal))
         (spec (nth i (gptel-tool-args
                       (gptel-fleet-find-tool (gptel-fleet-request-tool-name req)))))
         (orig (nth i (gptel-fleet-request-arg-values req)))
         (text (buffer-substring-no-properties (point-min) (point-max)))
         (stringy (or (stringp orig)
                      (member (format "%s" (plist-get spec :type)) '("string"))))
         (new (cond
               (stringy (if (and (null orig) (string-empty-p text)) nil text))
               ((string-blank-p text) nil)
               (t (condition-case err (car (read-from-string text))
                    (error (user-error "Not valid Lisp data: %s"
                                       (error-message-string err))))))))
    (when (derived-mode-p 'lisp-data-mode)
      (save-excursion (check-parens)))
    (setf (nth i gptel-fleet--edit-values) new)
    gptel-fleet--edit-values))

(defun gptel-fleet-edit-submit ()
  "Run the call with the edited arguments."
  (interactive)
  (let ((req gptel-fleet--edit-req)
        (values (gptel-fleet--edit-store))
        (buf (current-buffer)))
    (unless (memq req gptel-fleet--queue)
      (user-error "This call is no longer pending (already answered)"))
    (setf (gptel-fleet-request-arg-values req) values
          (gptel-fleet-request-edited req) t)
    (setf (gptel-fleet-request-args req)
          (gptel-fleet--arg-plist (gptel-fleet-find-tool (gptel-fleet-request-tool-name req))
                                  values))
    (gptel-fleet-log-event (gptel-fleet-request-ctx req) "approval-edit"
                           (list :tool (gptel-fleet-request-tool-name req)
                                 :args (gptel-fleet-request-args req)))
    (quit-window t (get-buffer-window buf))
    (when (buffer-live-p buf) (kill-buffer buf))
    (gptel-fleet--resolve req 'run)))

(defun gptel-fleet-edit-abort ()
  "Discard the edit; the call stays pending in the queue."
  (interactive)
  (let ((req gptel-fleet--edit-req) (buf (current-buffer)))
    (when (memq req gptel-fleet--queue)
      (setf (gptel-fleet-request-deferred req) nil))
    (quit-window t (get-buffer-window buf))
    (when (buffer-live-p buf) (kill-buffer buf))
    (gptel-fleet-approvals)))

(defun gptel-fleet-edit-other-arg ()
  "Keep this edit and switch to another argument of the same call."
  (interactive)
  (let* ((req gptel-fleet--edit-req)
         (values (gptel-fleet--edit-store))
         (names (gptel-fleet--arg-names req))
         (other (completing-read "Edit argument: " names nil t))
         (old (current-buffer)))
    (gptel-fleet--edit-args req other values)
    (unless (eq old (current-buffer)) (kill-buffer old))))

(defun gptel-fleet--describe-request (req)
  "Multi-line description of REQ."
  (let ((ctx (gptel-fleet-request-ctx req)))
    (if (equal (gptel-fleet-request-tool-name req) "fleet-change")
        (concat (format "Fleet change %s: %s\n"
                        (gptel-fleet-request-id req)
                        (plist-get (gptel-fleet-request-args req) :summary))
                (format "Keys in the queue: y apply   n reject   d reject with reason\n\n")
                (or (gptel-fleet-request-detail req) ""))
    (concat (format "Agent:  %s  (run %s)\nTool:   %s   [%s]\nSource: %s\n"
                    (plist-get ctx :agent) (plist-get ctx :run)
                    (gptel-fleet-request-tool-name req)
                    (or (gptel-fleet-tool-package (gptel-fleet-request-tool-name req)) "-")
                    (gptel-fleet-describe-source (gptel-fleet-request-source req)))
            (cl-loop for (k v) on (gptel-fleet-request-args req) by #'cddr
                     concat (format "\n%s:\n%s\n" k (if (stringp v) v (pp-to-string v))))
            (if-let* ((d (gptel-fleet-request-detail req))) (concat "\n" d) "")))))

(defvar-local gptel-fleet--call-req nil "Request shown in the *gptel-fleet call* buffer.")

(defun gptel-fleet--show-call (req)
  "Show REQ in the call buffer without selecting it."
  (with-current-buffer (get-buffer-create "*gptel-fleet call*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (gptel-fleet--describe-request req))
      (goto-char (point-min))
      (special-mode))
    (setq gptel-fleet--call-req req)
    (display-buffer (current-buffer))))

(defun gptel-fleet--close-call-view (req)
  "Close the call buffer if it shows REQ (which has just been answered)."
  (when-let* ((buf (get-buffer "*gptel-fleet call*")))
    (when (eq (buffer-local-value 'gptel-fleet--call-req buf) req)
      (dolist (win (get-buffer-window-list buf nil t))
        (quit-window nil win)))))

(defun gptel-fleet-approve-next ()
  "Answer the oldest fleet request in the minibuffer (only when you ask)."
  (interactive)
  (let ((req (car gptel-fleet--queue)))
    (unless req (user-error "No fleet requests waiting"))
    (with-current-buffer (get-buffer-create "*gptel-fleet call*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (gptel-fleet--describe-request req))
        (goto-char (point-min))
        (special-mode))
      (display-buffer (current-buffer)))
    (let ((choice (car (read-multiple-choice
                        (format "%s → %s" (plist-get (gptel-fleet-request-ctx req) :agent)
                                (gptel-fleet-request-tool-name req))
                        '((?y "yes" "Run once")
                          (?s "session" "Allow these exact args this session")
                          (?S "Session-any" "Allow this tool this session")
                          (?a "always" "Always allow these exact args")
                          (?A "Always-any" "Always allow this tool for this agent")
                          (?n "no" "Deny")
                          (?d "deny+reason" "Deny with a message to the model")
                          (?e "edit" "Edit the call in a buffer")
                          (?q "later" "Leave it in the queue"))))))
      (when-let* ((win (get-buffer-window "*gptel-fleet call*"))) (quit-window nil win))
      (unless (eq choice ?q) (gptel-fleet-answer req choice)))))

;;;; Queue buffer: fleet requests + gptel's inline pending calls

(defun gptel-fleet--queue-buffer ()
  "Return the queue buffer, refreshed."
  (let ((buf (get-buffer-create "*gptel-fleet approvals*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'gptel-fleet-approvals-mode)
        (gptel-fleet-approvals-mode))
      (gptel-fleet--queue-entries)
      (tabulated-list-print t))
    buf))

(defun gptel-fleet--refresh-queue-buffer ()
  "Refresh the queue buffer if it exists; close its window when empty."
  (when-let* ((buf (get-buffer "*gptel-fleet approvals*")))
    (with-current-buffer buf
      (gptel-fleet--queue-entries)
      (tabulated-list-print t)
      (when (and gptel-fleet-approvals-auto-close (null tabulated-list-entries))
        (dolist (win (get-buffer-window-list buf nil t))
          (when (window-live-p win) (quit-window nil win)))))))

(defun gptel-fleet--inline-token (call)
  "Token recorded for an inline CALL (TOOL ARGS CB), without consuming it."
  (gethash (cons (gptel-tool-name (car call))
                 (gptel-fleet--values-from-args (car call) (cadr call)))
           gptel-fleet--tokens))

(defun gptel-fleet--queue-entries ()
  "Fill `tabulated-list-entries'."
  (setq tabulated-list-entries
        (append
         (cl-loop
          for (buf ov calls) in (gptel-fleet--inline-pending)
          for tok = (gptel-fleet--inline-token (car calls))
          collect (list (list 'inline buf ov calls)
                        (vector (if tok (format-time-string "%H:%M:%S" (plist-get tok :time)) "")
                                (or (plist-get (plist-get tok :ctx) :agent) "?")
                                (mapconcat (lambda (c) (gptel-tool-name (car c))) calls ",")
                                (format "inline %s" (buffer-name buf))
                                (gptel-fleet--one-line
                                 (mapconcat (lambda (c)
                                              (let ((m (plist-get (cadr c) :model)))
                                                (concat (if (gptel-fleet--set-p m)
                                                            (format "[model %s] " m) "")
                                                        (prin1-to-string (cadr c)))))
                                            calls " ")
                                 120))))
         (mapcar (lambda (req)
                   (list req
                         (vector (format-time-string "%H:%M:%S" (gptel-fleet-request-time req))
                                 (plist-get (gptel-fleet-request-ctx req) :agent)
                                 (gptel-fleet-request-tool-name req)
                                 "queue"
                                 (gptel-fleet--one-line
                                  (prin1-to-string (gptel-fleet-request-args req)) 120))))
                 gptel-fleet--queue))))

(defmacro gptel-fleet--define-queue-command (name key doc)
  "Define approval-queue command NAME answering with KEY, documented by DOC."
  `(defun ,name ()
     ,(concat doc "\nActs on the entry at point in the approval queue.")
     (interactive)
     (gptel-fleet--queue-answer ,key)))

(gptel-fleet--define-queue-command gptel-fleet-approvals-run-once ?y
  "Run the call once.  For a question (AskUser): answer it.")
(gptel-fleet--define-queue-command gptel-fleet-approvals-allow-exact-session ?s
  "Run it, and allow these exact arguments for this agent until Emacs exits.")
(gptel-fleet--define-queue-command gptel-fleet-approvals-allow-tool-session ?S
  "Run it, and allow this tool with any arguments for this agent until Emacs exits.")
(gptel-fleet--define-queue-command gptel-fleet-approvals-allow-exact-always ?a
  "Run it, and always allow these exact arguments for this agent.
The rule is saved to the learned rules (`gptel-fleet-edit-learned').")
(gptel-fleet--define-queue-command gptel-fleet-approvals-allow-tool-always ?A
  "Run it, and always allow this tool with any arguments for this agent.
The rule is saved to the learned rules (`gptel-fleet-edit-learned').")
(gptel-fleet--define-queue-command gptel-fleet-approvals-deny ?n
  "Deny the call.  For a question (AskUser): decline to answer.")
(gptel-fleet--define-queue-command gptel-fleet-approvals-deny-with-reason ?d
  "Deny the call and send a reason to the model.
Inline calls jump to the chat buffer to steer from there.")
(gptel-fleet--define-queue-command gptel-fleet-approvals-edit ?e
  "Edit the call's arguments in a buffer before deciding.
Inline calls jump to the chat buffer and open gptel's inspector.")

(defun gptel-fleet-approvals-refresh ()
  "Re-read pending calls and redraw the approval queue."
  (interactive)
  (gptel-fleet-update-indicator))

(defvar-keymap gptel-fleet-approvals-mode-map
  :parent tabulated-list-mode-map
  "y" #'gptel-fleet-approvals-run-once
  "s" #'gptel-fleet-approvals-allow-exact-session
  "S" #'gptel-fleet-approvals-allow-tool-session
  "a" #'gptel-fleet-approvals-allow-exact-always
  "A" #'gptel-fleet-approvals-allow-tool-always
  "n" #'gptel-fleet-approvals-deny
  "d" #'gptel-fleet-approvals-deny-with-reason
  "e" #'gptel-fleet-approvals-edit
  "C-c '" #'gptel-fleet-approvals-edit
  "C-c C-'" #'gptel-fleet-approvals-edit
  "RET" #'gptel-fleet-approvals-show
  "g" #'gptel-fleet-approvals-refresh)

(define-derived-mode gptel-fleet-approvals-mode tabulated-list-mode "Fleet-Approvals"
  "Calls waiting for your decision, from every agent in one list.

Each row is one pending call.  The Where column says which kind:
  inline BUFFER  a call shown in that gptel chat buffer (main agent
                 or a subagent running inside it)
  queue          a fleet request (subagent escalations, fleet changes)
Rows for AskUser are questions: answering runs them, denying declines.

Deciding (on the row at point):
  \\[gptel-fleet-approvals-run-once]  run once                \
\\[gptel-fleet-approvals-deny]  deny
  \\[gptel-fleet-approvals-deny-with-reason]  deny, with a reason sent to the model
  \\[gptel-fleet-approvals-edit]  edit the arguments first
Running and learning a rule for this agent:
  \\[gptel-fleet-approvals-allow-exact-session]  these exact args, this session   \
\\[gptel-fleet-approvals-allow-exact-always]  these exact args, always
  \\[gptel-fleet-approvals-allow-tool-session]  this tool, this session          \
\\[gptel-fleet-approvals-allow-tool-always]  this tool, always
\"Always\" rules are saved; review them with `gptel-fleet-edit-learned'.
Floor rules (`gptel-fleet-floor-rules') still apply over learned rules.

Other:
  \\[gptel-fleet-approvals-show]  show the full call (inline: jump to the chat)
  \\[gptel-fleet-approvals-refresh]  refresh
  \\[quit-window]  quit

All bindings:
\\{gptel-fleet-approvals-mode-map}"
  (setq tabulated-list-format [("Time" 9 t) ("Agent" 16 t) ("Tool" 16 t)
                               ("Where" 24 t) ("Args" 0 nil)])
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

(defun gptel-fleet--inline-call-position (ov)
  "Position of the pending calls' prompt inside gptel's tool overlay OV.
OV itself starts at the beginning of the whole response (or of the
buffer), often far above the calls.  The \"... wants to run:\" header
is drawn before gptel's prompt overlays; a question has its own overlay."
  (with-current-buffer (overlay-buffer ov)
    (or (when-let* ((ask (cl-find-if (lambda (o) (overlay-get o 'gptel-fleet-ask))
                                     (overlays-in (overlay-start ov) (overlay-end ov)))))
          (overlay-start ask))
        (cl-loop for p in (overlay-get ov 'prompt)
                 when (overlay-buffer p) minimize (overlay-start p))
        (overlay-end ov))))

(defun gptel-fleet--inline-jump (buf ov &optional command)
  "Show the inline call OV in BUF at its prompt; run COMMAND there interactively."
  (let ((pos (gptel-fleet--inline-call-position ov)))
    (pop-to-buffer buf)
    (goto-char pos)
    (when (eq (window-buffer) buf)
      (recenter)))
  (when command (call-interactively command)))

(defun gptel-fleet--answer-inline (entry key)
  "Answer inline ENTRY (inline BUF OV CALLS) with KEY."
  (pcase-let ((`(inline ,buf ,ov ,calls) entry))
    (unless (overlay-buffer ov) (user-error "Already answered"))
    (if (cl-some (lambda (c) (equal (gptel-tool-name (car c)) "AskUser")) calls)
        ;; A question: answer it (minibuffer, since you asked for it here).
        (pcase key
          ((or ?y ?e ?s ?S ?a ?A) (gptel-fleet-ask-answer-inline buf ov calls))
          (?n (gptel-fleet-ask-answer-inline buf ov calls t))
          (?d (gptel-fleet--inline-jump buf ov #'gptel--steer-tool-calls)))
      (gptel-fleet--answer-inline-call buf ov calls key))
    (gptel-fleet-update-indicator)))

(defun gptel-fleet--answer-inline-call (buf ov calls key)
  "Answer gptel's pending CALLS at OV in BUF with KEY."
  (when (memq key '(?s ?S ?a ?A))
    (dolist (c calls)
      (let* ((tok (gptel-fleet--inline-token c))
             (agent (plist-get (plist-get tok :ctx) :agent))
             (tool (gptel-tool-name (car c))))
        (when agent
          (gptel-fleet-learn
           agent
           (if (memq key '(?S ?A)) `(allow :tool ,tool)
             `(allow :tool ,tool
                     :args ,(cl-loop for (k v) on (cadr c) by #'cddr
                                     collect (list k :equal v))))
           (memq key '(?a ?A)))))))
  (pcase key
    ((or ?y ?s ?S ?a ?A)
     (with-current-buffer buf (gptel--accept-tool-calls calls ov)))
    (?n (with-current-buffer buf (gptel--reject-tool-calls calls ov)))
    (?d (gptel-fleet--inline-jump buf ov #'gptel--steer-tool-calls))
    (?e (gptel-fleet--inline-jump buf ov #'gptel--inspect-tool-calls))))

(defun gptel-fleet--queue-answer (key)
  "Answer the entry at point with KEY."
  (let ((id (tabulated-list-get-id)))
    (cond ((null id) (user-error "Nothing at point"))
          ((eq (car-safe id) 'inline) (gptel-fleet--answer-inline id key))
          (t (gptel-fleet-answer id key)))))

(defun gptel-fleet-approvals-show ()
  "Show the call at point (inline calls: jump to the chat buffer)."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (cond
     ((null id))
     ((eq (car-safe id) 'inline) (gptel-fleet--inline-jump (nth 1 id) (nth 2 id)))
     (t (gptel-fleet--show-call id)))))

;;;###autoload
(defun gptel-fleet-approvals ()
  "Open the approval queue."
  (interactive)
  (gptel-fleet-update-indicator)
  (pop-to-buffer (gptel-fleet--queue-buffer)))

(provide 'gptel-fleet-approval)
;;; gptel-fleet-approval.el ends here
