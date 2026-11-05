;;; gptel-fleet-models.el --- Per-delegation model override for subagents  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Georg Alberding
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The Agent tool gets an optional `model' argument, "Backend:model"
;; ("Backend/model" is accepted too), that applies to that one delegation
;; only:
;;
;;   Agent(researcher, "quick lookup", "...", model="llama-local:qwen3-8b")
;;
;; - Absent: the delegation runs exactly as before, on the agent's
;;   configured model.
;; - Present: the backend must exist and the model must be listed for
;;   it, otherwise the delegation fails at once with a clear error.  The
;;   agent's override, profile and definition are not touched and no
;;   fleet change request is created.
;; - Policy: the delegated agent's override key `:models' lists the
;;   "Backend:model" entries (globs allowed, e.g. "llama-local:*") that
;;   may be chosen per task without asking.  The agent's configured model
;;   is always allowed.  Anything else is clamped to
;;   `gptel-fleet-model-override-level' (default `confirm'): the Agent
;;   call then waits for you, with the model shown in the confirmation.
;;   Moving a subagent from a local to a cloud backend is a privacy
;;   widening as well as a cost one, which is why the default is to ask.
;; - The effective model is shown in the subagent's task header (marked
;;   "per-task"), in the confirmation preview, in the queue, and logged
;;   in the run-start event (`model_override').

;;; Code:

(require 'gptel-fleet-core)
(require 'gptel-fleet-policy)

(defvar gptel-agent--agents)
(defvar gptel-fleet--model-override)

(defcustom gptel-fleet-model-override-level 'confirm
  "Strictest level an Agent call gets when its `model' is not allowed.
A per-task model that is neither the delegated agent's configured
model nor listed in its `:models' override makes the call at least
this strict.  `confirm' asks you; `deny' refuses such delegations."
  :group 'gptel-fleet
  :type '(choice (const confirm) (const escalate) (const deny)))

(defconst gptel-fleet--model-arg
  '(:name "model"
    :type "string"
    :optional t
    :description "Optional.  Run THIS task on a different model, written as \
\"Backend:model\".  Omit it to use the agent's configured model.  Prefer a \
small/local model for quick lookups and a large one for deep work.  Choices \
that run without asking are listed per agent in <fleet_subagents>; others \
need the user's approval.")
  "Argument spec added to the Agent tool.")

;;;; Parsing and validation

(defun gptel-fleet--backend-name (b)
  "Name of backend B (a struct or a name), or nil."
  (cond ((stringp b) b)
        ((and b (gptel-backend-p b)) (gptel-backend-name b))))

(defun gptel-fleet--model-names (backend)
  "Model names (strings) listed for BACKEND (a struct)."
  (mapcar (lambda (m) (format "%s" (if (consp m) (car m) m)))
          (gptel-backend-models backend)))

(defun gptel-fleet--agent-configured-model (agent)
  "Cons (BACKEND-NAME . MODEL-NAME) AGENT runs on when not overridden.
Agents without their own backend or model inherit the current one."
  (let ((p (cdr (assoc agent (bound-and-true-p gptel-agent--agents)))))
    (cons (or (gptel-fleet--backend-name (plist-get p :backend))
              (gptel-fleet--backend-name (bound-and-true-p gptel-backend)))
          (let ((m (or (plist-get p :model) (bound-and-true-p gptel-model))))
            (and m (format "%s" m))))))

(defun gptel-fleet-parse-model-spec (spec &optional default-backend)
  "Parse SPEC, \"Backend:model\" or \"Backend/model\".
Without a known backend prefix, SPEC is a model of DEFAULT-BACKEND (a
name).  Return (ok BACKEND-NAME MODEL-SYMBOL) or (error MESSAGE)."
  (let* ((spec (string-trim (format "%s" spec)))
         (names (mapcar #'car gptel--known-backends))
         (prefix (car (sort (cl-remove-if-not
                             (lambda (n)
                               (and (> (length spec) (1+ (length n)))
                                    (string-prefix-p n spec)
                                    (memq (aref spec (length n)) '(?: ?/))))
                             names)
                            (lambda (a b) (> (length a) (length b))))))
         (bname (or prefix default-backend))
         (model (if prefix (substring spec (1+ (length prefix))) spec))
         (backend (and bname (ignore-errors (gptel-get-backend bname)))))
    (cond
     ((string-empty-p spec) (list 'error "empty model"))
     ((not backend)
      (list 'error
            (format "no known backend in %S (format \"Backend:model\"; known backends: %s)"
                    spec (string-join names ", "))))
     ((string-empty-p model)
      (list 'error (format "no model named after backend %s" bname)))
     ((not (member model (gptel-fleet--model-names backend)))
      (list 'error
            (format "model %s is not listed for backend %s (listed: %s)"
                    model bname
                    (let ((ms (gptel-fleet--model-names backend)))
                      (if ms (string-join ms ", ") "none")))))
     (t (list 'ok bname (intern model))))))

(defun gptel-fleet--model-key (backend-name model)
  "Canonical \"Backend:model\" string."
  (format "%s:%s" backend-name model))

(defun gptel-fleet-model-allowed-p (agent backend-name model)
  "Non-nil if AGENT may run on BACKEND-NAME / MODEL per task without asking.
Allowed: AGENT's configured model, and entries of its `:models'
override (\"Backend:model\", globs like \"Backend:*\" allowed)."
  (let ((key (gptel-fleet--model-key backend-name model))
        (conf (gptel-fleet--agent-configured-model agent)))
    (or (and (equal (car conf) backend-name) (equal (cdr conf) (format "%s" model)))
        (cl-some (lambda (pat)
                   (and (stringp pat)
                        (string-match-p (concat "\\`" (wildcard-to-regexp pat) "\\'") key)))
                 (gptel-fleet-config-get agent :models)))))

(defun gptel-fleet-validate-models-list (v)
  "Problems with a `:models' override value V (list of strings)."
  (if (not (and (listp v) (cl-every #'stringp v)))
      (list ":models must be a list of \"Backend:model\" strings")
    (cl-loop for s in v
             for wild = (string-match-p "[*?[]" s)
             for parsed = (unless wild (gptel-fleet-parse-model-spec s))
             if (and wild (not (string-match-p "\\`[^*?[]+[:/]" s)))
             collect (format ":models entry %S: globs must start with a backend name" s)
             else if (eq (car parsed) 'error)
             collect (format ":models entry %S: %s" s (cadr parsed)))))

;;;; Policy: clamp Agent calls whose model is not allowed

(defun gptel-fleet--model-decide (agent tool-name args decision)
  "Clamp DECISION for an Agent call by AGENT whose ARGS carry a model.
Hooked into `gptel-fleet-decide-post-functions'."
  (let ((spec (plist-get args :model))
        (target (plist-get args :subagent_type)))
    (if (not (and (equal tool-name "Agent") (gptel-fleet--set-p spec) (stringp target)))
        decision
      (pcase (gptel-fleet-parse-model-spec
              spec (car (gptel-fleet--agent-configured-model target)))
        ;; Invalid: keep the level; the delegation itself fails with the error.
        (`(error ,_) decision)
        (`(ok ,bname ,model)
         (if (or (gptel-fleet-model-allowed-p target bname model)
                 (>= (gptel-fleet--level-rank-of gptel-fleet-model-override-level)
                     (gptel-fleet--level-rank-of (car decision))))
             decision
           (ignore agent)
           (cons gptel-fleet-model-override-level
                 (list 'model-override target (gptel-fleet--model-key bname model)
                       (gptel-fleet-config-get target :models)))))))))

;;;; Applying an override to one delegation

(defun gptel-fleet-resolve-model-override (agent spec)
  "Resolve SPEC for a delegation to AGENT: (ok BACKEND MODEL) or (error MSG)."
  (gptel-fleet-parse-model-spec spec (car (gptel-fleet--agent-configured-model agent))))

(defun gptel-fleet--model-task-advice (orig main-cb agent-type description prompt
                                            &optional model &rest _)
  "Around `gptel-agent--task' (ORIG): apply a per-delegation MODEL.
MAIN-CB, AGENT-TYPE, DESCRIPTION and PROMPT are passed on unchanged."
  (if (not (gptel-fleet--set-p model))
      (funcall orig main-cb agent-type description prompt)
    (pcase (gptel-fleet-resolve-model-override agent-type model)
      (`(error ,msg)
       (funcall main-cb
                (format "Error: delegation to %s was not started: %s.  \
Retry without `model' to use the agent's configured model, or pick a \
listed one." agent-type msg)))
      (`(ok ,bname ,msym)
       (let* ((gptel-fleet--model-override
               (list :backend bname :model msym :requested model))
              (gptel-agent--agents
               (mapcar (lambda (e)
                         (if (equal (car e) agent-type)
                             (cons (car e)
                                   (plist-put (plist-put (copy-sequence (cdr e))
                                                         :backend bname)
                                              :model msym))
                           e))
                       gptel-agent--agents)))
         (funcall orig main-cb agent-type description prompt))))))

(defun gptel-fleet--task-overlay-advice (orig &rest args)
  "Around `gptel-agent--task-overlay' (ORIG, ARGS): mark per-task models."
  (if-let* ((ov gptel-fleet--model-override))
      (let ((gptel-model (intern (format "%s · per-task" (plist-get ov :model)))))
        (apply orig args))
    (apply orig args)))

(defun gptel-fleet--task-preview-advice (orig arg-values info)
  "Around the Agent preview (ORIG): show a per-task model in the confirmation.
ARG-VALUES is (TYPE DESCRIPTION PROMPT [MODEL]), INFO the call info."
  (pcase-let ((`(,type ,desc ,prompt ,model) arg-values))
    (if (not (gptel-fleet--set-p model))
        (funcall orig arg-values info)
      (funcall orig
               (list type
                     (format "%s  [model: %s%s]" desc model
                             (pcase (gptel-fleet-resolve-model-override type model)
                               (`(error ,msg) (format " — INVALID: %s" msg))
                               (`(ok ,b ,m)
                                (if (gptel-fleet-model-allowed-p type b m) ""
                                  (format " — not in %s's :models" type)))))
                     prompt)
               info))))

;;;; Agent tool schema and roster

(defun gptel-fleet--agent-tool-add-model-arg ()
  "Add the `model' argument to the Agent tool (idempotent)."
  (when-let* ((tool (gptel-get-tool "Agent")))
    (unless (cl-find "model" (gptel-tool-args tool)
                     :key (lambda (a) (plist-get a :name)) :test #'equal)
      (setf (gptel-tool-args tool)
            (append (gptel-tool-args tool) (list (copy-sequence gptel-fleet--model-arg)))))))

(defun gptel-fleet--agent-tool-remove-model-arg ()
  "Remove the `model' argument from the Agent tool."
  (when-let* ((tool (ignore-errors (gptel-get-tool "Agent"))))
    (setf (gptel-tool-args tool)
          (cl-remove "model" (gptel-tool-args tool)
                     :key (lambda (a) (plist-get a :name)) :test #'equal))))

(defun gptel-fleet-roster-models (agent)
  "Text for the roster: models AGENT may use per task without asking."
  (let ((ms (gptel-fleet-config-get agent :models)))
    (if ms (format "; per-task without asking: %s" (string-join ms ", ")) "")))

(provide 'gptel-fleet-models)
;;; gptel-fleet-models.el ends here
