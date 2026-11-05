;;; gptel-fleet-api.el --- Approval-gated API for agents to manage the fleet  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Georg Alberding
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; One public API, used by you, by agents through `Eval', and by the thin
;; `fleet' tool package.  Read functions return text.  Changing functions
;; build a CHANGE (a list of ops), check it, and then:
;;
;;   - called by you (no agent tool running): apply it directly;
;;   - called while an agent's tool runs (any path: Fleet* tools, Eval, a
;;     nested Agent): never apply.  Queue it for your approval and return
;;     "PENDING chg-…" at once.  The agent can poll `gptel-fleet-api-status'.
;;
;; The approval request shows the file diffs, every (agent, tool) level
;; that changes, which agent asked and by which tool, and a WIDENS
;; warning when anything gains capability.
;;
;; Refused outright (not even queued): changes that widen the requesting
;; agent itself or the agent that spawned it, :function tools from
;; agents (arbitrary code), changes to the floor rules (not exposed).
;;
;; Applied changes are re-registered (`gptel-fleet-apply'), committed to
;; git in `gptel-fleet-directory' with the agent as author, and logged as
;; a `fleet-change' event in the requesting run.
;;
;; Ops:
;;   (:set AGENT KEY VALUE)    (:unset AGENT KEY)
;;   (:new-agent NAME DESCRIPTION SYSTEM-PROMPT)
;;   (:define-tool ID SPEC)    (:tool-level TOOL LEVEL)

;;; Code:

(require 'gptel-fleet-core)
(require 'gptel-fleet-policy)
(require 'gptel-fleet-log)
(require 'gptel-fleet-approval)
(require 'gptel-fleet-skills)
(require 'gptel-fleet-tools)
(require 'gptel-fleet-config)
(require 'gptel-fleet-list)
(require 'gptel-fleet-forge)

(declare-function gptel-fleet-apply "gptel-fleet")
(declare-function gptel-fleet-validate-config "gptel-fleet-config")
(declare-function gptel-fleet-forge-validate "gptel-fleet-forge")
(declare-function gptel-fleet-forge-write "gptel-fleet-forge")
(declare-function gptel-fleet--agent-names "gptel-fleet-list")
(declare-function gptel-fleet--agent-plist "gptel-fleet-list")
(defvar gptel-fleet-agent-source-directory)
(defvar gptel-fleet--tool-specs)

(defcustom gptel-fleet-git t
  "Commit every applied API change in `gptel-fleet-directory' (logs ignored)."
  :group 'gptel-fleet
  :type 'boolean)

(defconst gptel-fleet-api-functions
  '(gptel-fleet-api-overview gptel-fleet-api-agent gptel-fleet-api-explain
    gptel-fleet-api-status gptel-fleet-api-set gptel-fleet-api-assign
    gptel-fleet-api-add-rule gptel-fleet-api-remove-rule gptel-fleet-api-new-agent
    gptel-fleet-api-define-tool gptel-fleet-api-register-skill gptel-fleet-api-set-tool-level)
  "The public fleet API.")

(defvar gptel-fleet--changes (make-hash-table :test #'equal)
  "Change id -> plist (:status :summary :agent :time :reason :result).")

;;;; Simulation and diff

(defun gptel-fleet--simulate (ops)
  "Return (OVERRIDES . TOOL-LEVELS) as they would be after OPS."
  (let ((ov (copy-tree gptel-fleet--overrides))
        (tl (copy-alist gptel-fleet--tool-level-overrides)))
    (dolist (op ops)
      (pcase op
        (`(:set ,agent ,key ,val)
         (let ((cfg (copy-sequence (cdr (assoc agent ov)))))
           (setf (alist-get agent ov nil nil #'equal) (plist-put cfg key val))))
        (`(:unset ,agent ,key)
         (let ((cfg (copy-sequence (cdr (assoc agent ov)))))
           (cl-remf cfg key)
           (setf (alist-get agent ov nil nil #'equal) cfg)))
        (`(:tool-level ,tool ,level)
         (setq tl (assoc-delete-all tool tl))
         (when level (push (cons tool level) tl)))))
    (cons ov tl)))

(defun gptel-fleet--level-rank (level)
  "Looseness rank of LEVEL (higher = looser)."
  (or (cl-position level gptel-fleet-levels) 0))

(defun gptel-fleet--held-tools (agent sim)
  "Tool names AGENT has now or would have under SIM."
  (let* ((now (plist-get (cdr (assoc agent (bound-and-true-p gptel-agent--agents))) :tools))
         (cfg (cdr (assoc agent (car sim)))))
    (delete-dups
     (cl-remove-if-not #'stringp
                       (append now (plist-get cfg :tools)
                               (gptel-fleet-package-tools (plist-get cfg :tool-packages)))))))

(defun gptel-fleet--level-diff (agents sim &optional only-held)
  "List of (AGENT TOOL BEFORE AFTER) that differ for AGENTS under SIM.
With ONLY-HELD, consider only tools each agent has (for display); the
self-promotion check uses all tools."
  (cl-loop
   for agent in agents
   for tools = (or (and only-held (gptel-fleet--held-tools agent sim))
                   (gptel-fleet-tool-names))
   for levels = (lambda (agent tools)
                  (mapcar (lambda (tn)
                            (car (gptel-fleet-decide agent tn nil
                                                     (member agent gptel-fleet-main-agents))))
                          tools))
   for before = (funcall levels agent tools)
   for after = (let ((gptel-fleet--overrides (car sim))
                     (gptel-fleet--tool-level-overrides (cdr sim)))
                 (funcall levels agent tools))
   append (cl-loop for tn in tools for b in before for a in after
                   unless (eq a b) collect (list agent tn b a))))

(defun gptel-fleet--widenings (ops sim level-diff)
  "Human-readable list of ways OPS widen capabilities."
  (let (out)
    (dolist (d level-diff)
      (when (> (gptel-fleet--level-rank (nth 3 d)) (gptel-fleet--level-rank (nth 2 d)))
        (push (format "%s: %s %s → %s" (nth 0 d) (nth 1 d) (nth 2 d) (nth 3 d)) out)))
    (dolist (op ops)
      (pcase op
        (`(:set ,agent ,(and key (or :tools :tool-packages :skills :models)) ,val)
         (let* ((old (plist-get (cdr (assoc agent gptel-fleet--overrides)) key))
                (inherited (not (plist-member (cdr (assoc agent gptel-fleet--overrides)) key)))
                (added (cl-set-difference (ensure-list val) (ensure-list old) :test #'equal)))
           (when (and added (not (and inherited (eq key :skills))))
             (push (format "%s: %s gains %S" agent key added) out))))
        (`(:set ,agent ,(and key (or :enabled :delegate)) t)
         (unless (gptel-fleet-config-get agent key nil)
           (push (format "%s: %s → t" agent key) out)))
        (`(:set ,agent ,(and key (or :backend :model)) ,val)
         (push (format "%s: %s → %s (cost/capability change)" agent key val) out))
        (`(:unset ,agent ,(and key (or :tools :skills :policy :profile)))
         (push (format "%s: %s removed → inherits defaults" agent key) out))
        (`(:new-agent ,name . ,_) (push (format "new agent %s" name) out))
        (`(:register-skill ,path ,agents)
         (let ((name (car (gptel-fleet-validate-skill path))))
           (push (format "skill %s becomes visible to agents inheriting all skills: %s"
                         name (string-join
                               (cl-loop for (a . _) in gptel-agent--agents
                                        when (eq (gptel-fleet-agent-skills a) t) collect a)
                               ", "))
                 out)
           (dolist (a agents)
             (when (plist-member (gptel-fleet-config a) :skills)
               (push (format "%s: :skills gains %s" a name) out)))))
        (`(:define-tool ,id . ,_) (push (format "new/changed tool %s" id) out))))
    (ignore sim)
    (nreverse out)))

(defun gptel-fleet--file-diff (agent new-plist)
  "Unified diff between AGENT's override file and NEW-PLIST."
  (let* ((file (gptel-fleet-override-file agent))
         (old (if (file-exists-p file)
                  (with-temp-buffer (insert-file-contents file) (buffer-string))
                ""))
         (new (let ((tmp (make-temp-file "fleet-new-eld")))
                ;; Render exactly as `gptel-fleet-config-set' writes it, header
                ;; comments included, so the diff shows only real changes.
                (unwind-protect
                    (progn
                      (gptel-fleet--write-eld tmp new-plist
                                              (or (gptel-fleet--leading-comments file)
                                                  (format "gptel-fleet override for agent %s" agent)))
                      (with-temp-buffer (insert-file-contents tmp) (buffer-string)))
                  (delete-file tmp))))
         (a (make-temp-file "fleet-old")) (b (make-temp-file "fleet-new")))
    (unwind-protect
        (progn
          (with-temp-file a (insert old))
          (with-temp-file b (insert new))
          (with-temp-buffer
            (if (executable-find "diff")
                (call-process "diff" nil t nil "-u" "--label" (format "agents/%s.eld" agent)
                              "--label" (format "agents/%s.eld (new)" agent) a b)
              (insert "--- old\n" old "\n+++ new\n" new))
            (buffer-string)))
      (delete-file a) (delete-file b))))

;;;; Checking

(defun gptel-fleet--check-ops (ops ctx)
  "Return a list of error strings for OPS requested under CTX (nil = ok)."
  (let ((errs nil) (sim (gptel-fleet--simulate ops)))
    (dolist (op ops)
      (pcase op
        ((or `(:set ,agent ,key . ,_) `(:unset ,agent ,key))
         (unless (memq key gptel-fleet-override-keys)
           (push (format "unknown key %S (allowed: %S)" key gptel-fleet-override-keys) errs))
         (unless (stringp agent) (push "agent name must be a string" errs))
         (dolist (p (gptel-fleet-validate-config (cdr (assoc agent (car sim)))))
           (unless (string-prefix-p "warning:" p) (push (format "%s: %s" agent p) errs))))
        (`(:tool-level ,tool ,level)
         (unless (gptel-fleet-find-tool tool) (push (format "unknown tool %s" tool) errs))
         (unless (or (null level) (memq level gptel-fleet-levels))
           (push (format "unknown level %S" level) errs)))
        (`(:new-agent ,name ,desc ,system)
         (unless (and (stringp name) (string-match-p "\\`[a-z][a-z0-9-]*\\'" name))
           (push "agent name must be lowercase letters, digits and dashes" errs))
         (when (member name (gptel-fleet--agent-names))
           (push (format "agent %s already exists" name) errs))
         (unless (and (stringp desc) (stringp system))
           (push "description and system prompt must be strings" errs)))
        (`(:register-skill ,path ,agents)
         (dolist (p (cdr (gptel-fleet-validate-skill path)))
           (unless (string-prefix-p "warning:" p) (push (format "skill: %s" p) errs)))
         (dolist (a agents)
           (unless (member a (gptel-fleet--agent-names)) (push (format "unknown agent %s" a) errs))))
        (`(:define-tool ,id ,spec)
         (when (and ctx (plist-get spec :function))
           (push ":function tools cannot be defined by agents (arbitrary code); use :command" errs))
         (dolist (p (gptel-fleet-forge-validate id spec))
           (unless (string-prefix-p "warning:" p) (push (format "tool %s: %s" id p) errs))))
        (_ (push (format "unknown op %S" op) errs))))
    (nreverse errs)))

(defun gptel-fleet--affected-agents (ops)
  "Agents whose levels or config OPS may change."
  (if (cl-some (lambda (op) (memq (car op) '(:tool-level :define-tool))) ops)
      (gptel-fleet--agent-names)
    (delete-dups (cl-loop for op in ops
                          when (memq (car op) '(:set :unset)) collect (nth 1 op)))))

(defun gptel-fleet--self-promotion (ctx level-diff widen)
  "Non-nil (a string) if the change widens CTX's own agent or its parent.
LEVEL-DIFF and WIDEN come from the simulation.  Narrowing is allowed."
  (when ctx
    (let ((protected (delq nil (list (plist-get ctx :agent) (plist-get ctx :parent-agent)))))
      (or (cl-loop for d in level-diff
                   when (and (member (nth 0 d) protected)
                             (> (gptel-fleet--level-rank (nth 3 d))
                                (gptel-fleet--level-rank (nth 2 d))))
                   return (format "would loosen %s for %s" (nth 1 d) (nth 0 d)))
          (cl-loop for w in widen
                   when (cl-some (lambda (a) (string-prefix-p (concat a ": ") w)) protected)
                   return w)))))

;;;; Applying

(defun gptel-fleet--git (&rest args)
  "Run git with ARGS in the fleet directory; return exit code."
  (let ((default-directory gptel-fleet-directory))
    (apply #'call-process "git" nil nil nil args)))

(defun gptel-fleet--git-commit (summary author)
  "Commit fleet config changes with SUMMARY as AUTHOR."
  (when (and gptel-fleet-git (executable-find "git"))
    (unless (file-directory-p (expand-file-name ".git" gptel-fleet-directory))
      (gptel-fleet--git "init" "-q")
      (with-temp-file (expand-file-name ".gitignore" gptel-fleet-directory)
        (insert "logs/\n*.elc\n")))
    (gptel-fleet--git "add" "-A")
    (gptel-fleet--git "-c" (format "user.name=%s" author)
                      "-c" (format "user.email=%s@gptel-fleet.local"
                                   (gptel-fleet--safe-name author))
                      "commit" "-q" "--allow-empty" "-m" summary)))

(defun gptel-fleet--apply-ops (ops)
  "Perform OPS on disk."
  (dolist (op ops)
    (pcase op
      (`(:set ,agent ,key ,val) (gptel-fleet-config-set agent key val))
      (`(:unset ,agent ,key)
       (let ((cfg (copy-sequence (gptel-fleet-config agent))))
         (cl-remf cfg key)
         (setf (alist-get agent gptel-fleet--overrides nil nil #'equal) cfg)
         (let ((file (gptel-fleet-override-file agent)))
           (gptel-fleet--write-eld file cfg (gptel-fleet--leading-comments file)))))
      (`(:tool-level ,tool ,level) (gptel-fleet-set-tool-level tool level))
      (`(:new-agent ,name ,desc ,system)
       (let* ((dir (file-name-as-directory gptel-fleet-agent-source-directory))
              (file (expand-file-name (concat name ".md") dir)))
         (make-directory dir t)
         (with-temp-file file
           (insert "---\nname: " name "\ndescription: >\n  "
                   (replace-regexp-in-string "\n" " " desc)
                   "\ntools:\n  - Read\n  - Grep\n  - Glob\n---\n" system "\n"))
         ;; New agents start disabled and read-only: enabling is a second,
         ;; separately approved step.
         (gptel-fleet-config-set name :enabled nil)
         (gptel-fleet-config-set name :profile 'readonly)))
      (`(:define-tool ,id ,spec)
       (load (gptel-fleet-forge-write id spec) nil t))
      (`(:register-skill ,path ,agents)
       (gptel-fleet--register-skill-ops path agents))))
  (when (fboundp 'gptel-fleet-apply) (gptel-fleet-apply)))

(defun gptel-fleet--change-detail (ops level-diff widen requester)
  "Approval text for OPS."
  (concat
   (format "Requested by: %s%s\n\n" (or requester "you")
           (if-let* ((tool (and gptel-fleet--acting (plist-get gptel-fleet--acting :via))))
               (format " via %s" tool) ""))
   (if widen
       (concat "⚠ WIDENS:\n" (mapconcat (lambda (w) (concat "  • " w)) widen "\n") "\n\n")
     "Only narrows or keeps capabilities.\n\n")
   (if level-diff
       (concat "Level changes (agent  tool  before → after):\n"
               (mapconcat (lambda (d) (format "  %-16s %-20s %s → %s"
                                              (nth 0 d) (nth 1 d) (nth 2 d) (nth 3 d)))
                          level-diff "\n")
               "\n\n")
     "")
   (let ((sim (car (gptel-fleet--simulate ops))))
     (mapconcat (lambda (agent) (gptel-fleet--file-diff agent (cdr (assoc agent sim))))
                (delete-dups (cl-loop for op in ops
                                      when (memq (car op) '(:set :unset)) collect (nth 1 op)))
                "\n"))
   (mapconcat (lambda (op)
                (pcase op
                  (`(:new-agent ,n ,d ,s)
                   (format "New agent %s (created disabled, profile readonly)\n  %s\n--- prompt ---\n%s\n" n d s))
                  (`(:define-tool ,id ,spec)
                   (format "Tool %s:\n%s\n" id (pp-to-string spec)))
                  (`(:tool-level ,tl ,lv) (format "Default level of %s → %s\n" tl lv))
                  (`(:register-skill ,path ,_)
                   (let ((f (gptel-fleet--skill-file path)))
                     (format "Register skill %s\n--- %s ---\n%s\n" (abbreviate-file-name f)
                             (file-name-nondirectory f)
                             (if (file-readable-p f)
                                 (with-temp-buffer (insert-file-contents f) (buffer-string))
                               "(missing)"))))
                  (_ "")))
              ops "")))

(defun gptel-fleet-api--submit (summary ops)
  "Check OPS; apply them (your call) or queue them (agent call).  Return text."
  (let* ((ctx gptel-fleet--acting)
         (requester (plist-get ctx :agent))
         (errs (gptel-fleet--check-ops ops ctx)))
    (if errs
        (format "REJECTED (invalid): %s" (string-join errs "; "))
      (let* ((sim (gptel-fleet--simulate ops))
             (level-diff (gptel-fleet--level-diff (gptel-fleet--affected-agents ops) sim))
             (widen (gptel-fleet--widenings ops sim level-diff))
             (self (gptel-fleet--self-promotion ctx level-diff widen))
             (id (gptel-fleet-new-id "chg-")))
        (cond
         (self
          (puthash id (list :status 'refused :summary summary :agent requester
                            :reason self :time (current-time))
                   gptel-fleet--changes)
          (gptel-fleet-log-event ctx "fleet-change"
                                 (list :id id :summary summary :status "refused" :reason self))
          (format "REFUSED %s: %s.  Agents cannot widen their own (or their parent's) \
capabilities; ask the human to do it." id self))
         ((null ctx)
          (gptel-fleet--apply-ops ops)
          (gptel-fleet--git-commit summary (or user-login-name "user"))
          (puthash id (list :status 'applied :summary summary :time (current-time))
                   gptel-fleet--changes)
          (format "APPLIED %s: %s" id summary))
         (t
          (puthash id (list :status 'pending :summary summary :agent requester
                            :time (current-time))
                   gptel-fleet--changes)
          (gptel-fleet-log-event ctx "fleet-change"
                                 (list :id id :summary summary :status "pending"
                                       :widens widen))
          (setq level-diff (gptel-fleet--level-diff
                            (gptel-fleet--affected-agents ops) sim t))
          (gptel-fleet-enqueue
           (gptel-fleet-request-create
            :id id :time (current-time) :ctx ctx :tool-name "fleet-change"
            :args (list :summary summary) :source '(fleet-api) :no-learn t
            :detail (gptel-fleet--change-detail ops level-diff widen requester)
            :run-fn (lambda (_ &rest _)
                      (condition-case err
                          (progn
                            (gptel-fleet--apply-ops ops)
                            (gptel-fleet--git-commit summary requester)
                            (puthash id (list :status 'applied :summary summary
                                              :agent requester :time (current-time))
                                     gptel-fleet--changes)
                            (gptel-fleet-log-event ctx "fleet-change"
                                                   (list :id id :status "applied")))
                        (error (puthash id (list :status 'failed :summary summary
                                                 :reason (error-message-string err))
                                        gptel-fleet--changes))))
            :deny-fn (lambda (reason)
                       (puthash id (list :status 'denied :summary summary :agent requester
                                         :reason reason :time (current-time))
                                gptel-fleet--changes)
                       (gptel-fleet-log-event ctx "fleet-change"
                                              (list :id id :status "denied" :reason reason)))))
          (format "PENDING %s: %s.  Queued for human approval%s; nothing has changed yet. \
Check later with gptel-fleet-api-status (tool FleetStatus) and continue with other work."
                  id summary (if widen " (widens capabilities)" ""))))))))

;;;; Public API: reading

(defun gptel-fleet-api-overview ()
  "Explain the fleet architecture and its live state, for an agent or you."
  (let ((agents (gptel-fleet--agent-names)))
    (concat
     "# gptel-fleet\n"
     "Agents are gptel-agent definitions (.md) plus a fleet override per agent \
(lisp plist).  Tools are grouped in packages.  Every tool call gets a LEVEL: "
     (mapconcat #'symbol-name gptel-fleet-levels " < ")
     " (strict → loose).  Levels come from rules (LEVEL . MATCHER), first match wins, \
order: floor rules, skill gate, agent :policy, learned, :profile, global rules, tool/package default.\n"
     "Matcher keys: :tool :package :agent :arg+(:match :equal :under :not-under :any-segment \
:all-segments) :args :pred.\n"
     "Override keys: " (mapconcat #'symbol-name gptel-fleet-override-keys " ") "\n"
     "Changes made through gptel-fleet-api-* are queued for HUMAN approval when an agent \
asks; you get PENDING <id> back.  You can never widen yourself or your parent agent.\n\n"
     "## API (call via Eval or the Fleet* tools)\n"
     (mapconcat (lambda (f)
                  (format "(%s%s) — %s" f
                          (let ((al (help-function-arglist f t)))
                            (if al (concat " " (mapconcat (lambda (a) (format "%s" a)) al " ")) ""))
                          (car (split-string (or (documentation f t) "") "\n"))))
                gptel-fleet-api-functions "\n")
     "\n\n## Agents (name state backend:model #tools skills profile)\n"
     (mapconcat
      (lambda (a)
        (let ((p (gptel-fleet--agent-plist a))
              (sk (gptel-fleet-agent-skills a)))
          (format "%s %s %s:%s %d %s %s" a
                  (cond ((member a gptel-fleet-main-agents) "main")
                        ((not (gptel-fleet-agent-enabled-p a)) "off")
                        ((not (gptel-fleet-delegate-p a)) "nodelegate")
                        (t "on"))
                  (let ((b (plist-get p :backend)))
                    (cond ((stringp b) b) ((and b (gptel-backend-p b)) (gptel-backend-name b))
                          (t "default")))
                  (or (plist-get p :model) "default")
                  (length (plist-get p :tools))
                  (if (eq sk t) "all" (format "%S" sk))
                  (or (gptel-fleet-config-get a :profile) "-"))))
      agents "\n")
     "\n\n## Packages (name default-level: tools)\n"
     (mapconcat (lambda (p)
                  (format "%s %s: %s" (car p) (or (plist-get (cdr p) :level) "per-tool")
                          (string-join (plist-get (cdr p) :tools) " ")))
                gptel-fleet--packages "\n")
     "\n\n## Skills\n" (string-join (gptel-fleet-skill-names) " ")
     "\n\n## Profiles\n" (mapconcat (lambda (p) (symbol-name (car p))) gptel-fleet-profiles " ")
     "\n\n## Backends:models\n"
     (mapconcat (lambda (b) (format "%s: %s" (car b)
                                    (mapconcat (lambda (m) (format "%s" (if (consp m) (car m) m)))
                                               (gptel-backend-models (cdr b)) " ")))
                gptel--known-backends "\n")
     "\n")))

(defun gptel-fleet-api-agent (agent)
  "Show AGENT's override, effective setup and per-tool levels."
  (let ((p (gptel-fleet--agent-plist agent)))
    (unless (or p (gptel-fleet-config agent)) (error "Unknown agent %s" agent))
    (concat
     (format "# %s\nOverride: %S\n" agent (gptel-fleet-config agent))
     (format "Effective: backend %s, model %s\nPer-task models without asking: %s\nTools: %s\nSkills: %s\n"
             (or (plist-get p :backend) "default") (or (plist-get p :model) "default")
             (let ((ms (gptel-fleet-config-get agent :models)))
               (if ms (string-join ms ", ") "none (any other model needs approval)"))
             (string-join (cl-remove-if-not #'stringp (plist-get p :tools)) " ")
             (let ((s (gptel-fleet-agent-skills agent))) (if (eq s t) "all" (string-join s " "))))
     "Levels (calls with no special args):\n"
     (mapconcat (lambda (tn)
                  (let ((d (gptel-fleet-decide agent tn nil (member agent gptel-fleet-main-agents))))
                    (format "  %-22s %-8s %s" tn (car d) (gptel-fleet-describe-source (cdr d)))))
                (cl-remove-if-not #'stringp (plist-get p :tools)) "\n"))))

(defun gptel-fleet-api-explain (agent tool &optional args)
  "Level AGENT gets for TOOL with ARGS (a plist like (:command \"ls\")), and why."
  (let ((d (gptel-fleet-decide agent tool args (member agent gptel-fleet-main-agents))))
    (format "%s → %s (%s)" tool (car d) (gptel-fleet-describe-source (cdr d)))))

(defun gptel-fleet-api-status (id)
  "Status of change ID (pending, applied, denied, refused, failed)."
  (if-let* ((c (gethash id gptel-fleet--changes)))
      (format "%s %s: %s%s" id (plist-get c :status) (plist-get c :summary)
              (if (plist-get c :reason) (format " (reason: %s)" (plist-get c :reason)) ""))
    (format "unknown change %s" id)))

;;;; Public API: changing (all approval-gated for agents)

(defun gptel-fleet-api-set (agent key value)
  "Set override KEY of AGENT to VALUE (VALUE :unset removes the key)."
  (gptel-fleet-api--submit
   (format "%s: %s %s" agent (if (eq value :unset) "unset" "set") key)
   (list (if (eq value :unset) (list :unset agent key) (list :set agent key value)))))

(defun gptel-fleet-api-assign (agent kind names)
  "Set AGENT's KIND (tools, packages or skills) to NAMES, or :inherit."
  (let ((key (pcase kind
               ((or 'tools "tools") :tools)
               ((or 'packages "packages") :tool-packages)
               ((or 'skills "skills") :skills)
               (_ (error "KIND must be tools, packages or skills")))))
    (gptel-fleet-api-set agent key
                         (cond ((memq names '(:inherit inherit)) :unset)
                               ((eq key :tool-packages)
                                (mapcar (lambda (n) (if (symbolp n) n (intern n))) names))
                               (t (mapcar (lambda (n) (format "%s" n)) names))))))

(defun gptel-fleet-api-add-rule (agent rule &optional position)
  "Insert RULE into AGENT's :policy at POSITION (default 0 = first)."
  (let* ((rules (copy-sequence (gptel-fleet-config-get agent :policy)))
         (pos (min (or position 0) (length rules))))
    (gptel-fleet-api-set agent :policy
                         (append (cl-subseq rules 0 pos) (list rule) (nthcdr pos rules)))))

(defun gptel-fleet-api-remove-rule (agent index)
  "Remove rule number INDEX (0-based) from AGENT's :policy."
  (let ((rules (gptel-fleet-config-get agent :policy)))
    (unless (< -1 index (length rules)) (error "No rule %d" index))
    (gptel-fleet-api-set agent :policy (append (cl-subseq rules 0 index)
                                               (nthcdr (1+ index) rules)))))

(defun gptel-fleet-api-new-agent (name description system-prompt)
  "Create agent NAME with DESCRIPTION and SYSTEM-PROMPT.
It starts disabled with profile readonly; enabling is a separate change."
  (gptel-fleet-api--submit (format "new agent %s" name)
                           (list (list :new-agent name description system-prompt))))

(defun gptel-fleet-api-define-tool (id spec)
  "Create or replace command tool ID from SPEC (see gptel-fleet-tools.el)."
  (gptel-fleet-api--submit (format "define tool %s (%s)" id (plist-get spec :name))
                           (list (list :define-tool id spec))))

(defun gptel-fleet-api-register-skill (path &optional agents)
  "Load the skill at PATH (dir or SKILL.md) now; add it to AGENTS' :skills lists.
Agents that inherit all skills see it without being listed."
  (gptel-fleet-api--submit
   (format "register skill %s%s" (abbreviate-file-name path)
           (if agents (format " for %s" (string-join agents ", ")) ""))
   (list (list :register-skill (expand-file-name path) agents))))

(defun gptel-fleet-api-set-tool-level (tool level)
  "Set TOOL's default LEVEL (nil resets)."
  (gptel-fleet-api--submit (format "default level of %s → %s" tool level)
                           (list (list :tool-level tool level))))

;;;; The `fleet' tool package (thin wrappers; approval happens in the API)

(defun gptel-fleet--read-data (s)
  "Read S as Lisp data.  Emacs `read' never evaluates (no #. syntax)."
  (if (not (stringp s)) s
    (car (read-from-string s))))

(defun gptel-fleet--api-tool (name fn args description)
  "Register fleet API tool NAME calling FN."
  (gptel-fleet-register-tool
   (intern (concat "fleet-" (downcase name)))
   (list :package 'fleet :name name :description description :args args
         :level 'allow
         :function (lambda (&rest vals)
                     (let ((gptel-fleet--acting
                            (and gptel-fleet--acting
                                 (plist-put (copy-sequence gptel-fleet--acting) :via name))))
                       (condition-case err (apply fn vals)
                         (error (format "Error: %s" (error-message-string err)))))))))

(defun gptel-fleet-register-api-tools ()
  "Register the `fleet' tool package."
  (gptel-fleet-register-package
   'fleet :origin 'builtin :level 'allow
   :description "Inspect and (with human approval) change agents, tools and skills")
  (gptel-fleet--api-tool
   "FleetOverview" #'gptel-fleet-api-overview nil
   "Explain the agent fleet (architecture, levels, rules) and list its live state. Call first.")
  (gptel-fleet--api-tool
   "FleetAgent" #'gptel-fleet-api-agent
   '((:name "agent" :type string :description "Agent name"))
   "Show one agent's override, effective tools/skills/model and per-tool levels.")
  (gptel-fleet--api-tool
   "FleetExplain"
   (lambda (agent tool &optional args) (gptel-fleet-api-explain agent tool (gptel-fleet--read-data args)))
   '((:name "agent" :type string :description "Agent name")
     (:name "tool" :type string :description "Tool name")
     (:name "args" :type string :optional t
            :description "Tool args as a Lisp plist, e.g. (:command \"git push\")"))
   "Which approval level an agent gets for a tool call, and which rule decides it.")
  (gptel-fleet--api-tool
   "FleetStatus" #'gptel-fleet-api-status
   '((:name "id" :type string :description "Change id chg-…"))
   "Status of a change you requested.")
  (gptel-fleet--api-tool
   "FleetSet"
   (lambda (agent key value)
     (gptel-fleet-api-set agent (intern (if (string-prefix-p ":" key) key (concat ":" key)))
                          (gptel-fleet--read-data value)))
   '((:name "agent" :type string :description "Agent name")
     (:name "key" :type string :description "Override key, e.g. :model, :enabled, :profile, :policy")
     (:name "value" :type string
            :description "Lisp data, e.g. qwen3-coder, t, nil, \"Name\", ((deny :tool \"Bash\")), or :unset"))
   "Request a change to one override key of an agent (queued for human approval).")
  (gptel-fleet--api-tool
   "FleetAssign"
   (lambda (agent kind names) (gptel-fleet-api-assign agent kind (gptel-fleet--read-data names)))
   '((:name "agent" :type string :description "Agent name")
     (:name "kind" :type string :enum ["tools" "packages" "skills"] :description "What to assign")
     (:name "names" :type string
            :description "Lisp list of names, e.g. (\"Read\" \"Grep\"), or :inherit"))
   "Request a new tool, package or skill assignment for an agent (queued for approval).")
  (gptel-fleet--api-tool
   "FleetAddRule"
   (lambda (agent rule &optional position)
     (gptel-fleet-api-add-rule agent (gptel-fleet--read-data rule) position))
   '((:name "agent" :type string :description "Agent name")
     (:name "rule" :type string :description "Rule as Lisp, e.g. (deny :tool \"Bash\" :arg :command :any-segment \"^rm \")")
     (:name "position" :type integer :optional t :description "Insert position, default 0"))
   "Request a new approval rule for an agent (queued for approval).")
  (gptel-fleet--api-tool
   "FleetNewAgent" #'gptel-fleet-api-new-agent
   '((:name "name" :type string :description "lowercase-name")
     (:name "description" :type string :description "One line: what the agent is for")
     (:name "system_prompt" :type string :description "The agent's system prompt"))
   "Request a new agent (created disabled and read-only; queued for approval).")
  (gptel-fleet--api-tool
   "FleetRegisterSkill"
   (lambda (path &optional agents)
     (gptel-fleet-api-register-skill path (gptel-fleet--read-data agents)))
   '((:name "path" :type string :description "Skill directory or its SKILL.md")
     (:name "agents" :type string :optional t
            :description "Lisp list of agents to add it to, e.g. (\"researcher\"); agents inheriting all skills see it anyway"))
   "Load a skill you just created so agents can use it without restarting Emacs \
(queued for human approval).")
  (gptel-fleet--api-tool
   "FleetDefineTool"
   (lambda (id spec) (gptel-fleet-api-define-tool (intern id) (gptel-fleet--read-data spec)))
   '((:name "id" :type string :description "tool id, e.g. cargo-test")
     (:name "spec" :type string
            :description "Lisp plist: (:package P :name \"Name\" :description \"…\" :args (…) :level confirm :command (\"prog\" \"sub\" ARG …))"))
   "Request a new command tool (argv, no shell; queued for approval)."))

(provide 'gptel-fleet-api)
;;; gptel-fleet-api.el ends here
