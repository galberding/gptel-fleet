;;; gptel-fleet-policy.el --- Approval levels, rules and packages  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Georg Alberding
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Approval levels replace gptel's t / nil / auto.  From strictest to
;; loosest:
;;
;;   deny      Not run.  The model gets an error string explaining why
;;             (no dialog, so unattended runs keep going).
;;   escalate  Subagents: not run; the subagent is told to put the exact
;;             call into an <escalation> block of its final report, so
;;             the parent agent (with its own, wider policy) can decide.
;;             Main agents: treated as `confirm'.
;;   dry-run   Not run.  The model gets "would have run X with Y".  Useful
;;             to watch what a new agent would do.
;;   confirm   Waits for a human decision (inline in the chat or in the
;;             queue buffer).  Answers can be learned for the session or saved.
;;   notify    Runs, and an echo-area message names agent, tool and args.
;;   allow     Runs silently.
;;
;; Every decision and every call is logged independently of the level.
;;
;; A RULE is (LEVEL . MATCHER) where MATCHER is a plist:
;;
;;   :tool     "Name" or ("Name" ...)
;;   :package  sym or (sym ...)
;;   :agent    "name" or ("name" ...)      (useful in global/floor rules)
;;   :arg      :command  plus one condition:
;;     :match REGEXP | :equal VALUE | :under DIR | :not-under DIR
;;     :any-segment REGEXP  (shell: some &&/||/;/| segment matches)
;;     :all-segments REGEXP (shell: every segment matches)
;;   :args     ((:command :match RE) (:path :under DIR) ...)  all must hold
;;   :pred     FUNCTION called with (TOOL-NAME ARGS-PLIST AGENT)
;;
;; An empty matcher matches everything.
;;
;; Resolution order, first hit wins:
;;   1. `gptel-fleet-floor-rules'   (global safety floor, cannot be loosened)
;;      then the skill gate: Skill calls outside the agent's :skills -> deny
;;   2. the agent's :policy rules
;;   3. learned rules (session, then persisted)
;;   4. the agent's :profile rules (`gptel-fleet-profiles')
;;   5. `gptel-fleet-global-rules'
;;   6. the tool's own level, then its package's level
;;   7. for adopted tools without a level: the tool's original :confirm
;;      slot (confirm or allow), so adopting never silently loosens.
;;   8. `gptel-fleet-default-level'

;;; Code:

(require 'gptel-fleet-core)
(require 'gptel-fleet-skills)

(defconst gptel-fleet-levels '(deny escalate dry-run confirm notify allow)
  "Approval levels from strictest to loosest.")

(defcustom gptel-fleet-default-level 'confirm
  "Level used when nothing else decides."
  :group 'gptel-fleet
  :type `(choice ,@(mapcar (lambda (l) `(const ,l)) gptel-fleet-levels)))

(defcustom gptel-fleet-floor-rules
  '((deny :tool "Bash" :arg :command
          :any-segment "\\`\\s-*\\(sudo\\s-+\\)?rm\\s-+-[a-zA-Z]*r[a-zA-Z]*f?\\s-+/\\s-*\\'")
    (escalate :tool "Bash" :arg :command
              :any-segment "\\`\\s-*git\\s-+push\\b")
    ;; Side doors around the approval-gated fleet API (gptel-fleet-api.el):
    (confirm :tool ("Write" "Edit" "Insert" "Mkdir")
             :pred gptel-fleet--touches-fleet-files-p)
    (confirm :tool "Bash" :pred gptel-fleet--touches-fleet-files-p)
    (confirm :tool "Eval" :pred gptel-fleet--eval-touches-internals-p))
  "Rules checked before anything else, for every agent.
Use this for things no agent override or learned rule may loosen."
  :group 'gptel-fleet
  :type '(repeat sexp))

(defun gptel-fleet--fleet-path-strings ()
  "Spellings of the fleet directory an argument might use."
  (let ((d (directory-file-name (expand-file-name gptel-fleet-directory))))
    (delete-dups (list d (abbreviate-file-name d)))))

(defun gptel-fleet--touches-fleet-files-p (_tool args _agent)
  "Non-nil if any string in ARGS refers to the fleet directory."
  (let ((spellings (gptel-fleet--fleet-path-strings)))
    (cl-loop for (_k v) on args by #'cddr
             thereis (and (stringp v)
                          (or (cl-some (lambda (s) (string-search s v)) spellings)
                              (and (string-match-p "\\`[~/.]" v)
                                   (gptel-fleet--path-under-p v gptel-fleet-directory)))))))

(defun gptel-fleet--eval-touches-internals-p (_tool args _agent)
  "Non-nil if an Eval expression uses fleet symbols other than the public API."
  (let ((expr (format "%s" (or (plist-get args :expression) ""))))
    (and (string-match-p "gptel-fleet" expr)
         (with-temp-buffer
           (insert expr)
           (goto-char (point-min))
           (let (hit)
             (while (and (not hit) (re-search-forward "gptel-fleet[-a-z0-9*]*" nil t))
               (unless (string-prefix-p "gptel-fleet-api-" (match-string 0))
                 (setq hit t)))
             hit)))))

(defcustom gptel-fleet-global-rules nil
  "Rules checked after agent, learned and profile rules."
  :group 'gptel-fleet
  :type '(repeat sexp))

(defcustom gptel-fleet-profiles
  '((readonly
     (allow :package (fs-read emacs-introspection))
     (allow :package web)
     (deny  :package (fs-write shell emacs)))
    (executor
     (allow :package (fs-read emacs-introspection))
     (confirm :package fs-write)
     (escalate :package shell))
    (trusted
     (allow :package (fs-read fs-write emacs-introspection orchestration))
     (notify :package (web shell))))
  "Named, reusable rule lists.  Agents pick one with :profile."
  :group 'gptel-fleet
  :type '(alist :key-type symbol :value-type (repeat sexp)))

;;;; Packages

(defvar gptel-fleet--adopted (make-hash-table :test #'equal)
  "Tool name -> plist (:function :async :confirm) of adopted tools.")


(defvar gptel-fleet--packages nil
  "Alist (PACKAGE-SYMBOL . PLIST) of known tool packages.
PLIST keys: :description :level :tools (names) :tool-levels (alist
name . level) :file :origin (builtin or file).")

(defun gptel-fleet-register-package (name &rest props)
  "Register or update tool package NAME with PROPS."
  (let ((old (cdr (assq name gptel-fleet--packages))))
    (while props
      (setq old (plist-put old (pop props) (pop props))))
    (setf (alist-get name gptel-fleet--packages) old)
    old))

(defun gptel-fleet-package (name)
  "Plist of package NAME."
  (cdr (assq name gptel-fleet--packages)))

(defun gptel-fleet-package-add-tool (package tool-name &optional level)
  "Record TOOL-NAME as member of PACKAGE, with optional LEVEL."
  (let* ((plist (gptel-fleet-package package))
         (tools (plist-get plist :tools)))
    (gptel-fleet-register-package
     package :tools (append (remove tool-name tools) (list tool-name)))
    (when level
      (let ((tl (copy-alist (plist-get (gptel-fleet-package package) :tool-levels))))
        (setf (alist-get tool-name tl nil nil #'equal) level)
        (gptel-fleet-register-package package :tool-levels tl)))))

(defun gptel-fleet-tool-package (tool-name)
  "Package symbol that contains TOOL-NAME, or nil."
  (car (cl-find-if (lambda (p) (member tool-name (plist-get (cdr p) :tools)))
                   gptel-fleet--packages)))

(defun gptel-fleet-package-tools (packages)
  "All tool names in PACKAGES (a symbol or list)."
  (cl-loop for p in (ensure-list packages)
           append (plist-get (gptel-fleet-package p) :tools)))

;;;; Shell segment splitting

(defun gptel-fleet-shell-segments (cmd)
  "Split shell CMD on && || ; | and newlines outside quotes.
Heredoc bodies become extra segments, which keeps `:all-segments'
allow rules conservative (they simply fail to match)."
  (let ((segs nil) (cur nil) (i 0) (n (length cmd)) (quote nil))
    (cl-flet ((flush () (let ((s (string-trim (apply #'string (nreverse cur)))))
                          (unless (string-empty-p s) (push s segs))
                          (setq cur nil))))
      (while (< i n)
        (let ((c (aref cmd i))
              (nx (and (< (1+ i) n) (aref cmd (1+ i)))))
          (cond
           ((and quote (eq c ?\\) (eq quote ?\") nx)
            (push c cur) (push nx cur) (cl-incf i))
           (quote (when (eq c quote) (setq quote nil)) (push c cur))
           ((memq c '(?\" ?\')) (setq quote c) (push c cur))
           ((eq c ?\\) (push c cur) (when nx (push nx cur) (cl-incf i)))
           ((or (and (eq c ?&) (eq nx ?&)) (and (eq c ?|) (eq nx ?|)))
            (flush) (cl-incf i))
           ((memq c '(?\; ?| ?\n)) (flush))
           (t (push c cur))))
        (cl-incf i))
      (flush))
    (nreverse segs)))

;;;; Matching

(defun gptel-fleet--path-under-p (path dir)
  "Non-nil if PATH (expanded, symlinks resolved) is inside DIR."
  (and (stringp path)
       (let ((p (file-truename (expand-file-name path)))
             (d (file-name-as-directory (file-truename (expand-file-name dir)))))
         (or (string-prefix-p d p) (equal (file-name-as-directory p) d)))))

(defun gptel-fleet--arg-cond-p (args key cond-plist)
  "Check one argument condition: ARGS plist, KEY, COND-PLIST."
  (let* ((key (if (keywordp key) key (intern (format ":%s" key))))
         (val (plist-get args key))
         (sval (if (stringp val) val (and val (format "%S" val)))))
    (cl-loop for (k v) on cond-plist by #'cddr
             always
             (pcase k
               (:match (and sval (string-match-p v sval)))
               (:equal (equal val v))
               (:under (gptel-fleet--path-under-p val v))
               (:not-under (not (gptel-fleet--path-under-p val v)))
               (:any-segment (and sval (cl-some (lambda (s) (string-match-p v s))
                                                (gptel-fleet-shell-segments sval))))
               (:all-segments (and sval
                                   (let ((segs (gptel-fleet-shell-segments sval)))
                                     (and segs (cl-every (lambda (s) (string-match-p v s))
                                                         segs)))))
               (_ t)))))

(defconst gptel-fleet--arg-cond-keys
  '(:match :equal :under :not-under :any-segment :all-segments))

(defun gptel-fleet-rule-matches-p (matcher tool-name args agent)
  "Non-nil if MATCHER applies to TOOL-NAME with ARGS called by AGENT."
  (and
   (let ((tools (plist-get matcher :tool)))
     (or (null tools) (member tool-name (ensure-list tools))))
   (let ((pkgs (plist-get matcher :package)))
     (or (null pkgs) (memq (gptel-fleet-tool-package tool-name) (ensure-list pkgs))))
   (let ((agents (plist-get matcher :agent)))
     (or (null agents) (member agent (ensure-list agents))))
   (let ((key (plist-get matcher :arg)))
     (or (null key)
         (gptel-fleet--arg-cond-p
          args key
          (cl-loop for (k v) on matcher by #'cddr
                   when (memq k gptel-fleet--arg-cond-keys) append (list k v)))))
   (cl-every (lambda (spec) (gptel-fleet--arg-cond-p args (car spec) (cdr spec)))
             (plist-get matcher :args))
   (let ((pred (plist-get matcher :pred)))
     (or (null pred) (funcall pred tool-name args agent)))))

(defun gptel-fleet--first-match (rules tool-name args agent)
  "First rule in RULES matching; return (LEVEL . RULE)."
  (cl-loop for rule in rules
           when (and (consp rule)
                     (gptel-fleet-rule-matches-p (cdr rule) tool-name args agent))
           return (cons (car rule) rule)))

;;;; Learned rules

(defvar gptel-fleet--session-rules nil
  "Alist (AGENT . RULES) learned this Emacs session.")

(defvar gptel-fleet--learned nil
  "Alist (AGENT . RULES) persisted in `gptel-fleet-learned-file'.")

(defun gptel-fleet-load-learned ()
  "Reload persisted learned rules."
  (setq gptel-fleet--learned (gptel-fleet--read-eld (gptel-fleet-learned-file))))

(defun gptel-fleet-learn (agent rule &optional persist)
  "Add RULE for AGENT, for this session or (PERSIST) on disk."
  (if persist
      (progn
        (push rule (alist-get agent gptel-fleet--learned nil nil #'equal))
        (gptel-fleet--write-eld (gptel-fleet-learned-file) gptel-fleet--learned
                                "Learned approval rules: (AGENT . RULES).  Edit freely."))
    (push rule (alist-get agent gptel-fleet--session-rules nil nil #'equal))))

(defun gptel-fleet-forget-session-rules ()
  "Drop everything learned this session."
  (interactive)
  (setq gptel-fleet--session-rules nil))

;;;; Decision

(defvar gptel-fleet--tool-level-overrides nil
  "Alist (TOOL-NAME . LEVEL) set from the tools list, saved in tool-levels.eld.")

(defun gptel-fleet--tool-levels-file ()
  "File of per-tool default level overrides."
  (expand-file-name "tool-levels.eld" gptel-fleet-directory))

(defun gptel-fleet-load-tool-levels ()
  "Reload per-tool default level overrides."
  (setq gptel-fleet--tool-level-overrides
        (gptel-fleet--read-eld (gptel-fleet--tool-levels-file))))

(defun gptel-fleet-set-tool-level (tool-name level)
  "Persist LEVEL as default for TOOL-NAME (nil removes the override)."
  (setq gptel-fleet--tool-level-overrides
        (assoc-delete-all tool-name gptel-fleet--tool-level-overrides))
  (when level
    (push (cons tool-name level) gptel-fleet--tool-level-overrides))
  (gptel-fleet--write-eld (gptel-fleet--tool-levels-file) gptel-fleet--tool-level-overrides
                          "Per-tool default levels: ((TOOL-NAME . LEVEL) ...)"))

(defun gptel-fleet--tool-default-level (tool-name)
  "Level declared by the tool, its package, or derived from :confirm."
  (let* ((pkg (gptel-fleet-tool-package tool-name))
         (pl (gptel-fleet-package pkg))
         (tool-level (cdr (assoc tool-name (plist-get pl :tool-levels)))))
    (cond
     ((assoc tool-name gptel-fleet--tool-level-overrides)
      (cons (cdr (assoc tool-name gptel-fleet--tool-level-overrides))
            (format "tool-levels.eld %s" tool-name)))
     (tool-level (cons tool-level (format "tool %s" tool-name)))
     ((plist-get pl :level) (cons (plist-get pl :level) (format "package %s" pkg)))
     ((gethash tool-name gptel-fleet--adopted)
      (cons (if (plist-get (gethash tool-name gptel-fleet--adopted) :confirm)
                'confirm 'allow)
            "original :confirm"))
     (t nil))))

(defun gptel-fleet-decide (agent tool-name args &optional main-p)
  "Return (LEVEL . SOURCE) for AGENT calling TOOL-NAME with ARGS plist.
MAIN-P non-nil means AGENT is a top-level agent (escalate -> confirm)."
  (let* ((profile (gptel-fleet-config-get agent :profile))
         (hit
          (or (when-let* ((m (gptel-fleet--first-match
                              gptel-fleet-floor-rules tool-name args agent)))
                (cons (car m) (list 'floor (cdr m))))
              (when (and (equal tool-name "Skill")
                         (not (gptel-fleet-skill-allowed-p agent (plist-get args :skill))))
                (cons 'deny (list 'skills (plist-get args :skill))))
              (when-let* ((m (gptel-fleet--first-match
                              (gptel-fleet-config-get agent :policy)
                              tool-name args agent)))
                (cons (car m) (list 'agent (cdr m))))
              (when-let* ((m (gptel-fleet--first-match
                              (cdr (assoc agent gptel-fleet--session-rules))
                              tool-name args agent)))
                (cons (car m) (list 'session (cdr m))))
              (when-let* ((m (gptel-fleet--first-match
                              (cdr (assoc agent gptel-fleet--learned))
                              tool-name args agent)))
                (cons (car m) (list 'learned (cdr m))))
              (when-let* ((m (and profile
                                  (gptel-fleet--first-match
                                   (cdr (assq profile gptel-fleet-profiles))
                                   tool-name args agent))))
                (cons (car m) (list 'profile profile (cdr m))))
              (when-let* ((m (gptel-fleet--first-match
                              gptel-fleet-global-rules tool-name args agent)))
                (cons (car m) (list 'global (cdr m))))
              (when-let* ((d (gptel-fleet--tool-default-level tool-name)))
                (cons (car d) (list 'default (cdr d))))
              (cons gptel-fleet-default-level (list 'fallback))))
         (level (car hit)))
    (unless (memq level gptel-fleet-levels)
      (display-warning 'gptel-fleet (format "Unknown level %S, using deny" level))
      (setq level 'deny))
    (when (and main-p (eq level 'escalate))
      (setq level 'confirm))
    (cons level (cdr hit))))

(defun gptel-fleet-describe-source (source)
  "Human readable description of a decision SOURCE."
  (pcase source
    (`(floor ,r) (format "floor rule %S" r))
    (`(agent ,r) (format "agent rule %S" r))
    (`(session ,r) (format "session-learned %S" r))
    (`(learned ,r) (format "learned %S" r))
    (`(profile ,p ,r) (format "profile %s: %S" p r))
    (`(global ,r) (format "global rule %S" r))
    (`(default ,d) d)
    (`(fallback) "gptel-fleet-default-level")
    (`(skills ,s) (format "skill %s is not assigned to this agent (:skills)" s))
    (_ (format "%S" source))))

(provide 'gptel-fleet-policy)
;;; gptel-fleet-policy.el ends here
