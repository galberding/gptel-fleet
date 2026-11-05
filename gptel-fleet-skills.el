;;; gptel-fleet-skills.el --- Per-agent skill assignment  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Georg Alberding
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; gptel-agent expands {{SKILLS}} identically for every agent: each one
;; sees every skill found in `gptel-agent-skill-dirs'.  An override key
;; changes that per agent:
;;
;;   :skills absent              inherit: all skills (gptel-agent default)
;;   :skills ()                  no skills; the Skill tool is removed
;;   :skills ("pdf" "gitlab-*")  only these; glob patterns allowed
;;
;; Two layers enforce it:
;;
;; 1. Visibility: the agent's expanded <available_skills> block is
;;    replaced by a filtered one (appended if its prompt has no
;;    {{SKILLS}} but it has the Skill tool).
;; 2. Enforcement: `gptel-fleet-decide' denies a Skill call for a skill
;;    outside the agent's set before any agent rule is consulted, so a
;;    model that guesses a name it was not shown still gets nothing.

;;; Code:

(require 'gptel-fleet-core)

(defvar gptel-agent--skills)
(declare-function gptel-agent--skills-system-message "gptel-agent")

(defun gptel-fleet-skill-names ()
  "Names of all skills gptel-agent knows."
  (mapcar #'car (bound-and-true-p gptel-agent--skills)))

(defun gptel-fleet--skill-pattern-p (s)
  "Non-nil if S contains glob characters."
  (string-match-p "[*?[]" s))

(defun gptel-fleet-skill-matches-p (name patterns)
  "Non-nil if skill NAME matches one of PATTERNS (names or globs)."
  (cl-some (lambda (p)
             (if (gptel-fleet--skill-pattern-p p)
                 (string-match-p (wildcard-to-regexp p) name)
               (string= p name)))
           patterns))

(defun gptel-fleet-agent-skills (agent)
  "Skill names visible to AGENT, or t when it inherits all of them."
  (let ((cfg (gptel-fleet-config agent)))
    (if (not (plist-member cfg :skills))
        t
      (let ((pats (plist-get cfg :skills)))
        (cl-remove-if-not (lambda (n) (gptel-fleet-skill-matches-p n pats))
                          (gptel-fleet-skill-names))))))

(defun gptel-fleet-skill-allowed-p (agent skill)
  "Non-nil if AGENT may load SKILL."
  (let ((cfg (gptel-fleet-config agent)))
    (or (not (plist-member cfg :skills))
        (gptel-fleet-skill-matches-p (format "%s" skill) (plist-get cfg :skills)))))

(defun gptel-fleet--skills-message (names)
  "gptel-agent's skills block restricted to NAMES."
  (gptel-agent--skills-system-message
   (cl-remove-if-not (lambda (e) (member (car e) names)) gptel-agent--skills)))

(defun gptel-fleet-apply-skills (agent plist)
  "Return PLIST of AGENT with its skills block filtered."
  (let ((skills (gptel-fleet-agent-skills agent))
        (sys (plist-get plist :system)))
    (if (or (eq skills t) (not (stringp sys)))
        plist
      (let* ((full (gptel-agent--skills-system-message gptel-agent--skills))
             (mine (if skills (gptel-fleet--skills-message skills) ""))
             (has-skill-tool (member "Skill" (plist-get plist :tools)))
             (new-sys (cond ((string-search full sys) (string-replace full mine sys))
                            ((and skills has-skill-tool) (concat sys "\n\n" mine))
                            (t sys)))
             (p (plist-put (copy-sequence plist) :system new-sys)))
        (if skills p
          (plist-put p :tools (remove "Skill" (plist-get p :tools))))))))

;;;; Registering a new skill without restarting Emacs

(declare-function gptel-agent-parse-markdown-frontmatter "gptel-agent")
(declare-function gptel-agent--update-skills "gptel-agent")
(declare-function gptel-fleet-apply "gptel-fleet")
(defvar gptel-agent-skill-dirs)

(defun gptel-fleet--skill-file (path)
  "SKILL.md for PATH (the file itself or its directory)."
  (let ((p (expand-file-name path)))
    (if (file-directory-p p) (expand-file-name "SKILL.md" p) p)))

(defun gptel-fleet-validate-skill (path)
  "Check the skill at PATH.  Return (NAME . PROBLEMS).
Problems starting with \"warning:\" do not block registration."
  (let* ((file (gptel-fleet--skill-file path))
         (errs nil) (name nil))
    (cl-flet ((err (fmt &rest a) (push (apply #'format fmt a) errs)))
      (cond
       ((not (file-readable-p file)) (err "%s not found" file))
       ((not (equal (file-name-nondirectory file) "SKILL.md"))
        (err "a skill is a directory with a SKILL.md, got %s" file))
       (t
        (let ((meta (condition-case e
                        (gptel-agent-parse-markdown-frontmatter file nil nil t)
                      (error (err "frontmatter: %s" (error-message-string e)) nil))))
          (setq name (plist-get meta :name))
          (cond ((not (stringp name))
                 (err "missing \"name:\" in the frontmatter (all such skills would be called SKILL)"))
                ((not (string-match-p "\\`[a-z0-9][a-z0-9-]*\\'" name))
                 (err "warning: name %S should be lowercase letters, digits and dashes" name)))
          (unless (stringp (plist-get meta :description))
            (err "missing \"description:\" (gptel-agent ignores skills without one)"))
          (when (and (stringp name)
                     (not (equal name (file-name-nondirectory
                                       (directory-file-name (file-name-directory file))))))
            (err "warning: name %S differs from its directory name" name))
          (when-let* ((other (and (stringp name) (assoc name gptel-agent--skills)))
                      ((not (file-equal-p (cadr other) (file-name-directory file)))))
            (err "warning: a skill %S already exists at %s; precedence decides which wins"
                 name (abbreviate-file-name (cadr other))))))))
    (cons name (nreverse errs))))

(defun gptel-fleet--skill-visible-to (name)
  "Agents that see skill NAME with the current overrides."
  (cl-loop for (agent . _) in gptel-agent--agents
           for s = (gptel-fleet-agent-skills agent)
           when (or (eq s t) (member name s)) collect agent))

(defun gptel-fleet--register-skill-ops (path agents)
  "Perform registration of PATH and assignment to AGENTS.  Return a report."
  (let* ((check (gptel-fleet-validate-skill path))
         (name (car check))
         (errors (cl-remove-if (lambda (p) (string-prefix-p "warning:" p)) (cdr check))))
    (when errors (error "Skill not registered: %s" (string-join errors "; ")))
    (gptel-agent--update-skills)
    (unless (assoc name gptel-agent--skills)
      (error "Skill %s is not inside `gptel-agent-skill-dirs' (%s), so gptel-agent cannot find it"
             name (string-join gptel-agent-skill-dirs ", ")))
    (dolist (a agents)
      (let ((cfg (gptel-fleet-config a)))
        (when (plist-member cfg :skills)    ; inheriting agents see it anyway
          (let ((cur (plist-get cfg :skills)))
            (unless (gptel-fleet-skill-matches-p name cur)
              (gptel-fleet-config-set a :skills (append cur (list name))))))))
    (when (fboundp 'gptel-fleet-apply) (gptel-fleet-apply))
    (format "Skill %s registered (%s).  Visible to: %s"
            name (abbreviate-file-name (cadr (assoc name gptel-agent--skills)))
            (string-join (gptel-fleet--skill-visible-to name) ", "))))

;;;###autoload
(defun gptel-fleet-register-skill (path &optional agents)
  "Load the new skill at PATH now and optionally assign it to AGENTS.
PATH is a skill directory or its SKILL.md (default: the file you are
visiting).  Agents that inherit all skills see it right away; agents with
an explicit :skills list get it added if listed in AGENTS."
  (interactive
   (let* ((default (and buffer-file-name
                        (equal (file-name-nondirectory buffer-file-name) "SKILL.md")
                        buffer-file-name))
          (path (read-file-name "Skill (directory or SKILL.md): " nil default t default))
          (agents (completing-read-multiple
                   "Assign to agents with their own skill list (empty: none): "
                   (cl-loop for (a . _) in gptel-agent--agents
                            when (plist-member (gptel-fleet-config a) :skills) collect a))))
     (list path agents)))
  (let ((report (gptel-fleet--register-skill-ops path agents)))
    (message "%s" report)
    report))

(provide 'gptel-fleet-skills)
;;; gptel-fleet-skills.el ends here
