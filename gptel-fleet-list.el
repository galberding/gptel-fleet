;;; gptel-fleet-list.el --- Package-menu style lists for agents, tools, runs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Georg Alberding
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `gptel-fleet-list-agents'   all agents (like `list-packages')
;; `gptel-fleet-list-tools'    all tools with package, kind and default level
;; `gptel-fleet-policy-matrix' effective level of every tool for one agent
;; `gptel-fleet-list-runs'     logged runs; RET rebuilds the run tree

;;; Code:

(require 'gptel-fleet-core)
(require 'gptel-fleet-policy)
(require 'gptel-fleet-log)
(require 'gptel-fleet-tools)
(require 'gptel-fleet-config)
(require 'gptel-fleet-skills)
(require 'tabulated-list)

(declare-function gptel-fleet-apply "gptel-fleet")
(declare-function gptel-fleet-forge-edit-tool "gptel-fleet-forge")
(declare-function gptel-fleet-forge-test-tool "gptel-fleet-forge")
(declare-function gptel-agent-read-file "gptel-agent")
(defvar gptel-fleet--agent-files)
(defvar gptel-agent--agents)

(defun gptel-fleet--config-unset (agent key)
  "Remove KEY from AGENT's override (it inherits again)."
  (let ((cfg (copy-sequence (gptel-fleet-config agent))))
    (cl-remf cfg key)
    (setf (alist-get agent gptel-fleet--overrides nil nil #'equal) cfg)
    (let ((file (gptel-fleet-override-file agent)))
      (gptel-fleet--write-eld file cfg (gptel-fleet--leading-comments file)))))

;;;; Agents

(defun gptel-fleet--run-counts ()
  "Hash agent -> number of logged runs."
  (let ((h (make-hash-table :test #'equal)))
    (dolist (e (gptel-fleet-log-index))
      (cl-incf (gethash (alist-get 'agent e) h 0)))
    h))

(defun gptel-fleet--agent-names ()
  "All known agent names, including disabled ones."
  (delete-dups (append (mapcar #'car gptel-fleet--agent-files)
                       (mapcar #'car (bound-and-true-p gptel-agent--agents))
                       (mapcar #'car gptel-fleet--overrides))))

(defun gptel-fleet--agent-plist (name)
  "Effective plist of NAME, or its metadata if it is disabled."
  (or (cdr (assoc name gptel-agent--agents))
      (when-let* ((f (cdr (assoc name gptel-fleet--agent-files))))
        (cdr (ignore-errors (gptel-agent-read-file f nil t))))))

(defun gptel-fleet--agent-state (name)
  "Short state string for NAME."
  (cond ((member name gptel-fleet-main-agents) "main")
        ((not (gptel-fleet-agent-enabled-p name)) "off")
        ((not (gptel-fleet-delegate-p name)) "nodel")
        (t "on")))

(defun gptel-fleet--fmt-backend (b)
  "Display backend B."
  (cond ((null b) "·inherit") ((stringp b) b)
        ((gptel-backend-p b) (gptel-backend-name b)) (t (format "%s" b))))

(defun gptel-fleet--agents-entries ()
  "Entries for the agent list."
  (let ((runs (gptel-fleet--run-counts)))
    (mapcar
     (lambda (name)
       (let* ((p (gptel-fleet--agent-plist name))
              (state (gptel-fleet--agent-state name))
              (face (pcase state ("off" 'shadow) ("main" 'bold) (_ 'default))))
         (list name
               (vector
                (propertize name 'face face)
                (propertize state 'face (if (equal state "off") 'shadow 'success))
                (gptel-fleet--fmt-backend (plist-get p :backend))
                (format "%s" (or (plist-get p :model) "·inherit"))
                (format "%d" (length (plist-get p :tools)))
                (let ((sk (gptel-fleet-agent-skills name)))
                  (if (eq sk t) "all" (format "%d" (length sk))))
                (format "%s" (or (gptel-fleet-config-get name :profile) "-"))
                (format "%d" (gethash name runs 0))
                (if (gptel-fleet-config name) "*" "")
                (gptel-fleet--one-line (or (plist-get p :description) "") 200)))))
     (sort (gptel-fleet--agent-names) #'string<))))

(defvar-keymap gptel-fleet-agents-mode-map
  :parent tabulated-list-mode-map
  "RET" #'gptel-fleet-agents-edit
  "e" #'gptel-fleet-agents-edit
  "s" #'gptel-fleet-agents-source
  "i" #'gptel-fleet-agents-mark-enable
  "d" #'gptel-fleet-agents-mark-disable
  "u" #'gptel-fleet-agents-unmark
  "x" #'gptel-fleet-agents-execute
  "D" #'gptel-fleet-agents-toggle-delegate
  "M" #'gptel-fleet-agents-set-model
  "t" #'gptel-fleet-assign-tools
  "k" #'gptel-fleet-assign-packages
  "K" #'gptel-fleet-assign-skills
  "p" #'gptel-fleet-agents-set-profile
  "P" #'gptel-fleet-agents-policy
  "L" #'gptel-fleet-agents-runs
  "n" #'gptel-fleet-new-agent
  "a" #'gptel-fleet-apply
  "T" #'gptel-fleet-list-tools
  "A" #'gptel-fleet-approvals
  "R" #'gptel-fleet-list-runs)

(define-derived-mode gptel-fleet-agents-mode tabulated-list-mode "Fleet-Agents"
  "List of gptel-agent agents.
Ovr * means an override file exists.
\\{gptel-fleet-agents-mode-map}"
  (setq tabulated-list-format
        [("Agent" 14 t) ("St" 5 t) ("Backend" 11 t) ("Model" 16 t)
         ("Tools" 5 (lambda (a b) (< (string-to-number (aref (cadr a) 4))
                                     (string-to-number (aref (cadr b) 4))))
          :right-align t)
         ("Skills" 6 nil :right-align t)
         ("Profile" 9 t) ("Runs" 4 nil :right-align t) ("Ovr" 3 t) ("Description" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Agent"))
  (add-hook 'tabulated-list-revert-hook
            (lambda () (setq tabulated-list-entries (gptel-fleet--agents-entries)))
            nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun gptel-fleet-list-agents ()
  "List all gptel-agent agents."
  (interactive)
  (let ((buf (get-buffer-create "*gptel-fleet agents*")))
    (with-current-buffer buf
      (gptel-fleet-agents-mode)
      (setq tabulated-list-entries (gptel-fleet--agents-entries))
      (tabulated-list-print))
    (pop-to-buffer buf)))

(defun gptel-fleet--agent-at-point ()
  "Agent name at point."
  (or (tabulated-list-get-id) (user-error "No agent on this line")))

(defun gptel-fleet-agents-edit ()
  "Edit the override of the agent at point."
  (interactive)
  (gptel-fleet-edit-agent (gptel-fleet--agent-at-point)))

(defun gptel-fleet-agents-source ()
  "Open the definition file of the agent at point."
  (interactive)
  (if-let* ((f (cdr (assoc (gptel-fleet--agent-at-point) gptel-fleet--agent-files))))
      (find-file f)
    (user-error "No definition file")))

(defun gptel-fleet-agents-mark-enable ()
  "Mark agent for enabling."
  (interactive) (tabulated-list-put-tag "I" t))

(defun gptel-fleet-agents-mark-disable ()
  "Mark agent for disabling."
  (interactive)
  (if (member (gptel-fleet--agent-at-point) gptel-fleet-main-agents)
      (user-error "Main agents cannot be disabled")
    (tabulated-list-put-tag "D" t)))

(defun gptel-fleet-agents-unmark ()
  "Remove mark."
  (interactive) (tabulated-list-put-tag " " t))

(defun gptel-fleet-agents-execute ()
  "Apply I/D marks, then re-register agents."
  (interactive)
  (let (changed)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((tag (char-after)) (id (tabulated-list-get-id)))
          (pcase tag
            (?I (gptel-fleet-config-set id :enabled t) (push id changed))
            (?D (gptel-fleet-config-set id :enabled nil) (push id changed))))
        (forward-line 1)))
    (if (not changed)
        (message "No marks")
      (gptel-fleet-apply)
      (message "Updated %s" (string-join changed ", ")))))

(defun gptel-fleet-agents-toggle-delegate ()
  "Toggle whether other agents may delegate to the agent at point."
  (interactive)
  (let ((a (gptel-fleet--agent-at-point)))
    (gptel-fleet-config-set a :delegate (not (gptel-fleet-delegate-p a)))
    (gptel-fleet-apply)))

(defun gptel-fleet--model-candidates ()
  "Alist \"Backend:model\" -> (BACKEND-NAME . MODEL)."
  (cl-loop for (bname . backend) in gptel--known-backends
           append (cl-loop for m in (gptel-backend-models backend)
                           for sym = (if (consp m) (car m) m)
                           collect (cons (format "%s:%s" bname sym) (cons bname sym)))))

(defun gptel-fleet-agents-set-model ()
  "Choose backend and model for the agent at point (\"inherit\" to unset)."
  (interactive)
  (let* ((a (gptel-fleet--agent-at-point))
         (cands (cons '("inherit" . nil) (gptel-fleet--model-candidates)))
         (choice (cdr (assoc (completing-read (format "Model for %s: " a) cands nil t) cands))))
    (if (null choice)
        (progn (gptel-fleet--config-unset a :backend) (gptel-fleet--config-unset a :model))
      (gptel-fleet-config-set a :backend (car choice))
      (gptel-fleet-config-set a :model (cdr choice)))
    (gptel-fleet-apply)))

(defun gptel-fleet-agents-set-tools ()
  "Choose the tool list of the agent at point (empty input: inherit)."
  (interactive)
  (let* ((a (gptel-fleet--agent-at-point))
         (cur (plist-get (gptel-fleet--agent-plist a) :tools))
         (sel (completing-read-multiple
               (format "Tools for %s (comma separated, empty = inherit): " a)
               (gptel-fleet-tool-names) nil t
               (string-join (cl-remove-if-not #'stringp cur) ","))))
    (if sel (gptel-fleet-config-set a :tools sel) (gptel-fleet--config-unset a :tools))
    (gptel-fleet-apply)))

(defun gptel-fleet-agents-set-packages ()
  "Choose tool packages added to the agent at point."
  (interactive)
  (let* ((a (gptel-fleet--agent-at-point))
         (sel (completing-read-multiple
               (format "Tool packages for %s: " a)
               (mapcar (lambda (p) (symbol-name (car p))) gptel-fleet--packages) nil t
               (mapconcat #'symbol-name (gptel-fleet-config-get a :tool-packages) ","))))
    (gptel-fleet-config-set a :tool-packages (mapcar #'intern sel))
    (gptel-fleet-apply)))

(defun gptel-fleet-agents-set-profile ()
  "Choose the policy profile of the agent at point."
  (interactive)
  (let* ((a (gptel-fleet--agent-at-point))
         (p (completing-read (format "Profile for %s: " a)
                             (cons "none" (mapcar (lambda (p) (symbol-name (car p)))
                                                  gptel-fleet-profiles))
                             nil t)))
    (if (equal p "none") (gptel-fleet--config-unset a :profile)
      (gptel-fleet-config-set a :profile (intern p)))
    (gptel-fleet-apply)))

(defun gptel-fleet-agents-policy ()
  "Show the policy matrix of the agent at point."
  (interactive)
  (gptel-fleet-policy-matrix (gptel-fleet--agent-at-point)))

(defun gptel-fleet-agents-runs ()
  "List runs of the agent at point."
  (interactive)
  (gptel-fleet-list-runs (gptel-fleet--agent-at-point)))

;;;; Tools

(defun gptel-fleet--tool-kind (name)
  "Kind of tool NAME."
  (let ((spec (gethash name gptel-fleet--tool-specs)))
    (cond ((and spec (plist-get spec :command)) "command")
          (spec "elisp")
          ((gethash name gptel-fleet--adopted) "adopted")
          (t "native"))))

(defun gptel-fleet--tools-entries ()
  "Entries for the tools list."
  (let ((usage (make-hash-table :test #'equal)))
    (dolist (a (bound-and-true-p gptel-agent--agents))
      (dolist (tn (plist-get (cdr a) :tools))
        (cl-incf (gethash tn usage 0))))
    (mapcar
     (lambda (tool)
       (let* ((name (gptel-tool-name tool))
              (d (gptel-fleet--tool-default-level name))
              (lvl (format "%s" (or (car d) gptel-fleet-default-level))))
         (list name
               (vector name
                       (format "%s" (or (gptel-fleet-tool-package name)
                                        (gptel-tool-category tool) "-"))
                       (gptel-fleet--tool-kind name)
                       (propertize lvl 'face (pcase lvl
                                               ("deny" 'error) ("escalate" 'warning)
                                               ("confirm" 'warning) ("allow" 'success)
                                               (_ 'default)))
                       (format "%d" (gethash name usage 0))
                       (gptel-fleet--one-line (gptel-tool-description tool) 200)))))
     (cl-remove-duplicates (gptel-fleet-all-tools) :key #'gptel-tool-name :test #'equal))))

(defvar-keymap gptel-fleet-tools-mode-map
  :parent tabulated-list-mode-map
  "RET" #'gptel-fleet-tools-describe
  "l" #'gptel-fleet-tools-set-level
  "f" #'gptel-fleet-tools-forge
  "t" #'gptel-fleet-tools-test
  "a" #'gptel-fleet-apply)

(define-derived-mode gptel-fleet-tools-mode tabulated-list-mode "Fleet-Tools"
  "All gptel tools with their package and default approval level.
Agents = number of agents whose tool list contains the tool.
\\{gptel-fleet-tools-mode-map}"
  (setq tabulated-list-format
        [("Tool" 22 t) ("Package" 18 t) ("Kind" 8 t) ("Default" 9 t)
         ("Agents" 6 nil :right-align t) ("Description" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Package"))
  (add-hook 'tabulated-list-revert-hook
            (lambda () (setq tabulated-list-entries (gptel-fleet--tools-entries))) nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun gptel-fleet-list-tools ()
  "List all tools."
  (interactive)
  (let ((buf (get-buffer-create "*gptel-fleet tools*")))
    (with-current-buffer buf
      (gptel-fleet-tools-mode)
      (setq tabulated-list-entries (gptel-fleet--tools-entries))
      (tabulated-list-print))
    (pop-to-buffer buf)))

(defun gptel-fleet-tools-describe ()
  "Show the definition of the tool at point."
  (interactive)
  (let* ((name (tabulated-list-get-id))
         (spec (gethash name gptel-fleet--tool-specs))
         (tool (gptel-fleet-find-tool name)))
    (if (and spec (plist-get spec :file))
        (progn (find-file (plist-get spec :file))
               (goto-char (point-min))
               (re-search-forward (format "(gptel-fleet-define-tool %s\\_>"
                                          (regexp-quote (symbol-name (plist-get spec :id))))
                                  nil t)
               (beginning-of-line))
      (with-help-window "*gptel-fleet tool*"
        (princ (format "%s  [%s, %s]\n\n%s\n\nArgs:\n%s"
                       name (gptel-fleet--tool-kind name) (gptel-tool-category tool)
                       (gptel-tool-description tool)
                       (pp-to-string (gptel-tool-args tool))))))))

(defun gptel-fleet-tools-set-level ()
  "Set the default level of the tool at point (saved in tool-levels.eld)."
  (interactive)
  (let* ((name (tabulated-list-get-id))
         (l (completing-read (format "Default level for %s: " name)
                             (cons "reset" (mapcar #'symbol-name gptel-fleet-levels)) nil t)))
    (gptel-fleet-set-tool-level name (unless (equal l "reset") (intern l)))
    (revert-buffer)))

(defun gptel-fleet-tools-forge ()
  "Open the tool at point in the forge."
  (interactive)
  (gptel-fleet-forge-edit-tool (tabulated-list-get-id)))

(defun gptel-fleet-tools-test ()
  "Test the tool at point in the forge bench."
  (interactive)
  (gptel-fleet-forge-test-tool (tabulated-list-get-id)))

;;;; Policy matrix

(defun gptel-fleet--arg-rules-for (agent tool-name)
  "Number of arg-dependent rules for AGENT that could apply to TOOL-NAME."
  (let ((rules (append gptel-fleet-floor-rules
                       (gptel-fleet-config-get agent :policy)
                       (cdr (assoc agent gptel-fleet--session-rules))
                       (cdr (assoc agent gptel-fleet--learned))
                       (cdr (assq (gptel-fleet-config-get agent :profile) gptel-fleet-profiles))
                       gptel-fleet-global-rules)))
    (cl-count-if (lambda (r)
                   (let ((m (cdr-safe r)))
                     (and (or (plist-get m :arg) (plist-get m :args) (plist-get m :pred))
                          (let ((tools (plist-get m :tool)) (pk (plist-get m :package)))
                            (and (or (null tools) (member tool-name (ensure-list tools)))
                                 (or (null pk) (memq (gptel-fleet-tool-package tool-name)
                                                     (ensure-list pk))))))))
                 rules)))

;;;###autoload
(defun gptel-fleet-policy-matrix (agent &optional all-tools)
  "Show AGENT's effective level for each of its tools.
With ALL-TOOLS (prefix arg) include tools the agent does not have.
\"Arg rules\" counts rules that depend on arguments: the level shown is
for a call that matches none of them."
  (interactive (list (completing-read "Agent: " (gptel-fleet--agent-names)) current-prefix-arg))
  (let* ((p (gptel-fleet--agent-plist agent))
         (names (if all-tools (gptel-fleet-tool-names)
                  (or (cl-remove-if-not #'stringp (plist-get p :tools))
                      (gptel-fleet-tool-names))))
         (main (member agent gptel-fleet-main-agents))
         (buf (get-buffer-create (format "*gptel-fleet policy: %s*" agent))))
    (with-current-buffer buf
      (tabulated-list-mode)
      (setq tabulated-list-format [("Tool" 22 t) ("Package" 18 t) ("Level" 9 t)
                                   ("Arg rules" 9 nil :right-align t) ("Decided by" 0 nil)])
      (setq tabulated-list-padding 2)
      (setq tabulated-list-entries
            (mapcar (lambda (n)
                      (let ((d (gptel-fleet-decide agent n nil main)))
                        (list n (vector n (format "%s" (or (gptel-fleet-tool-package n) "-"))
                                        (format "%s" (car d))
                                        (format "%d" (gptel-fleet--arg-rules-for agent n))
                                        (gptel-fleet-describe-source (cdr d))))))
                    names))
      (tabulated-list-init-header)
      (tabulated-list-print))
    (pop-to-buffer buf)))

;;;; Runs

(defvar-local gptel-fleet--runs-agent nil "Agent filter of the runs list.")

(defun gptel-fleet--runs-entries ()
  "Entries for the runs list."
  (let ((idx (gptel-fleet-log-index)))
    (cl-loop for e in (reverse idx)
             for agent = (alist-get 'agent e)
             when (or (null gptel-fleet--runs-agent) (equal agent gptel-fleet--runs-agent))
             collect (list (alist-get 'run e)
                           (vector (substring (or (alist-get 'ts e) "") 0
                                              (min 19 (length (or (alist-get 'ts e) ""))))
                                   (or agent "?")
                                   (alist-get 'run e)
                                   (or (alist-get 'parent e) "")
                                   (gptel-fleet--one-line
                                    (or (alist-get 'description e) (alist-get 'buffer e) "")
                                    200))))))

(defvar-keymap gptel-fleet-runs-mode-map
  :parent tabulated-list-mode-map
  "RET" #'gptel-fleet-runs-show
  "T" #'gptel-fleet-runs-show-tree
  "f" #'gptel-fleet-runs-open-file)

(define-derived-mode gptel-fleet-runs-mode tabulated-list-mode "Fleet-Runs"
  "Logged agent runs.
\\{gptel-fleet-runs-mode-map}"
  (setq tabulated-list-format [("Started" 20 t) ("Agent" 16 t) ("Run" 26 t)
                               ("Parent" 26 t) ("Description" 0 nil)])
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook
            (lambda () (setq tabulated-list-entries (gptel-fleet--runs-entries))) nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun gptel-fleet-list-runs (&optional agent)
  "List logged runs, optionally only those of AGENT."
  (interactive)
  (let ((buf (get-buffer-create (if agent (format "*gptel-fleet runs: %s*" agent)
                                  "*gptel-fleet runs*"))))
    (with-current-buffer buf
      (gptel-fleet-runs-mode)
      (setq gptel-fleet--runs-agent agent)
      (setq tabulated-list-entries (gptel-fleet--runs-entries))
      (tabulated-list-print))
    (pop-to-buffer buf)))

(defun gptel-fleet-runs-show ()
  "Reconstruct the run at point with its subagents."
  (interactive)
  (gptel-fleet-log-reconstruct (tabulated-list-get-id)))

(defun gptel-fleet-runs-show-tree ()
  "Reconstruct the whole tree containing the run at point."
  (interactive)
  (gptel-fleet-log-reconstruct (tabulated-list-get-id) t))

(defun gptel-fleet-runs-open-file ()
  "Open the raw JSONL of the run at point."
  (interactive)
  (let* ((run (tabulated-list-get-id))
         (e (cl-find run (gptel-fleet-log-index) :key (lambda (e) (alist-get 'run e))
                     :test #'equal)))
    (find-file (alist-get 'file e))))

;;;; Assignment checklists (tools, packages, skills)

;; One buffer per agent and kind, listing every candidate with a mark
;; column.  Nothing is written until you save, so you can toggle freely.
;;
;;   m / u / SPC  mark / unmark / toggle      * / U  mark / unmark all
;;   C-c C-c, x   save and re-register         i      inherit (remove key)
;;   q            quit without saving

(defvar-local gptel-fleet--assign-agent nil)
(defvar-local gptel-fleet--assign-kind nil "tools, packages or skills.")
(defvar-local gptel-fleet--assign-selected nil "Hash of selected ids.")
(defvar-local gptel-fleet--assign-initial nil "Ids selected when the checklist opened.")
(defvar-local gptel-fleet--assign-patterns nil "Skill globs kept as they are.")

(defun gptel-fleet--assign-candidates (kind)
  "List of (ID GROUP DESCRIPTION) for KIND."
  (pcase kind
    ('tools
     (mapcar (lambda (tool)
               (let ((n (gptel-tool-name tool)))
                 (list n (format "%s" (or (gptel-fleet-tool-package n)
                                          (gptel-tool-category tool) "-"))
                       (gptel-tool-description tool))))
             (cl-remove-duplicates (gptel-fleet-all-tools)
                                   :key #'gptel-tool-name :test #'equal)))
    ('packages
     (mapcar (lambda (p)
               (list (symbol-name (car p))
                     (format "%s, %d tools" (or (plist-get (cdr p) :origin) "-")
                             (length (plist-get (cdr p) :tools)))
                     (concat (or (plist-get (cdr p) :description) "")
                             "  [" (string-join (plist-get (cdr p) :tools) " ") "]")))
             gptel-fleet--packages))
    ('skills
     (mapcar (lambda (e)
               (list (car e)
                     (abbreviate-file-name (directory-file-name (cadr e)))
                     (plist-get (cddr e) :description)))
             (bound-and-true-p gptel-agent--skills)))))

(defun gptel-fleet--assign-initial (agent kind)
  "Initially selected ids of KIND for AGENT."
  (pcase kind
    ('tools
     (cl-loop for n in (plist-get (gptel-fleet--agent-plist agent) :tools)
              for tool = (ignore-errors (gptel-get-tool n))
              append (mapcar #'gptel-tool-name (ensure-list tool))))
    ('packages (mapcar #'symbol-name (gptel-fleet-config-get agent :tool-packages)))
    ('skills (let ((s (gptel-fleet-agent-skills agent)))
               (if (eq s t) (gptel-fleet-skill-names) s)))))

(defun gptel-fleet--assign-key (kind)
  "Override key for KIND."
  (pcase kind ('tools :tools) ('packages :tool-packages) ('skills :skills)))

(defun gptel-fleet--assign-entries ()
  "Entries for the checklist."
  (mapcar (lambda (c)
            (let ((on (gethash (car c) gptel-fleet--assign-selected)))
              (list (car c)
                    (vector (if on "[x]" "[ ]")
                            (propertize (car c) 'face (if on 'bold 'default))
                            (cadr c)
                            (gptel-fleet--one-line (or (nth 2 c) "") 200)))))
          (gptel-fleet--assign-candidates gptel-fleet--assign-kind)))

(defun gptel-fleet--assign-header ()
  "Header line text."
  (let ((inherit (not (plist-member (gptel-fleet-config gptel-fleet--assign-agent)
                                    (gptel-fleet--assign-key gptel-fleet--assign-kind)))))
    (format " %s · %s · %d selected · %s%s   C-c C-c save  i inherit  q quit"
            gptel-fleet--assign-agent gptel-fleet--assign-kind
            (hash-table-count gptel-fleet--assign-selected)
            (if inherit "currently INHERITED" "explicit")
            (if gptel-fleet--assign-patterns
                (format " · globs kept: %s" (string-join gptel-fleet--assign-patterns " "))
              ""))))

(defvar-keymap gptel-fleet-assign-mode-map
  :parent tabulated-list-mode-map
  "m" #'gptel-fleet-assign-mark
  "u" #'gptel-fleet-assign-unmark
  "SPC" #'gptel-fleet-assign-toggle
  "*" #'gptel-fleet-assign-mark-all
  "U" #'gptel-fleet-assign-unmark-all
  "i" #'gptel-fleet-assign-inherit
  "+" #'gptel-fleet-assign-register-skill
  "x" #'gptel-fleet-assign-save
  "C-c C-c" #'gptel-fleet-assign-save
  "q" #'quit-window)

(define-derived-mode gptel-fleet-assign-mode tabulated-list-mode "Fleet-Assign"
  "Choose the tools, packages or skills of one agent.
\\{gptel-fleet-assign-mode-map}"
  (setq tabulated-list-format [("" 3 t) ("Name" 26 t) ("Group" 22 t) ("Description" 0 nil)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key '("Group"))
  (setq header-line-format '(:eval (gptel-fleet--assign-header)))
  (add-hook 'tabulated-list-revert-hook
            (lambda () (setq tabulated-list-entries (gptel-fleet--assign-entries))) nil t)
  (tabulated-list-init-header)
  (setq header-line-format '(:eval (gptel-fleet--assign-header))))

(defun gptel-fleet-assign (agent kind)
  "Open the KIND checklist (tools, packages, skills) for AGENT."
  (let ((buf (get-buffer-create (format "*gptel-fleet %s: %s*" kind agent)))
        (initial (gptel-fleet--assign-initial agent kind)))
    (with-current-buffer buf
      (gptel-fleet-assign-mode)
      (setq gptel-fleet--assign-agent agent
            gptel-fleet--assign-kind kind
            gptel-fleet--assign-selected (make-hash-table :test #'equal)
            gptel-fleet--assign-patterns
            (and (eq kind 'skills)
                 (cl-remove-if-not #'gptel-fleet--skill-pattern-p
                                   (gptel-fleet-config-get agent :skills))))
      (setq gptel-fleet--assign-initial initial)
      (dolist (id initial) (puthash id t gptel-fleet--assign-selected))
      (setq tabulated-list-entries (gptel-fleet--assign-entries))
      (tabulated-list-print))
    (pop-to-buffer buf)
    buf))

(defun gptel-fleet--assign-set (state &optional all)
  "Set the line's (or with ALL every line's) selection to STATE (t, nil, toggle)."
  (let ((ids (if all (mapcar #'car tabulated-list-entries)
               (list (or (tabulated-list-get-id) (user-error "No item here"))))))
    (dolist (id ids)
      (if (if (eq state 'toggle) (not (gethash id gptel-fleet--assign-selected)) state)
          (puthash id t gptel-fleet--assign-selected)
        (remhash id gptel-fleet--assign-selected))))
  (let ((line (line-number-at-pos)))
    (setq tabulated-list-entries (gptel-fleet--assign-entries))
    (tabulated-list-print t)
    (goto-char (point-min))
    (forward-line (1- line))
    (unless all (forward-line 1)))
  (force-mode-line-update))

(defun gptel-fleet-assign-mark () "Select." (interactive) (gptel-fleet--assign-set t))
(defun gptel-fleet-assign-unmark () "Deselect." (interactive) (gptel-fleet--assign-set nil))
(defun gptel-fleet-assign-toggle () "Toggle." (interactive) (gptel-fleet--assign-set 'toggle))
(defun gptel-fleet-assign-mark-all () "Select all." (interactive) (gptel-fleet--assign-set t t))
(defun gptel-fleet-assign-unmark-all () "Select none." (interactive) (gptel-fleet--assign-set nil t))

(defun gptel-fleet--assign-value ()
  "Value to store for the current selection."
  (let ((sel (let ((kept (cl-remove-if-not
                           (lambda (id) (gethash id gptel-fleet--assign-selected))
                           gptel-fleet--assign-initial)))
               ;; Keep the existing order; newly marked items go at the end.
               (append kept
                       (cl-loop for (id) in (gptel-fleet--assign-candidates
                                             gptel-fleet--assign-kind)
                                when (and (gethash id gptel-fleet--assign-selected)
                                          (not (member id kept)))
                                collect id)))))
    (pcase gptel-fleet--assign-kind
      ('packages (mapcar #'intern sel))
      ('skills
       (let* ((pats gptel-fleet--assign-patterns)
              (covered-off (cl-remove-if
                            (lambda (n) (or (member n sel)
                                            (not (gptel-fleet-skill-matches-p n pats))))
                            (gptel-fleet-skill-names))))
         (when covered-off
           (display-warning
            'gptel-fleet
            (format "%s stay visible: they match the glob(s) %s. Edit :skills in the .eld to change."
                    (string-join covered-off ", ") (string-join pats " "))))
         (append pats (cl-remove-if (lambda (n) (gptel-fleet-skill-matches-p n pats)) sel))))
      (_ sel))))

(defun gptel-fleet-assign-save ()
  "Save the selection into the agent's override and re-register."
  (interactive)
  (let ((agent gptel-fleet--assign-agent)
        (key (gptel-fleet--assign-key gptel-fleet--assign-kind))
        (val (gptel-fleet--assign-value)))
    (gptel-fleet-config-set agent key val)
    (gptel-fleet-apply)
    (message "%s %s: %d saved" agent key (length val))
    (quit-window)))

(defun gptel-fleet-assign-inherit ()
  "Remove the key from the override, so the agent inherits it again."
  (interactive)
  (when (y-or-n-p (format "Let %s inherit %s again? " gptel-fleet--assign-agent
                          gptel-fleet--assign-kind))
    (gptel-fleet--config-unset gptel-fleet--assign-agent
                               (gptel-fleet--assign-key gptel-fleet--assign-kind))
    (gptel-fleet-apply)
    (quit-window)))

(defun gptel-fleet--agent-arg ()
  "Agent at point in the agent list, otherwise read one."
  (or (and (derived-mode-p 'gptel-fleet-agents-mode) (tabulated-list-get-id))
      (completing-read "Agent: " (gptel-fleet--agent-names) nil t)))

(defun gptel-fleet-assign-register-skill ()
  "Register a new skill and mark it in this skills checklist."
  (interactive)
  (unless (eq gptel-fleet--assign-kind 'skills)
    (user-error "Only in the skills checklist"))
  (let* ((path (read-file-name "New skill (directory or SKILL.md): " nil nil t))
         (name (car (gptel-fleet-validate-skill path))))
    (gptel-fleet-register-skill path)
    (when name (puthash name t gptel-fleet--assign-selected))
    (setq tabulated-list-entries (gptel-fleet--assign-entries))
    (tabulated-list-print t)
    (message "Skill %s registered and marked; C-c C-c saves the assignment" name)))

;;;###autoload
(defun gptel-fleet-assign-tools (&optional agent)
  "Choose AGENT's tools in a checklist (default: agent at point)."
  (interactive)
  (gptel-fleet-assign (or agent (gptel-fleet--agent-arg)) 'tools))

;;;###autoload
(defun gptel-fleet-assign-packages (&optional agent)
  "Choose AGENT's tool packages in a checklist."
  (interactive)
  (gptel-fleet-assign (or agent (gptel-fleet--agent-arg)) 'packages))

;;;###autoload
(defun gptel-fleet-assign-skills (&optional agent)
  "Choose AGENT's visible skills in a checklist."
  (interactive)
  (gptel-fleet-assign (or agent (gptel-fleet--agent-arg)) 'skills))

(provide 'gptel-fleet-list)
;;; gptel-fleet-list.el ends here
