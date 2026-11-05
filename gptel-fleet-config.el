;;; gptel-fleet-config.el --- Edit agent overrides as validated lisp-data  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Georg Alberding
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `gptel-fleet-edit-agent' opens <fleet>/agents/<name>.eld in
;; `gptel-fleet-config-mode': plain lisp-data with a generated reference
;; header (frontmatter values, available backends/models, packages,
;; profiles, levels).  Saving validates the plist; errors block the save
;; (see `gptel-fleet-config-block-invalid'), warnings are shown.  A valid
;; save re-registers agents with gptel-agent and live main buffers.
;;
;;   C-c C-c  save, validate, apply, close
;;   C-c C-v  validate only
;;   C-c C-p  effective-policy matrix for this agent (unsaved edits included)
;;   C-c C-s  open the agent's definition file (.md/.org)
;;   C-M-i    complete tool names, packages, levels, profiles, keys

;;; Code:

(require 'gptel-fleet-core)
(require 'gptel-fleet-policy)
(require 'gptel-fleet-skills)
(require 'gptel-fleet-models)

(declare-function gptel-fleet-apply "gptel-fleet")
(declare-function gptel-fleet-policy-matrix "gptel-fleet-list")
(defvar gptel-fleet--agent-files)
(defvar gptel-fleet--frontmatter)
(defvar gptel-fleet--preset-keys)
(defvar gptel-agent-dirs)

(defcustom gptel-fleet-config-block-invalid t
  "If non-nil, refuse to save an override file that has errors."
  :group 'gptel-fleet
  :type 'boolean)

(defcustom gptel-fleet-agent-source-directory
  (expand-file-name "agent-defs" gptel-fleet-directory)
  "Where `gptel-fleet-new-agent' writes new agent definitions.
Added to `gptel-agent-dirs' by `gptel-fleet-mode'."
  :group 'gptel-fleet
  :type 'directory)

(defvar-local gptel-fleet-config--agent nil "Agent edited in this buffer.")

;;;; Validation

(defun gptel-fleet--regexp-ok-p (re)
  "Non-nil if RE is a valid regexp."
  (and (stringp re) (condition-case nil (progn (string-match-p re "") t) (error nil))))

(defun gptel-fleet-validate-rule (rule)
  "List of problems in RULE."
  (let (errs)
    (cond
     ((not (consp rule)) (push (format "rule is not a list: %S" rule) errs))
     ((not (memq (car rule) gptel-fleet-levels))
      (push (format "unknown level %S in %S (one of %S)" (car rule) rule gptel-fleet-levels) errs))
     ((not (plistp (cdr rule))) (push (format "matcher is not a plist: %S" rule) errs))
     (t
      (cl-loop for (k v) on (cdr rule) by #'cddr do
               (pcase k
                 (:tool (dolist (n (ensure-list v))
                          (unless (gptel-fleet-find-tool n)
                            (push (format "warning: unknown tool %S" n) errs))))
                 (:package (dolist (p (ensure-list v))
                             (unless (gptel-fleet-package p)
                               (push (format "warning: unknown package %S" p) errs))))
                 (:agent nil)
                 (:arg (unless (or (keywordp v) (stringp v))
                         (push (format ":arg must be a keyword like :command: %S" v) errs)))
                 ((or :match :any-segment :all-segments)
                  (unless (gptel-fleet--regexp-ok-p v)
                    (push (format "invalid regexp %S" v) errs)))
                 ((or :under :not-under)
                  (unless (stringp v) (push (format "%s needs a directory string" k) errs)))
                 (:equal nil)
                 (:args (dolist (a v)
                          (unless (and (consp a) (keywordp (car a)) (plistp (cdr a)))
                            (push (format "bad :args entry %S" a) errs))
                          (dolist (re (list (plist-get (cdr a) :match)
                                            (plist-get (cdr a) :any-segment)
                                            (plist-get (cdr a) :all-segments)))
                            (when (and re (not (gptel-fleet--regexp-ok-p re)))
                              (push (format "invalid regexp %S" re) errs)))))
                 (:pred (unless (or (functionp v) (symbolp v))
                          (push (format ":pred must be a function: %S" v) errs)))
                 (_ (push (format "unknown matcher key %S in %S" k rule) errs))))))
    (nreverse errs)))

(defun gptel-fleet-validate-config (plist)
  "Return a list of problem strings for override PLIST.
Strings starting with \"warning:\" do not block saving."
  (let (errs)
    (cl-flet ((err (fmt &rest args) (push (apply #'format fmt args) errs)))
      (if (not (plistp plist))
          (err "top level must be a plist, got %S" (type-of plist))
        (cl-loop
         for (k v) on plist by #'cddr do
         (pcase k
           ((or :enabled :delegate :log)
            (unless (memq v '(t nil)) (err "%s must be t or nil" k)))
           (:backend
            (cond ((not (stringp v)) (err ":backend must be a backend name string"))
                  ((not (ignore-errors (gptel-get-backend v)))
                   (err ":backend %S is not defined (known: %s)" v
                        (string-join (gptel-fleet--backend-names) ", ")))))
           (:model
            (if (not (symbolp v))
                (err ":model must be a symbol")
              (when-let* ((b (ignore-errors (gptel-get-backend (plist-get plist :backend))))
                          (models (gptel-backend-models b)))
                (unless (memq v (mapcar (lambda (m) (if (consp m) (car m) m)) models))
                  (err "warning: model %s not listed for backend %s" v (gptel-backend-name b))))))
           (:models
            (dolist (p (gptel-fleet-validate-models-list v)) (err "%s" p)))
           (:tools
            (if (not (and (listp v) (cl-every #'stringp v)))
                (err ":tools must be a list of tool name strings")
              (dolist (n v)
                (unless (gptel-fleet-find-tool n) (err "unknown tool %S" n)))))
           (:tool-packages
            (dolist (p (ensure-list v))
              (unless (gptel-fleet-package p) (err "unknown tool package %S" p))))
           (:skills
            (if (not (and (listp v) (cl-every #'stringp v)))
                (err ":skills must be a list of skill names or globs (strings)")
              (dolist (s v)
                (unless (or (gptel-fleet--skill-pattern-p s)
                            (member s (gptel-fleet-skill-names)))
                  (err "warning: unknown skill %S (known: %s)" s
                       (string-join (gptel-fleet-skill-names) ", "))))))
           (:profile
            (unless (or (null v) (assq v gptel-fleet-profiles))
              (err "unknown profile %S (known: %S)" v (mapcar #'car gptel-fleet-profiles))))
           (:policy
            (if (not (listp v)) (err ":policy must be a list of rules")
              (dolist (r v) (setq errs (append (reverse (gptel-fleet-validate-rule r)) errs)))))
           (:log-dir (unless (or (null v) (stringp v)) (err ":log-dir must be a string")))
           (:preset
            (if (not (plistp v)) (err ":preset must be a plist")
              (cl-loop for (pk _) on v by #'cddr
                       unless (memq pk gptel-fleet--preset-keys)
                       do (err "warning: preset key %s is unknown to gptel" pk))))
           (:notes nil)
           (_ (err "unknown key %S (allowed: %S)" k gptel-fleet-override-keys))))))
    (nreverse errs)))

(defun gptel-fleet--backend-names ()
  "Names of defined gptel backends."
  (mapcar #'car gptel--known-backends))

;;;; Mode

(defvar-keymap gptel-fleet-config-mode-map
  "C-c C-c" #'gptel-fleet-config-commit
  "C-c C-v" #'gptel-fleet-config-validate
  "C-c C-p" #'gptel-fleet-config-policy-preview
  "C-c C-s" #'gptel-fleet-config-visit-source
  "C-c C-k" #'kill-current-buffer)

(define-derived-mode gptel-fleet-config-mode lisp-data-mode "Fleet-Config"
  "Edit a gptel-fleet agent override.
\\{gptel-fleet-config-mode-map}"
  (add-hook 'write-contents-functions #'gptel-fleet-config--before-save nil t)
  (add-hook 'after-save-hook #'gptel-fleet-config--after-save nil t)
  (add-hook 'completion-at-point-functions #'gptel-fleet-config-capf nil t))

(defun gptel-fleet-config--read-buffer ()
  "Read the plist in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (condition-case err (read (current-buffer))
      (end-of-file nil)
      (error (user-error "Syntax error: %s" (error-message-string err))))))

(defun gptel-fleet-config--report (problems)
  "Show PROBLEMS; return non-nil if any is an error."
  (let ((errors (cl-remove-if (lambda (p) (string-prefix-p "warning:" p)) problems)))
    (if (null problems)
        (progn (message "gptel-fleet: config OK") nil)
      (with-current-buffer (get-buffer-create "*gptel-fleet validation*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "%d error(s), %d warning(s)\n\n" (length errors)
                          (- (length problems) (length errors))))
          (dolist (p problems) (insert "• " p "\n"))
          (special-mode))
        (display-buffer (current-buffer)))
      errors)))

(defun gptel-fleet-config-validate ()
  "Validate the buffer; return non-nil if it has errors."
  (interactive)
  (gptel-fleet-config--report (gptel-fleet-validate-config (gptel-fleet-config--read-buffer))))

(defun gptel-fleet-config--before-save ()
  "Block saving invalid configs.
Runs from `write-contents-functions': errors in `before-save-hook' are
demoted to messages since Emacs 29, which would let invalid files through.
Returns nil so the normal write proceeds."
  (when (and (gptel-fleet-config-validate) gptel-fleet-config-block-invalid)
    (user-error "gptel-fleet: not saved, fix the errors (or set `gptel-fleet-config-block-invalid')"))
  nil)

(defun gptel-fleet-config--after-save ()
  "Re-register agents after a successful save."
  (when (fboundp 'gptel-fleet-apply) (gptel-fleet-apply)))

(defun gptel-fleet-config-commit ()
  "Save, apply and close."
  (interactive)
  (save-buffer)
  (quit-window t))

(defun gptel-fleet-config-policy-preview ()
  "Show the effective policy for the edited agent, using unsaved edits."
  (interactive)
  (let* ((agent gptel-fleet-config--agent)
         (cfg (gptel-fleet-config--read-buffer))
         (gptel-fleet--overrides (cons (cons agent cfg)
                                       (assoc-delete-all agent (copy-alist gptel-fleet--overrides)))))
    (gptel-fleet-policy-matrix agent)))

(defun gptel-fleet-config-visit-source ()
  "Open the agent's definition file."
  (interactive)
  (if-let* ((f (cdr (assoc gptel-fleet-config--agent gptel-fleet--agent-files))))
      (find-file f)
    (user-error "No definition file known for %s" gptel-fleet-config--agent)))

(defun gptel-fleet-config-capf ()
  "Complete tool names inside strings, symbols elsewhere."
  (let* ((ppss (syntax-ppss))
         (in-string (nth 3 ppss)))
    (if in-string
        (let ((beg (1+ (nth 8 ppss)))
              (end (save-excursion (skip-chars-forward "^\"\n") (point))))
          (list beg end (append (gptel-fleet-tool-names) (gptel-fleet-skill-names)
                                (gptel-fleet--backend-names))
                :exclusive 'no))
      (when-let* ((bounds (bounds-of-thing-at-point 'symbol)))
        (list (car bounds) (cdr bounds)
              (mapcar #'symbol-name
                      (append gptel-fleet-override-keys gptel-fleet-levels
                              (mapcar #'car gptel-fleet--packages)
                              (mapcar #'car gptel-fleet-profiles)
                              '(:tool :package :agent :arg :args :match :equal :under
                                      :not-under :any-segment :all-segments :pred)))
              :exclusive 'no)))))

;;;; Opening

(defun gptel-fleet--config-header (agent)
  "Reference comment block for AGENT."
  (let* ((fm (cdr (assoc agent gptel-fleet--frontmatter)))
         (src (cdr (assoc agent gptel-fleet--agent-files))))
    (string-join
     (list
      (format "gptel-fleet override for agent \"%s\"" agent)
      (format "Definition: %s" (or src "?"))
      (format "Frontmatter tools: %S" (plist-get fm :tools))
      (format "Frontmatter backend/model: %s / %s"
              (or (plist-get fm :backend) "inherit") (or (plist-get fm :model) "inherit"))
      ""
      (format "Backends: %s" (string-join (gptel-fleet--backend-names) ", "))
      (format "Packages: %s" (mapconcat (lambda (p) (symbol-name (car p))) gptel-fleet--packages " "))
      (format "Skills: %s" (string-join (gptel-fleet-skill-names) " "))
      (format "Profiles: %s" (mapconcat (lambda (p) (symbol-name (car p))) gptel-fleet-profiles " "))
      (format "Levels (strict→loose): %s" (mapconcat #'symbol-name gptel-fleet-levels " "))
      ""
      "Keys: :enabled :delegate :backend :model :tools :tool-packages :skills :profile"
      "      :policy :log :log-dir :preset :notes"
      "Rule: (LEVEL :tool \"Bash\" :arg :command :any-segment \"^git push\")"
      "      (LEVEL :package fs-write :args ((:path :not-under \"~/proj\")))"
      "Omit a key to inherit it.  C-c C-c save+apply, C-c C-p policy matrix.")
     "\n")))

(defun gptel-fleet--config-template (agent)
  "Initial override text for AGENT."
  (concat
   (mapconcat (lambda (l) (concat ";; " l))
              (split-string (gptel-fleet--config-header agent) "\n") "\n")
   "\n(:enabled t\n"
   " ;; :delegate t\n"
   " ;; :backend \"llama-server\"\n"
   " ;; :model qwen3-coder-30b\n"
   " ;; :tools (\"Read\" \"Grep\" \"Glob\")\n"
   " ;; :tool-packages (fs-read)\n"
   " ;; :skills (\"pdf\" \"gitlab-*\")   ; () = none, omit = all\n"
   " ;; :profile readonly\n"
   " :policy ()\n"
   " :log t)\n"))

;;;###autoload
(defun gptel-fleet-edit-agent (agent)
  "Edit the override file of AGENT."
  (interactive
   (list (completing-read "Agent: " (mapcar #'car (bound-and-true-p gptel-agent--agents)))))
  (let ((file (gptel-fleet-override-file agent)))
    (unless (file-exists-p file)
      (make-directory (file-name-directory file) t)
      (with-temp-file file
        (insert ";; -*- mode: lisp-data; coding: utf-8 -*-\n"
                (gptel-fleet--config-template agent))))
    (find-file file)
    (gptel-fleet-config-mode)
    (setq gptel-fleet-config--agent agent)))

;;;###autoload
(defun gptel-fleet-edit-learned ()
  "Edit persisted learned rules."
  (interactive)
  (let ((file (gptel-fleet-learned-file)))
    (unless (file-exists-p file)
      (gptel-fleet--write-eld file nil "Learned approval rules: ((AGENT . RULES) ...)"))
    (find-file file)
    (lisp-data-mode)
    (add-hook 'after-save-hook #'gptel-fleet-load-learned nil t)))

;;;###autoload
(defun gptel-fleet-new-agent (name description)
  "Create a new agent definition NAME with DESCRIPTION, then edit it."
  (interactive "sAgent name: \nsOne-line description: ")
  (let* ((dir (file-name-as-directory gptel-fleet-agent-source-directory))
         (file (expand-file-name (concat (gptel-fleet--safe-name name) ".md") dir)))
    (when (file-exists-p file) (user-error "%s exists" file))
    (make-directory dir t)
    (with-temp-file file
      (insert "---\n"
              "name: " name "\n"
              "description: >\n  " description "\n"
              "tools:\n  - Read\n  - Grep\n  - Glob\n"
              "---\n"
              "You are the " name " agent.  " description "\n\n"
              "Work only on the task you are given.  Finish with a concise report:\n"
              "what you did, what you found, and anything you could not do.\n"))
    (add-to-list 'gptel-agent-dirs dir)
    (find-file file)
    (message "Edit the prompt, save, then `gptel-fleet-apply' to register %s" name)))

(provide 'gptel-fleet-config)
;;; gptel-fleet-config.el ends here
