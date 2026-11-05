;;; gptel-fleet-core.el --- Shared state for gptel-fleet  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Georg Alberding
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Customization, paths, the per-agent override registry and the small
;; utilities every other gptel-fleet module uses.
;;
;; Agent overrides live in one lisp-data file per agent:
;;
;;   <gptel-fleet-directory>/agents/<agent-name>.eld
;;
;; Each file holds a single plist.  Keys gptel-fleet understands:
;;
;;   :enabled       t/nil   Hide the agent from gptel-agent entirely.
;;   :delegate      t/nil   Offer it as a subagent_type to other agents.
;;   :backend       STRING  gptel backend name (`gptel-get-backend').
;;   :model         SYMBOL  model of that backend.
;;   :tools         LIST    Replace the frontmatter tool list (names).
;;   :tool-packages LIST    Add every tool of these packages (symbols).
;;   :skills        LIST    Visible skills (names or globs); () = none,
;;                          absent = all (see gptel-fleet-skills.el).
;;   :profile       SYMBOL  Policy profile from `gptel-fleet-profiles'.
;;   :policy        LIST    Ordered approval rules, first match wins.
;;   :log           t/nil   Write JSONL logs for this agent.
;;   :log-dir       STRING  Dedicated log directory (default logs/<agent>/).
;;   :preset        PLIST   Extra raw gptel preset keys (:temperature ...).
;;   :notes         STRING  Free text, for you.
;;
;; Only gptel-native keys are ever merged into `gptel-agent--agents', so
;; gptel's preset machinery never sees (and never warns about) fleet keys.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gptel)

(defgroup gptel-fleet nil
  "Manage gptel-agent agents, tools and approvals."
  :group 'gptel
  :prefix "gptel-fleet-")

(defcustom gptel-fleet-directory
  (file-name-as-directory (locate-user-emacs-file "gptel-fleet"))
  "Root directory for agent overrides, tool packages, learned rules and logs."
  :type 'directory)

(defcustom gptel-fleet-tool-directories nil
  "Extra directories with tool package files (*.el).
The directory \"tools/\" under `gptel-fleet-directory' is always used and
is where the forge writes new packages."
  :type '(repeat directory))

(defcustom gptel-fleet-log-directory nil
  "Root of all logs.  nil means \"logs/\" under `gptel-fleet-directory'."
  :type '(choice (const :tag "Default" nil) directory))

(defcustom gptel-fleet-main-agents '("gptel-agent" "gptel-plan")
  "Agents that act as top-level (main) agents rather than subagents."
  :type '(repeat string))

(defconst gptel-fleet-override-keys
  '(:enabled :delegate :backend :model :tools :tool-packages :skills :profile
    :policy :log :log-dir :preset :notes)
  "Keys allowed in an agent override file.")

;;;; Paths

(defun gptel-fleet--dir (&rest parts)
  "Return directory PARTS below `gptel-fleet-directory', creating it."
  (let ((dir (file-name-as-directory
              (expand-file-name (string-join parts "/") gptel-fleet-directory))))
    (make-directory dir t)
    dir))

(defun gptel-fleet-agents-dir () "Override directory." (gptel-fleet--dir "agents"))
(defun gptel-fleet-tools-dir () "Forge output directory." (gptel-fleet--dir "tools"))

(defun gptel-fleet-log-root ()
  "Root directory of all logs."
  (let ((dir (file-name-as-directory
              (or gptel-fleet-log-directory
                  (expand-file-name "logs" gptel-fleet-directory)))))
    (make-directory dir t)
    dir))

(defun gptel-fleet-learned-file ()
  "File holding learned (\"always allow\") approval rules."
  (expand-file-name "learned.eld" gptel-fleet-directory))

(defun gptel-fleet-override-file (agent)
  "Override file for AGENT (a string)."
  (expand-file-name (concat (gptel-fleet--safe-name agent) ".eld")
                    (gptel-fleet-agents-dir)))

(defun gptel-fleet--safe-name (s)
  "Return S usable as a file name component."
  (replace-regexp-in-string "[^[:alnum:]_.-]" "_" (format "%s" s)))

;;;; lisp-data IO

(defun gptel-fleet--read-eld (file &optional default)
  "Read the first sexp of FILE, or DEFAULT if FILE is missing or empty."
  (if (not (file-readable-p file))
      default
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (read (current-buffer))
        (end-of-file default)))))

(defun gptel-fleet--write-eld (file data &optional header)
  "Pretty-print DATA into FILE, preceded by an optional comment HEADER."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (insert ";; -*- mode: lisp-data; coding: utf-8 -*-\n")
    (when header
      (insert (mapconcat (lambda (l) (concat ";; " l))
                         (split-string header "\n") "\n")
              "\n"))
    (let ((print-length nil) (print-level nil))
      (pp data (current-buffer)))))

;;;; Override registry

(defvar gptel-fleet--overrides nil
  "Alist of (AGENT-NAME . PLIST) read from the override directory.")

(defun gptel-fleet-load-overrides ()
  "Reload `gptel-fleet--overrides' from disk.  Return it."
  (setq gptel-fleet--overrides
        (cl-loop for file in (directory-files (gptel-fleet-agents-dir) t "\\.eld\\'")
                 for data = (condition-case err (gptel-fleet--read-eld file)
                              (error (display-warning
                                      'gptel-fleet
                                      (format "Cannot read %s: %S" file err))
                                     nil))
                 when (plistp data)
                 collect (cons (file-name-base file) data))))

(defun gptel-fleet-config (agent)
  "Return the override plist for AGENT (may be nil)."
  (cdr (assoc agent gptel-fleet--overrides)))

(defun gptel-fleet-config-get (agent key &optional default)
  "Return KEY of AGENT's override, DEFAULT when the key is absent."
  (let ((cfg (gptel-fleet-config agent)))
    (if (plist-member cfg key) (plist-get cfg key) default)))

(defun gptel-fleet-config-set (agent key value)
  "Set KEY to VALUE in AGENT's override and save it to disk."
  (let ((cfg (copy-sequence (gptel-fleet-config agent))))
    (setq cfg (plist-put cfg key value))
    (setf (alist-get agent gptel-fleet--overrides nil nil #'equal) cfg)
    (let* ((file (gptel-fleet-override-file agent))
           (header (gptel-fleet--leading-comments file)))
      (gptel-fleet--write-eld file cfg
                              (or header (format "gptel-fleet override for agent %s" agent))))
    cfg))

(defun gptel-fleet--leading-comments (file)
  "Leading \";; \" comment lines of FILE (minus the mode line), joined."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let (lines)
        (while (looking-at "^;;\\(?: \\(.*\\)\\)?$")
          (let ((l (or (match-string 1) "")))
            (unless (string-match-p "-\\*-" l) (push l lines)))
          (forward-line 1))
        (and lines (string-join (nreverse lines) "\n"))))))

(defun gptel-fleet-agent-enabled-p (agent)
  "Non-nil unless AGENT is disabled in its override.  Main agents always are."
  (or (member agent gptel-fleet-main-agents)
      (gptel-fleet-config-get agent :enabled t)))

(defun gptel-fleet-delegate-p (agent)
  "Non-nil if AGENT is offered as a subagent_type.
Default: every agent except \"gptel-agent\", as in gptel-agent itself."
  (and (gptel-fleet-agent-enabled-p agent)
       (gptel-fleet-config-get agent :delegate (not (equal agent "gptel-agent")))))

;;;; Small helpers

(defun gptel-fleet--set-p (v)
  "Non-nil if V counts as provided (not nil, :json-false or \"\")."
  (not (or (null v) (eq v :json-false) (equal v ""))))

(defun gptel-fleet--now ()
  "ISO-8601 timestamp with milliseconds."
  (format-time-string "%Y-%m-%dT%H:%M:%S.%3N%z"))

(defun gptel-fleet-new-id (&optional prefix)
  "Return a fresh, sortable run id, optionally with PREFIX."
  (format "%s%s-%04x" (or prefix "")
          (format-time-string "%Y%m%dT%H%M%S")
          (random #x10000)))

(defun gptel-fleet--truncate (s n)
  "Truncate string S to N characters with a marker."
  (let ((s (if (stringp s) s (format "%S" s))))
    (if (and n (> (length s) n))
        (concat (substring s 0 n)
                (format "\n…[truncated %d chars]" (- (length s) n)))
      s)))

(defun gptel-fleet--one-line (s &optional n)
  "S on one line, at most N (default 80) characters."
  (truncate-string-to-width
   (replace-regexp-in-string "[\n\t ]+" " " (format "%s" s)) (or n 80) nil nil "…"))

(defun gptel-fleet-all-tools ()
  "Flat list of all tool structs registered with gptel."
  (cl-loop for (_cat . tools) in gptel--known-tools
           append (mapcar #'cdr tools)))

(defun gptel-fleet-tool-names ()
  "Names of all registered gptel tools."
  (delete-dups (mapcar #'gptel-tool-name (gptel-fleet-all-tools))))

(defun gptel-fleet-find-tool (name)
  "Tool struct called NAME, or nil."
  (cl-find name (gptel-fleet-all-tools) :key #'gptel-tool-name :test #'equal))

(defun gptel-fleet--arg-plist (tool arg-values)
  "Map positional ARG-VALUES of TOOL to a plist keyed like :command."
  (cl-loop for spec in (gptel-tool-args tool)
           for val in arg-values
           append (list (intern (concat ":" (plist-get spec :name))) val)))

(provide 'gptel-fleet-core)
;;; gptel-fleet-core.el ends here
