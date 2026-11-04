;;; gptel-fleet.el --- Agent, tool and approval manager for gptel-agent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Georg Alberding

;; Author: Georg Alberding <galberding@web.de>
;; Version: 0.5.0
;; Package-Requires: ((emacs "29.1") (gptel "0.9.9") (gptel-agent "0.1"))
;; Keywords: tools, convenience
;; URL: https://github.com/galberding/gptel-fleet

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; gptel-fleet sits on top of gptel-agent and adds:
;;
;; - `gptel-fleet-list-agents': a package-menu style list of all agents,
;;   with per-agent backend/model, tools, policy profile and run counts.
;; - Per-agent overrides in lisp-data files (`gptel-fleet-edit-agent'),
;;   validated on save, merged into gptel-agent's agent table, so the
;;   bundled agents never need editing.  This is how a subagent gets its
;;   own (cheaper) model.
;; - Graded approvals (deny / escalate / dry-run / confirm / notify /
;;   allow) per agent, package, tool and argument pattern, replacing
;;   gptel's t / nil / auto.  See gptel-fleet-policy.el.
;; - Tool packages and command tools that run argv lists without a shell
;;   (gptel-fleet-tools.el), and the forge (gptel-fleet-forge.el) that
;;   turns a Bash call into such a tool with a test bench.
;; - JSONL logs per agent run with a reconstruction view that splices
;;   subagent runs into the main run (gptel-fleet-log.el).
;;
;; Setup:
;;
;;   (use-package gptel-fleet
;;     :straight (:local-repo "~/emacs_setup/packages/gptel-fleet")
;;     :after gptel-agent
;;     :config (gptel-fleet-mode 1))
;;
;; `gptel-fleet-mode' installs everything; turning it off restores
;; gptel-agent's behaviour (tools un-adopted, advices removed).

;;; Code:

(require 'gptel-fleet-core)
(require 'gptel-fleet-policy)
(require 'gptel-fleet-log)
(require 'gptel-fleet-approval)
(require 'gptel-fleet-tools)
(require 'gptel-fleet-skills)
(require 'gptel-fleet-config)
(require 'gptel-fleet-list)
(require 'gptel-fleet-forge)
(require 'gptel-fleet-api)
(require 'gptel-agent)
(require 'gptel-agent-tools)

(defconst gptel-fleet--preset-keys
  '(:backend :model :tools :system :temperature :max-tokens :use-tools
    :include-reasoning :stream :use-context :include-tool-results
    :confirm-tool-calls :cache :description :parents :pre :post)
  "Keys of an agent plist that gptel understands as preset keys.")

;;;; Merging overrides into gptel-agent

(defvar gptel-fleet--agent-files nil
  "Alist (NAME . FILE) of agent definitions found by gptel-agent.")

(defvar gptel-fleet--frontmatter nil
  "Alist (NAME . PLIST): agent plists as gptel-agent parsed them, before overrides.")

(defun gptel-fleet--update-agents-advice (orig)
  "Around `gptel-agent--update-agents' (ORIG): drop disabled agents.
Directories in `gptel-agent-dirs' that no longer exist are skipped
instead of aborting the whole update."
  (let ((files (let ((gptel-agent-dirs (cl-remove-if-not #'file-directory-p gptel-agent-dirs)))
                 (funcall orig))))
    (setq gptel-fleet--agent-files files)
    (setq gptel-agent--agents
          (cl-remove-if-not (lambda (e) (gptel-fleet-agent-enabled-p (car e)))
                            gptel-agent--agents))
    (cl-remove-if-not (lambda (e) (gptel-fleet-agent-enabled-p (car e))) files)))

(defun gptel-fleet--effective-tools (name plist)
  "Tool names for agent NAME given its gptel-agent PLIST."
  (let ((base (if (plist-member (gptel-fleet-config name) :tools)
                  (gptel-fleet-config-get name :tools)
                (plist-get plist :tools)))
        (pkgs (gptel-fleet-package-tools (gptel-fleet-config-get name :tool-packages))))
    ;; Keep names gptel can resolve: tool names and category names alike.
    (cl-remove-if-not (lambda (n) (or (not (stringp n)) (ignore-errors (gptel-get-tool n))))
                      (delete-dups (append (copy-sequence base) pkgs)))))

(defun gptel-fleet--apply-override (name plist)
  "Return gptel-agent PLIST for NAME with the fleet override merged in."
  (let ((cfg (gptel-fleet-config name))
        (p (copy-sequence plist)))
    (when (plist-member cfg :backend) (setq p (plist-put p :backend (plist-get cfg :backend))))
    (when (plist-member cfg :model) (setq p (plist-put p :model (plist-get cfg :model))))
    (when (or (plist-member cfg :tools) (plist-get cfg :tool-packages))
      (setq p (plist-put p :tools (gptel-fleet--effective-tools name plist))))
    (cl-loop for (k v) on (plist-get cfg :preset) by #'cddr
             when (memq k gptel-fleet--preset-keys) do (setq p (plist-put p k v)))
    p))

(defcustom gptel-fleet-append-roster t
  "Append the authoritative list of delegate agents to main agents' prompts.
gptel-agent's bundled gptel-agent.md names subagents (e.g. introspector)
in its prose rather than through {{AGENTS}}, so disabling an agent would
otherwise leave the main agent believing it exists."
  :group 'gptel-fleet
  :type 'boolean)

(defun gptel-fleet--roster ()
  "Text block listing the agents available for delegation."
  (concat
   "<fleet_subagents>\nThe ONLY subagent types available to the Agent tool right now \
are listed below; ignore any other agent named elsewhere in these instructions.\n"
   (mapconcat
    (lambda (e)
      (let ((p (cdr e)))
        (format "- %s (%s:%s): %s" (car e)
                (let ((b (plist-get p :backend)))
                  (cond ((stringp b) b) ((and b (gptel-backend-p b)) (gptel-backend-name b))
                        (t "default")))
                (or (plist-get p :model) "default")
                (gptel-fleet--one-line (or (plist-get p :description) "") 200))))
    (cl-remove-if-not (lambda (e) (gptel-fleet-delegate-p (car e))) gptel-agent--agents)
    "\n")
   "\n</fleet_subagents>"))

(defun gptel-fleet--agent-update-advice (orig)
  "Around `gptel-agent-update' (ORIG): merge overrides, fix enum and presets."
  (gptel-fleet-load-overrides)
  (let ((result (funcall orig)))
    (setq gptel-fleet--frontmatter (copy-tree gptel-agent--agents))
    (setq gptel-agent--agents
          (mapcar (lambda (e)
                    (cons (car e)
                          (gptel-fleet-apply-skills
                           (car e) (gptel-fleet--apply-override (car e) (cdr e)))))
                  gptel-agent--agents))
    ;; Delegation targets: enabled and :delegate not nil.  Defaults match
    ;; gptel-agent: everything except "gptel-agent" itself.
    (when-let* ((agent-tool (gptel-get-tool "Agent")))
      (setf (plist-get (car (gptel-tool-args agent-tool)) :enum)
            (vconcat (cl-loop for (name . _) in gptel-agent--agents
                              when (gptel-fleet-delegate-p name)
                              collect name))))
    (when gptel-fleet-append-roster
      (dolist (main gptel-fleet-main-agents)
        (when-let* ((cell (assoc main gptel-agent--agents))
                    (sys (plist-get (cdr cell) :system))
                    ((stringp sys)))
          (setcdr cell (plist-put (copy-sequence (cdr cell)) :system
                                  (concat sys "\n\n" (gptel-fleet--roster)))))))
    (dolist (main gptel-fleet-main-agents)
      (when-let* ((plist (assoc-default main gptel-agent--agents)))
        (apply #'gptel-make-preset (intern main) plist)))
    (or gptel-agent--agents result)))

;;;; Applying to live buffers

(defun gptel-fleet--agent-buffers ()
  "Live gptel buffers whose preset is a main agent."
  (cl-loop for buf in (buffer-list)
           when (with-current-buffer buf
                  (and (bound-and-true-p gptel-mode)
                       (member (format "%s" (bound-and-true-p gptel--preset))
                               gptel-fleet-main-agents)))
           collect buf))

(defun gptel-fleet-refresh-buffer (&optional buffer)
  "Re-apply the main agent preset in BUFFER (default current).
This registers changed agents, models and the delegate list with an
already running main agent."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (when-let* ((preset (bound-and-true-p gptel--preset)))
      (gptel--apply-preset preset (lambda (sym val) (set (make-local-variable sym) val)))
      (message "gptel-fleet: refreshed %s in %s" preset (buffer-name)))))

;;;###autoload
(defun gptel-fleet-apply (&optional no-buffers)
  "Reload overrides, packages and learned rules; re-register all agents.
Live main-agent buffers are refreshed unless NO-BUFFERS."
  (interactive "P")
  (gptel-fleet-load-learned)
  (gptel-fleet-load-tool-levels)
  (when (file-directory-p gptel-fleet-agent-source-directory)
    (add-to-list 'gptel-agent-dirs
                 (file-name-as-directory gptel-fleet-agent-source-directory) t))
  (gptel-fleet-load-tool-packages)
  (gptel-fleet-register-api-tools)
  (gptel-fleet-adopt-all)
  (gptel-agent-update)
  (unless no-buffers
    (mapc #'gptel-fleet-refresh-buffer (gptel-fleet--agent-buffers)))
  (gptel-fleet--refresh-list-buffers)
  (message "gptel-fleet: %d agents, %d packages registered"
           (length gptel-agent--agents) (length gptel-fleet--packages)))

(defun gptel-fleet--refresh-list-buffers ()
  "Refresh open fleet list buffers."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (derived-mode-p 'tabulated-list-mode)
                 (string-prefix-p "gptel-fleet" (symbol-name major-mode)))
        (revert-buffer)))))

;;;; Mode

(defconst gptel-fleet--advices
  '((gptel-agent-update :around gptel-fleet--agent-update-advice)
    (gptel-agent--update-agents :around gptel-fleet--update-agents-advice)
    (gptel-agent--task :around gptel-fleet--task-advice)
    (gptel-request :around gptel-fleet--request-advice)
    (gptel-make-tool :filter-return gptel-fleet--make-tool-advice)
    (gptel-send :before gptel-fleet--send-advice)
    (gptel--accept-tool-calls :after gptel-fleet--refresh-advice)
    (gptel--reject-tool-calls :after gptel-fleet--forget-calls-advice)
    (gptel--steer-tool-calls :after gptel-fleet--forget-calls-advice)))

;;;###autoload
(define-minor-mode gptel-fleet-mode
  "Manage gptel-agent agents, tools, approvals and logs."
  :global t
  :lighter " Fleet"
  :group 'gptel-fleet
  (if gptel-fleet-mode
      (progn
        (pcase-dolist (`(,sym ,how ,fn) gptel-fleet--advices)
          (advice-add sym how fn))
        (add-hook 'gptel-pre-tool-call-functions #'gptel-fleet--pre-tool-hook)
        (add-hook 'gptel-post-tool-call-functions #'gptel-fleet--post-tool-hook)
        (add-hook 'gptel-post-response-functions #'gptel-fleet--post-response-hook)
        (gptel-fleet-install-handlers)
        (add-hook 'gptel-pre-tool-call-functions #'gptel-fleet--decide-hook 90)
        (unless (member 'gptel-fleet-mode-line global-mode-string)
          (setq global-mode-string (append (or global-mode-string '("")) '(gptel-fleet-mode-line))))
        (gptel-fleet-apply t))
    (pcase-dolist (`(,sym ,_how ,fn) gptel-fleet--advices)
      (advice-remove sym fn))
    (remove-hook 'gptel-pre-tool-call-functions #'gptel-fleet--pre-tool-hook)
    (remove-hook 'gptel-pre-tool-call-functions #'gptel-fleet--decide-hook)
    (setq global-mode-string (delete 'gptel-fleet-mode-line global-mode-string))
    (remove-hook 'gptel-post-tool-call-functions #'gptel-fleet--post-tool-hook)
    (remove-hook 'gptel-post-response-functions #'gptel-fleet--post-response-hook)
    (gptel-fleet-uninstall-handlers)
    (gptel-fleet-unadopt-all)
    (gptel-agent-update)))

(provide 'gptel-fleet)
;;; gptel-fleet.el ends here
