;;; fleet-init.el --- Interactive test session for gptel-fleet  -*- lexical-binding: t; -*-

;; Loaded by `emacs -nw -Q -l fleet-init.el' inside tmux (tmux_scenarios.py)
;; and by the Ecukes support file (features/support/env.el).  Environment:
;;   FLEET_ROOT          gptel-fleet checkout
;;   FLEET_LOADPATH      colon-separated extra load-path (gptel, gptel-agent, ...)
;;   FLEET_DIR           fresh gptel-fleet-directory
;;   FLEET_PORT          port of test/mock_llm.py
;;   FLEET_SCENARIO      delegate | bash | fleet
;;   FLEET_UI            inline | queue
;;   FLEET_NO_AUTOSTART  set by Ecukes, which calls the functions itself

;;; Code:

(dolist (d (split-string (or (getenv "FLEET_LOADPATH") "") ":" t))
  (add-to-list 'load-path d))
(add-to-list 'load-path (getenv "FLEET_ROOT"))
(setq inhibit-startup-screen t
      make-backup-files nil
      create-lockfiles nil
      gptel-use-curl t)
(require 'gptel)
(require 'gptel-openai)
(require 'gptel-agent)
(require 'gptel-fleet)
(setq gptel-fleet-approval-ui (intern (or (getenv "FLEET_UI") "inline")))

(defvar fleet-test-backend
  (gptel-make-openai "FleetMock" :protocol "http"
                     :host (format "127.0.0.1:%s" (or (getenv "FLEET_PORT") "1"))
                     :key "x" :stream nil :models '(big small)))

(defvar fleet-test-target nil "File the mock subagent reads.")

(defun fleet-test-reset (&optional dir port)
  "Fresh fleet state in DIR (default $FLEET_DIR) talking to the mock on PORT."
  (setq gptel-fleet-directory (file-name-as-directory (or dir (getenv "FLEET_DIR"))))
  (make-directory gptel-fleet-directory t)
  (when port
    ;; The backend's URL is computed at creation, so re-create it.
    (setq fleet-test-backend
          (gptel-make-openai "FleetMock" :protocol "http"
                             :host (format "127.0.0.1:%s" port)
                             :key "x" :stream nil :models '(big small))))
  (setq gptel-fleet--overrides nil gptel-fleet--session-rules nil gptel-fleet--learned nil
        gptel-fleet--queue nil)
  (clrhash gptel-fleet--tokens)
  (unless gptel-fleet-mode (gptel-fleet-mode 1))
  (gptel-fleet-config-set "researcher" :backend "FleetMock")
  (gptel-fleet-config-set "researcher" :model 'small)
  (gptel-fleet-config-set "researcher" :tools '("Read" "Bash"))
  (gptel-fleet-config-set "researcher" :policy '((confirm :tool "Read")))
  (gptel-fleet-config-set "gptel-agent" :policy '((allow :package orchestration)
                                                  (confirm :tool "Bash")))
  (gptel-fleet-config-set "gptel-agent" :tool-packages '(fleet))
  (gptel-fleet-apply t)
  (setq fleet-test-target (expand-file-name "target.txt" gptel-fleet-directory))
  (with-temp-file fleet-test-target (insert "SECRET-CONTENT-42\n")))

(defun fleet-test-open-chat (&optional scenario)
  "Open the chat buffer with the SCENARIO prompt, ready for <f5>."
  (interactive)
  (let ((buf (get-buffer-create "*fleet-chat*")))
    (switch-to-buffer buf)
    (let ((inhibit-read-only t)) (erase-buffer))
    (text-mode)
    (gptel--apply-preset 'gptel-agent (lambda (sym val) (set (make-local-variable sym) val)))
    (setq-local gptel--preset 'gptel-agent)
    (setq-local gptel-backend fleet-test-backend gptel-model 'big gptel-stream nil
                gptel-include-reasoning nil)
    (gptel-mode 1)
    (insert (format "scenario:%s please investigate %s"
                    (or scenario (getenv "FLEET_SCENARIO") "delegate") fleet-test-target))
    (local-set-key (kbd "<f5>") #'gptel-send)
    buf))

(unless (getenv "FLEET_NO_AUTOSTART")
  (fleet-test-reset)
  (fleet-test-open-chat)
  (message "fleet test session ready"))

;;; fleet-init.el ends here
