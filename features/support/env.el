;;; env.el --- Ecukes environment for gptel-fleet  -*- lexical-binding: t; -*-

;; Run with:  make ecukes
;; Every scenario gets a fresh fleet directory and a fresh mock LLM.

(require 'f)
(require 'espuds)
(require 'ert)

(defvar fleet-ecukes-root (f-parent (f-parent (f-dirname load-file-name))))
(setenv "FLEET_ROOT" fleet-ecukes-root)
(setenv "FLEET_NO_AUTOSTART" "1")
(load (f-join fleet-ecukes-root "test" "interactive" "fleet-init.el") nil t)

(defvar fleet-ecukes-mock nil)
(defvar fleet-ecukes-dir nil)

(defun fleet-ecukes-free-port ()
  (let* ((p (make-network-process :name "port" :server t :host "127.0.0.1" :service t))
         (port (process-contact p :service)))
    (delete-process p) port))

(defun fleet-ecukes-start (scenario ui)
  "Fresh fleet state, mock LLM and chat buffer for SCENARIO with approval UI."
  (setq fleet-ecukes-dir (make-temp-file "fleet-ecukes" t))
  (let ((port (fleet-ecukes-free-port)))
    (fleet-test-reset fleet-ecukes-dir port)
    (setq fleet-ecukes-mock
          (let ((process-environment (append '("no_proxy=127.0.0.1" "NO_PROXY=127.0.0.1")
                                             process-environment)))
            (start-process "mock-llm" " *mock-llm*" "python3"
                           (f-join fleet-ecukes-root "test" "mock_llm.py")
                           (number-to-string port)
                           (f-join fleet-ecukes-dir "requests.jsonl")
                           fleet-test-target)))
    ;; Wait until the mock accepts connections.
    (let ((deadline (+ (float-time) 10)) up)
      (while (and (not up) (< (float-time) deadline))
        (setq up (ignore-errors
                   (delete-process (open-network-stream "probe" nil "127.0.0.1" port))
                   t))
        (unless up (sleep-for 0.2)))
      (unless up (error "Mock LLM did not start on port %s" port))))
  (setq gptel-fleet-approval-ui (intern ui))
  (setenv "no_proxy" "127.0.0.1,localhost")
  (setenv "NO_PROXY" "127.0.0.1,localhost")
  (delete-other-windows)
  (fleet-test-open-chat scenario))

(Before
 (setq fleet-ecukes-mock nil))

(After
 (when (process-live-p fleet-ecukes-mock) (delete-process fleet-ecukes-mock))
 (dolist (b (buffer-list))
   (when (string-match-p "\\`\\*\\(gptel-fleet\\|fleet-chat\\)" (buffer-name b))
     (with-current-buffer b (set-buffer-modified-p nil))
     (kill-buffer b)))
 (when fleet-ecukes-dir (delete-directory fleet-ecukes-dir t)))
