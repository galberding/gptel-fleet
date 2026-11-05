;;; gptel-fleet-steps.el --- Ecukes steps for gptel-fleet  -*- lexical-binding: t; -*-

(defun fleet-ecukes-wait (pred &optional secs)
  (let ((deadline (+ (float-time) (or secs 30))))
    (while (and (< (float-time) deadline) (not (funcall pred)))
      (accept-process-output nil 0.1))
    (funcall pred)))

(Given "^a fleet session for scenario \"\\([^\"]+\\)\" with \\(inline\\|queue\\) approvals$"
  (lambda (scenario ui) (fleet-ecukes-start scenario ui)))

(When "^I send the prompt$"
  (lambda ()
    (switch-to-buffer "*fleet-chat*")
    (goto-char (point-max))
    (call-interactively #'gptel-send)))

(Then "^I wait until buffer \"\\([^\"]+\\)\" shows \"\\([^\"]+\\)\"$"
  (lambda (buf text)
    (cl-assert (fleet-ecukes-wait
                (lambda () (and (get-buffer buf)
                                (with-current-buffer buf
                                  (save-excursion (goto-char (point-min))
                                                  (search-forward text nil t))))))
               nil "Buffer %s never showed %S" buf text)))

(Then "^a tool call waits inline$"
  (lambda ()
    (cl-assert (fleet-ecukes-wait (lambda () (gptel-fleet--inline-pending)))
               nil "No inline pending tool call appeared")))

(Then "^the mode line says \"\\([^\"]*\\)\"$"
  (lambda (expected)
    (gptel-fleet-update-indicator)
    (let ((shown (or (eval (cadr gptel-fleet-mode-line) t) "")))
      (cl-assert (equal (string-trim shown) expected) nil
                 "Mode line shows %S, expected %S" shown expected))))

(Then "^no window shows \"\\([^\"]+\\)\"$"
  (lambda (prefix)
    (cl-assert (not (cl-some (lambda (w) (string-prefix-p prefix (buffer-name (window-buffer w))))
                             (window-list)))
               nil "A window shows a buffer starting with %S" prefix)))

(Then "^there \\(?:is\\|are\\) \\([0-9]+\\) windows?$"
  (lambda (n)
    (cl-assert (= (length (window-list)) (string-to-number n)) nil
               "There are %d windows" (length (window-list)))))

(When "^I go to the tool call overlay$"
  (lambda ()
    (pcase-let ((`(,buf ,ov ,_) (car (gptel-fleet--inline-pending))))
      (switch-to-buffer buf)
      (goto-char (overlay-start ov)))))

(When "^I open the approval queue$"
  (lambda () (call-interactively #'gptel-fleet-approvals)))

(When "^I go to the row for \"\\([^\"]+\\)\"$"
  (lambda (tool)
    (goto-char (point-min))
    (cl-assert (re-search-forward (regexp-quote tool) nil t) nil "No row for %s" tool)
    (beginning-of-line)))

(Then "^the queue row says agent \"\\([^\"]+\\)\" tool \"\\([^\"]+\\)\"$"
  (lambda (agent tool)
    (let ((e (tabulated-list-get-entry)))
      (cl-assert (and e (equal (aref e 1) agent) (equal (aref e 2) tool)) nil
                 "Row is %S" e))))

(Then "^the current buffer is in a mode derived from \\(.+\\)$"
  (lambda (mode)
    (cl-assert (derived-mode-p (intern mode)) nil "Major mode is %s" major-mode)))

(When "^I replace the buffer contents with \"\\([^\"]*\\)\"$"
  (lambda (text) (erase-buffer) (insert text)))

(Then "^agent \"\\([^\"]+\\)\" has \\(:[a-z-]+\\) \\(.+\\)$"
  (lambda (agent key value)
    (let ((actual (gptel-fleet-config-get agent (intern key))))
      (cl-assert (equal actual (car (read-from-string value))) nil
                 "%s %s is %S" agent key actual))))

(Then "^agent \"\\([^\"]+\\)\" has no \\(:[a-z-]+\\)$"
  (lambda (agent key)
    (cl-assert (not (plist-member (gptel-fleet-config agent) (intern key))) nil
               "%s still has %s" agent key)))

(When "^I go to the agent \"\\([^\"]+\\)\"$"
  (lambda (agent)
    (goto-char (point-min))
    (while (and (not (eobp)) (not (equal (tabulated-list-get-id) agent)))
      (forward-line 1))
    (cl-assert (equal (tabulated-list-get-id) agent) nil "No row for agent %s" agent)))

(When "^I go to the checklist item \"\\([^\"]+\\)\"$"
  (lambda (id)
    (goto-char (point-min))
    (while (and (not (eobp)) (not (equal (tabulated-list-get-id) id)))
      (forward-line 1))
    (cl-assert (equal (tabulated-list-get-id) id) nil "No item %s" id)))

(Then "^the change queue holds \\([0-9]+\\) requests?$"
  (lambda (n)
    (cl-assert (fleet-ecukes-wait
                (lambda () (= (length gptel-fleet--queue) (string-to-number n))) 30)
               nil "Queue holds %d" (length gptel-fleet--queue))))
