;;; gptel-fleet-ask.el --- AskUser: multiple-choice questions from agents  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Georg Alberding
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The AskUser tool lets an agent ask you a multiple-choice question and
;; wait for the answer.  Nothing pops up and nothing times out.
;;
;; Inline (`gptel-fleet-approval-ui' = inline, the default): the question
;; appears in the chat buffer where gptel shows tool confirmations; for a
;; subagent, inside its task block.  With point on it:
;;
;;   1-9      choose an option (single choice: answers at once;
;;            multiple choice: toggles, then RET / C-c C-c)
;;   o        answer in your own words (if the agent allows it)
;;   d        decline to answer (the agent is told and continues)
;;   C-c C-k  cancel the whole request (gptel's cancel)
;;
;; The question is also listed in `gptel-fleet-approvals' (and counted in
;; Fleet:N).  There, y / e answer it through the minibuffer, n declines,
;; RET jumps to it.  From anywhere, `gptel-fleet-goto-question' puts
;; point on the oldest question (then press a digit), and
;; `gptel-fleet-answer-question' answers it in the minibuffer.
;;
;; Queue mode: the question waits only in the queue.
;;
;; Which agents get the tool: `gptel-fleet-ask-agents' (default: the
;; main agents); others through :tools ("AskUser") or :tool-packages
;; (interaction).

;;; Code:

(require 'gptel-fleet-core)
(require 'gptel-fleet-policy)
(require 'gptel-fleet-log)
(require 'gptel-fleet-approval)
(require 'gptel-fleet-tools)

(declare-function gptel--accept-tool-calls "gptel")
(defvar gptel--tool-preview-alist)

(defcustom gptel-fleet-ask-agents 'main
  "Agents that get the AskUser tool without listing it.
`main': the agents in `gptel-fleet-main-agents'.  t: every agent.  A
list of names: those agents.  nil: none (use :tools or :tool-packages)."
  :group 'gptel-fleet
  :type '(choice (const :tag "Main agents" main) (const :tag "All" t)
                 (const :tag "None" nil) (repeat string)))

(defconst gptel-fleet-ask-tool-name "AskUser")

(defcustom gptel-fleet-ask-move-point t
  "Move point onto a new inline question if you are idle at the current turn.
Only windows whose point is in the current exchange (where it is right
after sending: end of your prompt, or later) are moved; if you are
reading earlier parts of the chat or another buffer, nothing happens.
Then 1-9 answer at once."
  :group 'gptel-fleet
  :type 'boolean)

(defvar gptel-fleet--ask-answers (make-hash-table :test #'equal)
  "ARG-VALUES of an AskUser call -> answer text, set before gptel runs it.")

;;;; Arguments and answers

(defun gptel-fleet--ask-parse (arg-values)
  "Plist (:question :options :multiple :other) from AskUser ARG-VALUES."
  (pcase-let ((`(,q ,opts ,multi ,other) arg-values))
    (list :question (format "%s" (or q ""))
          :options (mapcar (lambda (o) (format "%s" o)) (append opts nil))
          :multiple (gptel-fleet--set-p (and (not (eq multi :json-false)) multi))
          ;; Absent means allowed; only an explicit false forbids it.
          :other (not (eq other :json-false)))))

(defun gptel-fleet--ask-answer-text (kind value)
  "Text returned to the model.  KIND is chosen, other or declined."
  (pcase kind
    ('chosen (if value
                 (format "The user chose: %s" (string-join value "; "))
               "The user chose none of the options."))
    ('other (format "The user answered in their own words: %s" value))
    ('declined "The user declined to answer.  Continue with your best \
judgement, or explain in your answer what you would need.")))

(defun gptel-fleet--ask-read (q)
  "Ask question plist Q in the minibuffer (only when you call it)."
  (let* ((opts (plist-get q :options))
         (other "Other (type an answer)") (decline "Decline to answer")
         (extra (append (and (plist-get q :other) (list other)) (list decline)))
         (prompt (format "%s " (plist-get q :question))))
    (if (plist-get q :multiple)
        (let ((sel (completing-read-multiple
                    (concat prompt "(comma separated) ") (append opts extra) nil t)))
          (cond ((member decline sel) (gptel-fleet--ask-answer-text 'declined nil))
                ((member other sel)
                 (gptel-fleet--ask-answer-text 'other (read-string "Your answer: ")))
                (t (gptel-fleet--ask-answer-text 'chosen sel))))
      (let ((c (completing-read prompt (append opts extra) nil t)))
        (cond ((equal c decline) (gptel-fleet--ask-answer-text 'declined nil))
              ((equal c other)
               (gptel-fleet--ask-answer-text 'other (read-string "Your answer: ")))
              (t (gptel-fleet--ask-answer-text 'chosen (list c))))))))

;;;; The tool

(defun gptel-fleet--ask-function (cb &rest arg-values)
  "AskUser implementation: CB gets the answer; ARG-VALUES as declared."
  (if-let* ((answer (gethash arg-values gptel-fleet--ask-answers)))
      (progn (remhash arg-values gptel-fleet--ask-answers)
             (funcall cb answer))
    ;; Not answered inline (queue mode, or gptel asked in the minibuffer):
    ;; wait in the fleet queue.
    (let* ((ctx (or gptel-fleet--acting (gptel-fleet-current-context)))
           (q (gptel-fleet--ask-parse arg-values))
           (req nil))
      (setq req (gptel-fleet-request-create
                 :id (gptel-fleet-new-id "ask-") :time (current-time) :ctx ctx
                 :tool-name gptel-fleet-ask-tool-name
                 :args (list :question (plist-get q :question)
                             :options (plist-get q :options))
                 :arg-values arg-values :source '(question) :no-learn t
                 :detail (gptel-fleet--ask-render-plain q)
                 :run-fn (lambda (answer &rest _) (funcall cb answer))
                 :deny-fn (lambda (_reason)
                            (funcall cb (gptel-fleet--ask-answer-text 'declined nil)))))
      (gptel-fleet-enqueue req))))

(defun gptel-fleet-register-ask-tool ()
  "Register the AskUser tool in package `interaction'."
  (gptel-fleet-register-package
   'interaction :origin 'builtin :level 'allow
   :description "Ask the user questions and wait for the answer")
  (gptel-fleet-register-tool
   'ask-user
   (list :package 'interaction :name gptel-fleet-ask-tool-name :level 'allow
         :async t :function #'gptel-fleet--ask-function
         :description "Ask the user a multiple-choice question and wait for the \
answer.  Use it when a decision is the user's to make (scope, trade-offs, \
which of several valid approaches) and you cannot settle it from the task \
or the code.  Keep options short and distinct; the user may also answer in \
their own words or decline.  Do not use it for permission to run tools: \
that is handled separately."
         :args '((:name "question" :type "string"
                  :description "The question, self-contained")
                 (:name "options" :type "array" :items (:type "string")
                  :description "2-9 answer options")
                 (:name "multiple" :type "boolean" :optional t
                  :description "true if several options may be chosen")
                 (:name "allow_other" :type "boolean" :optional t
                  :description "false to forbid free-text answers (default: allowed)"))))
  (setf (alist-get gptel-fleet-ask-tool-name gptel--tool-preview-alist nil nil #'equal)
        (list #'gptel-fleet--ask-preview-setup #'gptel-fleet--ask-preview-teardown)))

(defun gptel-fleet-ask-agent-p (agent)
  "Non-nil if AGENT gets AskUser by default."
  (pcase gptel-fleet-ask-agents
    ('main (member agent gptel-fleet-main-agents))
    ('t t)
    ((pred listp) (member agent gptel-fleet-ask-agents))))

;;;; Inline UI (hosted in gptel's tool-call confirmation)

(defun gptel-fleet--ask-render-plain (q)
  "Plain-text rendering of question plist Q (queue detail view)."
  (concat (plist-get q :question) "\n\n"
          (cl-loop for o in (plist-get q :options) for i from 1
                   concat (format "  %d  %s\n" i o))
          (if (plist-get q :multiple) "\n(several may be chosen)\n" "")))

(defvar-keymap gptel-fleet-ask-map
  "1" #'gptel-fleet-ask-pick "2" #'gptel-fleet-ask-pick "3" #'gptel-fleet-ask-pick
  "4" #'gptel-fleet-ask-pick "5" #'gptel-fleet-ask-pick "6" #'gptel-fleet-ask-pick
  "7" #'gptel-fleet-ask-pick "8" #'gptel-fleet-ask-pick "9" #'gptel-fleet-ask-pick
  "SPC" #'gptel-fleet-ask-pick
  "o" #'gptel-fleet-ask-other
  "d" #'gptel-fleet-ask-decline
  "RET" #'gptel-fleet-ask-submit
  "C-c C-c" #'gptel-fleet-ask-submit)

(defun gptel-fleet--ask-preview-setup (arg-values _info)
  "Insert the question for ARG-VALUES at point; return its overlay."
  (let* ((q (gptel-fleet--ask-parse arg-values))
         (tok (gethash (cons gptel-fleet-ask-tool-name arg-values) gptel-fleet--tokens))
         (agent (or (plist-get (plist-get tok :ctx) :agent) "The agent"))
         (multi (plist-get q :multiple))
         (from (point)))
    (insert (propertize (format "%s asks:\n" agent) 'face 'font-lock-keyword-face)
            (propertize (concat "  " (plist-get q :question) "\n") 'face 'bold))
    (cl-loop for o in (plist-get q :options) for i from 1
             do (insert (format "  %s%d  %s\n" (if multi "[ ] " "") i o)))
    (insert (propertize
             (concat "  "
                     (if multi "1-9 toggle · RET answer" "1-9 answer")
                     (if (plist-get q :other) " · o own answer" "")
                     " · d decline · C-c C-k cancel request\n")
             'face 'shadow))
    (let ((ov (make-overlay from (point) nil t)))
      (overlay-put ov 'gptel-fleet-ask t)
      (overlay-put ov 'ask-values arg-values)
      (overlay-put ov 'ask-question q)
      (overlay-put ov 'ask-selected nil)
      (overlay-put ov 'priority 20)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'keymap (make-composed-keymap gptel-fleet-ask-map
                                                    gptel-tool-call-actions-map))
      (run-at-time 0 nil (lambda ()
                           (gptel-fleet--ask-focus ov)
                           (gptel-fleet-update-indicator)
                           (when gptel-fleet-approval-notify
                             (message "gptel-fleet: %s asks: %s" agent
                                      (gptel-fleet--one-line (plist-get q :question) 80)))))
      ov)))

(defun gptel-fleet--ask-focus (ov)
  "Put point on question OV in windows idling at the current exchange."
  (when (and gptel-fleet-ask-move-point (overlay-buffer ov))
    (let* ((buf (overlay-buffer ov))
           (tool-ov (with-current-buffer buf
                      (cdr (get-char-property-and-overlay (overlay-start ov) 'gptel-tool))))
           (turn (if tool-ov (1- (overlay-start tool-ov)) (overlay-start ov)))
           (idle-p (lambda (pos) (>= pos turn))))
      (dolist (win (get-buffer-window-list buf nil t))
        (when (funcall idle-p (window-point win))
          (set-window-point win (overlay-start ov))))
      (with-current-buffer buf
        (when (and (not (get-buffer-window buf t)) (funcall idle-p (point)))
          (goto-char (overlay-start ov)))))))

(defun gptel-fleet--ask-display-advice (orig tool-calls info &rest args)
  "Around `gptel--display-tool-calls' (ORIG): label question-only batches.
gptel's header says \"<backend> wants to run\"; for TOOL-CALLS that are
all AskUser questions it says who asks instead.  INFO and ARGS pass on."
  (let ((prompt-ov (apply orig tool-calls info args)))
    (when (and (overlayp prompt-ov) tool-calls
               (cl-every (lambda (c) (and (consp c) (gptel-tool-p (car c))
                                          (equal (gptel-tool-name (car c))
                                                 gptel-fleet-ask-tool-name)))
                         tool-calls))
      (overlay-put prompt-ov 'before-string
                   (concat "\n" (propertize "Question — answer below (1-9, o, d)\n\n"
                                             'face 'font-lock-string-face))))
    prompt-ov))

(defun gptel-fleet--ask-preview-teardown (ov)
  "Remove the question overlay OV."
  (when (overlayp ov) (delete-overlay ov)))

(defun gptel-fleet--ask-overlay-at (&optional pos)
  "The question overlay at POS (default point)."
  (cdr (get-char-property-and-overlay (or pos (point)) 'gptel-fleet-ask)))

(defun gptel-fleet--ask-submit-overlay (ov answer)
  "Record ANSWER for question overlay OV; run the calls once all are answered."
  (let ((values (overlay-get ov 'ask-values)))
    (pcase-let ((`(,calls . ,tool-ov)
                 (with-current-buffer (overlay-buffer ov)
                   (get-char-property-and-overlay (overlay-start ov) 'gptel-tool))))
      (puthash values answer gptel-fleet--ask-answers)
      (gptel-fleet--ask-accept-if-done calls tool-ov))))

(defun gptel-fleet--ask-accept-if-done (calls tool-ov)
  "Accept CALLS, gptel's pending batch at TOOL-OV, once all questions are answered."
  (let ((open (cl-loop for c in calls
                       when (and (equal (gptel-tool-name (car c)) gptel-fleet-ask-tool-name)
                                 (not (gethash (gptel--map-tool-args (car c) (cadr c))
                                               gptel-fleet--ask-answers)))
                       count t))
        (others (cl-remove-if (lambda (c) (equal (gptel-tool-name (car c))
                                                 gptel-fleet-ask-tool-name))
                              calls)))
    (cond
     ((> open 0) (message "Answered; %d more question(s) in this batch" open))
     ((and others
           (not (y-or-n-p (format "Answering also runs %s.  Continue? "
                                  (mapconcat (lambda (c) (gptel-tool-name (car c)))
                                             others ", ")))))
      (message "Answer kept; run the batch with C-c C-c when ready"))
     (t (gptel--accept-tool-calls calls tool-ov)
        (gptel-fleet-update-indicator)))))

(defun gptel-fleet-ask-pick ()
  "Choose (single) or toggle (multiple) the option under the typed digit."
  (interactive)
  (let* ((ov (or (gptel-fleet--ask-overlay-at) (user-error "No question here")))
         (q (overlay-get ov 'ask-question))
         (opts (plist-get q :options))
         (key (event-basic-type last-command-event))
         (n (if (and (characterp key) (<= ?1 key ?9)) (- key ?0)
              ;; SPC: the option on the current line
              (save-excursion
                (beginning-of-line)
                (and (looking-at "  \\(?:\\[.\\] \\)?\\([0-9]\\)  ")
                     (string-to-number (match-string 1)))))))
    (unless (and n (<= 1 n (length opts))) (user-error "No option %s" (or n "here")))
    (if (not (plist-get q :multiple))
        (gptel-fleet--ask-submit-overlay
         ov (gptel-fleet--ask-answer-text 'chosen (list (nth (1- n) opts))))
      (let* ((sel (overlay-get ov 'ask-selected))
             (sel (if (memq n sel) (delq n sel) (cons n sel))))
        (overlay-put ov 'ask-selected sel)
        (save-excursion
          (goto-char (overlay-start ov))
          (when (re-search-forward (format "^  \\[\\(.\\)\\] %d  " n) (overlay-end ov) t)
            (let ((inhibit-read-only t))
              (replace-match (if (memq n sel) "x" " ") t t nil 1))))))))

(defun gptel-fleet-ask-submit ()
  "Send the selected options (multiple choice)."
  (interactive)
  (let* ((ov (or (gptel-fleet--ask-overlay-at) (user-error "No question here")))
         (q (overlay-get ov 'ask-question)))
    (if (not (plist-get q :multiple))
        (user-error "Press the number of your answer")
      (gptel-fleet--ask-submit-overlay
       ov (gptel-fleet--ask-answer-text
           'chosen (mapcar (lambda (n) (nth (1- n) (plist-get q :options)))
                           (sort (copy-sequence (overlay-get ov 'ask-selected)) #'<)))))))

(defun gptel-fleet-ask-other ()
  "Answer in your own words."
  (interactive)
  (let* ((ov (or (gptel-fleet--ask-overlay-at) (user-error "No question here")))
         (q (overlay-get ov 'ask-question)))
    (unless (plist-get q :other) (user-error "This question allows only the listed options"))
    (gptel-fleet--ask-submit-overlay
     ov (gptel-fleet--ask-answer-text 'other (read-string "Your answer: ")))))

(defun gptel-fleet-ask-decline ()
  "Decline to answer; the agent is told and continues."
  (interactive)
  (gptel-fleet--ask-submit-overlay
   (or (gptel-fleet--ask-overlay-at) (user-error "No question here"))
   (gptel-fleet--ask-answer-text 'declined nil)))

;;;; From the queue, or from anywhere

(defun gptel-fleet--ask-inline-calls (calls)
  "The AskUser calls among gptel's pending CALLS."
  (cl-remove-if-not (lambda (c) (equal (gptel-tool-name (car c)) gptel-fleet-ask-tool-name))
                    calls))

(defun gptel-fleet-ask-answer-inline (buf tool-ov calls &optional decline)
  "Answer the questions among CALLS (pending at TOOL-OV in BUF) in the minibuffer.
With DECLINE, decline them all."
  (dolist (c (gptel-fleet--ask-inline-calls calls))
    (let ((values (gptel--map-tool-args (car c) (cadr c))))
      (puthash values
               (if decline (gptel-fleet--ask-answer-text 'declined nil)
                 (gptel-fleet--ask-read (gptel-fleet--ask-parse values)))
               gptel-fleet--ask-answers)))
  (with-current-buffer buf (gptel-fleet--ask-accept-if-done calls tool-ov)))

(defun gptel-fleet-ask-answer-request (req &optional decline)
  "Answer the queued question REQ in the minibuffer (or DECLINE it)."
  (let ((answer (if decline (gptel-fleet--ask-answer-text 'declined nil)
                  (gptel-fleet--ask-read
                   (gptel-fleet--ask-parse (gptel-fleet-request-arg-values req))))))
    (setq gptel-fleet--queue (delq req gptel-fleet--queue))
    (gptel-fleet--close-call-view req)
    (gptel-fleet-log-event (gptel-fleet-request-ctx req) "approval"
                           (list :tool gptel-fleet-ask-tool-name :answer answer))
    (gptel-fleet-update-indicator)
    (funcall (gptel-fleet-request-run-fn req) answer)))

;;;###autoload
(defun gptel-fleet-goto-question ()
  "Move point onto the oldest waiting inline question, so 1-9 answer it."
  (interactive)
  (let ((entry (or (cl-find-if (lambda (e) (gptel-fleet--ask-inline-calls (nth 2 e)))
                               (gptel-fleet--inline-pending))
                   (user-error "No inline question waiting"))))
    (pop-to-buffer (car entry))
    (goto-char (overlay-start
                (cl-find-if (lambda (o) (overlay-get o 'gptel-fleet-ask))
                            (overlays-in (overlay-start (nth 1 entry))
                                         (overlay-end (nth 1 entry))))))))

;;;###autoload
(defun gptel-fleet-answer-question ()
  "Answer the oldest waiting AskUser question, inline or queued."
  (interactive)
  (if-let* ((inline (cl-find-if (lambda (e) (gptel-fleet--ask-inline-calls (nth 2 e)))
                                (gptel-fleet--inline-pending))))
      (apply #'gptel-fleet-ask-answer-inline inline)
    (if-let* ((req (cl-find gptel-fleet-ask-tool-name gptel-fleet--queue
                            :key #'gptel-fleet-request-tool-name :test #'equal)))
        (gptel-fleet-ask-answer-request req)
      (user-error "No questions waiting"))))

(provide 'gptel-fleet-ask)
;;; gptel-fleet-ask.el ends here
