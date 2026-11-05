;;; gptel-fleet-test.el --- Tests for gptel-fleet  -*- lexical-binding: t; -*-

;; Run:  make test   (see Makefile)

;;; Code:

(require 'ert)
(require 'gptel)
(require 'gptel-agent)
(require 'gptel-openai)
(require 'gptel-fleet)

(defvar gptel-fleet-test--dir nil)

(defmacro gptel-fleet-test--with-env (&rest body)
  "Run BODY with a fresh fleet directory and `gptel-fleet-mode' on."
  (declare (indent 0))
  `(let* ((gptel-fleet-test--dir (make-temp-file "fleet-test" t))
          (gptel-fleet-directory (file-name-as-directory gptel-fleet-test--dir))
          (gptel-fleet-agent-source-directory
           (expand-file-name "agent-defs" gptel-fleet-test--dir))
          (gptel-fleet--overrides nil)
          (gptel-fleet--session-rules nil)
          (gptel-fleet--learned nil)
          (gptel-fleet--tool-level-overrides nil)
          (gptel-fleet--queue nil)
          (gptel-fleet-approval-ui 'queue)
          (gptel-fleet-floor-rules (default-value 'gptel-fleet-floor-rules))
          (gptel-agent-dirs gptel-agent-dirs)
          (gptel-fleet--changes (make-hash-table :test #'equal))
          (gptel-fleet--tokens (make-hash-table :test #'equal))
          (gptel-fleet--pending-count 0))
     (unwind-protect
         (progn (gptel-fleet-mode 1) ,@body)
       (gptel-fleet-mode -1)
       (dolist (b (buffer-list))
         (when (string-match-p "\\*gptel-fleet" (buffer-name b)) (kill-buffer b)))
       (delete-directory gptel-fleet-test--dir t))))

(defun gptel-fleet-test--call (tool-name &rest arg-values)
  "Call TOOL-NAME's (guarded, async) function and wait for the result."
  (let* ((tool (gptel-get-tool tool-name))
         (result :pending)
         (deadline (+ (float-time) 10)))
    (apply (gptel-tool-function tool) (lambda (r) (setq result r)) arg-values)
    (while (and (eq result :pending) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    result))

;;;; Policy

(ert-deftest gptel-fleet-test-segments ()
  (should (equal (gptel-fleet-shell-segments "cd x && make | tail -5; echo 'a && b'")
                 '("cd x" "make" "tail -5" "echo 'a && b'")))
  (should (equal (gptel-fleet-shell-segments "a || b") '("a" "b"))))

(ert-deftest gptel-fleet-test-rule-matching ()
  (gptel-fleet-test--with-env
    (should (gptel-fleet-rule-matches-p
             '(:tool "Bash" :arg :command :any-segment "\\`git push")
             "Bash" '(:command "make && git push origin") "x"))
    (should-not (gptel-fleet-rule-matches-p
                 '(:tool "Bash" :arg :command :all-segments "\\`git \\(status\\|log\\)")
                 "Bash" '(:command "git status && rm -rf x") "x"))
    (should (gptel-fleet-rule-matches-p
             '(:package fs-read) "Read" nil "x"))
    (let ((d (make-temp-file "under" t)))
      (should (gptel-fleet-rule-matches-p
               `(:args ((:file_path :under ,d))) "Read" (list :file_path (expand-file-name "a" d)) "x"))
      (should-not (gptel-fleet-rule-matches-p
                   `(:args ((:file_path :under ,d))) "Read"
                   (list :file_path (expand-file-name "../etc" d)) "x")))))

(ert-deftest gptel-fleet-test-resolution-order ()
  (gptel-fleet-test--with-env
    ;; Default for adopted tools follows the original :confirm slot.
    (should (eq (car (gptel-fleet-decide "researcher" "Read" '(:file_path "/x"))) 'allow))
    (should (eq (car (gptel-fleet-decide "researcher" "Bash" '(:command "ls"))) 'confirm))
    ;; Agent rule beats profile and default.
    (gptel-fleet-config-set "researcher" :profile 'readonly)
    (should (eq (car (gptel-fleet-decide "researcher" "Bash" '(:command "ls"))) 'deny))
    (gptel-fleet-config-set "researcher" :policy '((allow :tool "Bash" :arg :command
                                                          :all-segments "\\`ls\\b")))
    (should (eq (car (gptel-fleet-decide "researcher" "Bash" '(:command "ls -la"))) 'allow))
    (should (eq (car (gptel-fleet-decide "researcher" "Bash" '(:command "ls && rm x"))) 'deny))
    ;; Floor beats everything, even an agent allow-all.
    (gptel-fleet-config-set "researcher" :policy '((allow)))
    (should (eq (car (gptel-fleet-decide "researcher" "Bash" '(:command "git push"))) 'escalate))
    ;; Main agents get confirm instead of escalate.
    (should (eq (car (gptel-fleet-decide "gptel-agent" "Bash" '(:command "git push") t)) 'confirm))
    ;; Learned rules come after agent rules.
    (gptel-fleet-config-set "researcher" :policy '((deny :tool "Grep")))
    (gptel-fleet-learn "researcher" '(allow :tool "Grep"))
    (should (eq (car (gptel-fleet-decide "researcher" "Grep" nil)) 'deny))
    (gptel-fleet-learn "researcher" '(allow :tool "Glob") t)
    (should (file-exists-p (gptel-fleet-learned-file)))
    (should (eq (car (gptel-fleet-decide "researcher" "Glob" nil)) 'allow))
    ;; Tool level override from the tools list.
    (gptel-fleet-set-tool-level "WebFetch" 'notify)
    (should (eq (car (gptel-fleet-decide "executor" "WebFetch" nil)) 'notify))))

;;;; Overrides and registration

(ert-deftest gptel-fleet-test-overrides ()
  (gptel-fleet-test--with-env
    (gptel-make-openai "FleetTestBackend" :key "x" :models '(small big))
    (gptel-fleet-config-set "researcher" :backend "FleetTestBackend")
    (gptel-fleet-config-set "researcher" :model 'small)
    (gptel-fleet-config-set "researcher" :tools '("Read" "Grep"))
    (gptel-fleet-config-set "researcher" :tool-packages '(web))
    (gptel-fleet-config-set "introspector" :enabled nil)
    (gptel-fleet-config-set "executor" :delegate nil)
    (gptel-fleet-apply t)
    (let ((r (cdr (assoc "researcher" gptel-agent--agents))))
      (should (equal (plist-get r :backend) "FleetTestBackend"))
      (should (eq (plist-get r :model) 'small))
      (should (equal (sort (copy-sequence (plist-get r :tools)) #'string<)
                     '("Grep" "Read" "WebFetch" "WebSearch"))))
    (should-not (assoc "introspector" gptel-agent--agents))
    (let ((enum (append (plist-get (car (gptel-tool-args (gptel-get-tool "Agent"))) :enum) nil)))
      (should (member "researcher" enum))
      (should-not (member "introspector" enum))
      (should-not (member "executor" enum))
      (should-not (member "gptel-agent" enum)))
    ;; {{AGENTS}} expansion (executor) no longer advertises disabled agents.
    (should-not (string-match-p "`introspector`:"
                                (plist-get (cdr (assoc "executor" gptel-agent--agents)) :system)))
    ;; Main agents get the authoritative roster appended.
    (let ((sys (plist-get (cdr (assoc "gptel-agent" gptel-agent--agents)) :system)))
      (should (string-match-p "<fleet_subagents>" sys))
      (should (string-match-p "researcher (FleetTestBackend:small)" sys))
      (should-not (string-match-p "^- introspector" sys)))
    ;; Validation
    (should (null (cl-remove-if (lambda (p) (string-prefix-p "warning:" p))
                                (gptel-fleet-validate-config (gptel-fleet-config "researcher")))))
    (should (gptel-fleet-validate-config '(:backend "Nope" :policy ((maybe :tool "X")) :bogus 1)))
    (should (= 3 (length (cl-remove-if (lambda (p) (string-prefix-p "warning:" p))
                                       (gptel-fleet-validate-config
                                        '(:backend "Nope" :policy ((maybe)) :bogus 1)))))))
  ;; After the mode is off, everything is back.
  (should (assoc "introspector" gptel-agent--agents))
  (should (gptel-tool-confirm (gptel-get-tool "Bash"))))

(ert-deftest gptel-fleet-test-override-header-preserved ()
  (gptel-fleet-test--with-env
    (gptel-fleet-edit-agent "researcher")
    (kill-buffer)
    (gptel-fleet-config-set "researcher" :profile 'readonly)
    (with-temp-buffer
      (insert-file-contents (gptel-fleet-override-file "researcher"))
      (should (search-forward "Frontmatter tools" nil t)))))

;;;; Adopted tools

(ert-deftest gptel-fleet-test-adoption ()
  (gptel-fleet-test--with-env
    (let ((bash (gptel-get-tool "Bash")))
      (should (gptel-tool-async bash))
      (should-not (gptel-tool-confirm bash))
      (should (gethash "Bash" gptel-fleet--adopted)))
    (let ((f (make-temp-file "fleet-read" nil ".txt" "line one\nline two\n")))
      (should (string-match-p "line one" (gptel-fleet-test--call "Read" f nil nil)))
      (gptel-fleet-config-set "main" :policy `((deny :tool "Read")))
      (should (string-match-p "denied" (gptel-fleet-test--call "Read" f nil nil))))))

(ert-deftest gptel-fleet-test-new-tools-adopted ()
  (gptel-fleet-test--with-env
    (gptel-make-tool :name "FleetLateTool" :function (lambda () "late") :args nil
                     :description "late" :confirm t :category "late")
    (should (gethash "FleetLateTool" gptel-fleet--adopted))
    (should (eq (car (gptel-fleet-decide "main" "FleetLateTool" nil)) 'confirm))))

;;;; Command tools

(ert-deftest gptel-fleet-test-command-tool ()
  (gptel-fleet-test--with-env
    (eval '(gptel-fleet-define-package fleettest :description "t" :level allow) t)
    (eval '(gptel-fleet-define-tool fleet-echo
             :package fleettest
             :description "echo"
             :args ((:name "word" :type string :description "w")
                    (:name "loud" :type boolean :optional t :description "l"))
             :command ("echo" (:flag "-e" loud) "hello" word)
             :tail 1)
          t)
    (should (gptel-get-tool "FleetEcho"))
    (should (eq (gptel-fleet-tool-package "FleetEcho") 'fleettest))
    (let ((out (gptel-fleet-test--call "FleetEcho" "world" nil)))
      (should (string-match-p "exit 0" out))
      (should (string-match-p "hello world" out)))
    ;; Option injection is refused.
    (should (string-match-p "may not start with" (gptel-fleet-test--call "FleetEcho" "--help" nil)))
    ;; Environment can be stripped.
    (eval '(gptel-fleet-define-tool fleet-env :package fleettest :description "env"
             :args nil :command ("env") :inherit-env nil)
          t)
    (let ((process-environment (cons "FLEET_SECRET=xyz" process-environment)))
      (should-not (string-match-p "FLEET_SECRET" (gptel-fleet-test--call "FleetEnv"))))))

;;;; Approval queue

(ert-deftest gptel-fleet-test-approval ()
  (gptel-fleet-test--with-env
    (gptel-fleet-config-set "main" :policy '((confirm :tool "Glob")))
    (let* ((tool (gptel-get-tool "Glob")) (result :pending))
      (funcall (gptel-tool-function tool) (lambda (r) (setq result r)) "*.el" gptel-fleet-test--dir nil)
      (should (eq result :pending))
      (should (= 1 (length gptel-fleet--queue)))
      (gptel-fleet-answer (car gptel-fleet--queue) ?A)
      (should (stringp result))
      (should (null gptel-fleet--queue))
      ;; Learned: next call no longer asks... but the agent rule still comes first.
      (should (eq (car (gptel-fleet-decide "main" "Glob" nil)) 'confirm))
      (gptel-fleet-config-set "main" :policy nil)
      (should (eq (car (gptel-fleet-decide "main" "Glob" '(:pattern "x"))) 'allow))
      ;; Deny path
      (gptel-fleet-config-set "main" :policy '((confirm :tool "Grep")))
      (setq result :pending)
      (funcall (gptel-tool-function (gptel-get-tool "Grep"))
               (lambda (r) (setq result r)) "x" gptel-fleet-test--dir nil nil)
      (gptel-fleet-answer (car gptel-fleet--queue) ?n)
      (should (string-match-p "denied" result)))))

;;;; Forge

(ert-deftest gptel-fleet-test-forge-draft ()
  (gptel-fleet-test--with-env
    (let* ((draft (gptel-fleet-forge-draft
                   "cd /tmp && cargo test -p fleet-core -- policy 2>&1 | tail -50"
                   '((3 . "package") (5 . "filter"))))
           (spec (cdr draft)))
      (should (eq (car draft) 'cargo-test))
      (should (equal (plist-get spec :command)
                     '("cargo" "test" (:opt "-p" package) "--" filter)))
      (should (equal (plist-get spec :tail) 50))
      (should (equal (plist-get spec :cwd) "/tmp"))
      (should (equal (car (plist-get spec :examples)) '(:package "fleet-core" :filter "policy")))
      (should (equal (gptel-fleet-render-command
                      (plist-get spec :command) '(("package" . "a") ("filter" . nil))
                      (plist-get spec :args))
                     '("cargo" "test" "-p" "a" "--")))
      ;; Placeholder description is a warning, not an error.
      (should (cl-every (lambda (p) (string-prefix-p "warning:" p))
                        (gptel-fleet-forge-validate (car draft) spec))))
    (should (equal (gptel-fleet-forge-signature "cd x && git log -5 | head") "git log | head"))))

(ert-deftest gptel-fleet-test-forge-write-and-load ()
  (gptel-fleet-test--with-env
    (let* ((draft (gptel-fleet-forge-draft "echo hello world" '((2 . "who"))))
           (spec (plist-put (cdr draft) :description "Say hello to someone.")))
      (setq spec (plist-put spec :level 'allow))
      (let ((file (gptel-fleet-forge-write (car draft) spec)))
        (should (file-exists-p file))
        (load file nil t)
        (should (gptel-get-tool "EchoHello"))
        (should (string-match-p "hello fleet" (gptel-fleet-test--call "EchoHello" "fleet")))
        ;; Writing again replaces instead of duplicating.
        (gptel-fleet-forge-write (car draft) (plist-put spec :timeout 5))
        (with-temp-buffer
          (insert-file-contents file)
          (should (= 1 (how-many "(gptel-fleet-define-tool echo-hello" (point-min) (point-max)))))))))

;;;; End to end: main -> subagent on a cheaper model, logs, reconstruction

(defun gptel-fleet-test--free-port ()
  (let* ((p (make-network-process :name "port" :server t :host "127.0.0.1" :service t))
         (port (process-contact p :service)))
    (delete-process p) port))

(ert-deftest gptel-fleet-test-end-to-end ()
  (gptel-fleet-test--with-env
    (let* ((port (gptel-fleet-test--free-port))
           (reqlog (expand-file-name "requests.jsonl" gptel-fleet-test--dir))
           (target (make-temp-file "fleet-target" nil ".txt" "SECRET-CONTENT-42\n"))
           (server (start-process "mock-llm" "*mock-llm*" "python3"
                                  (expand-file-name "mock_llm.py"
                                                    (file-name-directory
                                                     (or load-file-name buffer-file-name
                                                         (locate-library "gptel-fleet-test"))))
                                  (number-to-string port) reqlog target))
           (backend (gptel-make-openai "FleetMock" :protocol "http"
                                       :host (format "127.0.0.1:%d" port)
                                       :key "x" :stream nil :models '(big small)))
           (gptel-use-curl t)
           (process-environment (append '("no_proxy=127.0.0.1,localhost"
                                          "NO_PROXY=127.0.0.1,localhost")
                                        process-environment)))
      (unwind-protect
          (progn
            (sleep-for 0.7)
            (gptel-fleet-config-set "researcher" :backend "FleetMock")
            (gptel-fleet-config-set "researcher" :model 'small)
            (gptel-fleet-config-set "researcher" :tools '("Read" "Bash"))
            (gptel-fleet-config-set "researcher" :policy '((allow :tool "Read")))
            (gptel-fleet-config-set "gptel-agent" :policy '((allow :package orchestration)))
            (gptel-fleet-apply t)
            (with-current-buffer (get-buffer-create "*fleet-e2e*")
              (erase-buffer)
              (text-mode)
              (gptel--apply-preset 'gptel-agent
                                   (lambda (sym val) (set (make-local-variable sym) val)))
              (setq-local gptel--preset 'gptel-agent)
              (setq-local gptel-backend backend gptel-model 'big gptel-stream nil
                          gptel-include-reasoning nil)
              (gptel-mode 1)
              (insert "Please investigate.")
              (gptel-send)
              (let ((deadline (+ (float-time) 30)))
                (while (and (< (float-time) deadline)
                            (not (save-excursion (goto-char (point-min))
                                                 (search-forward "MAIN DONE" nil t))))
                  (accept-process-output nil 0.1)))
              (should (save-excursion (goto-char (point-min)) (search-forward "MAIN DONE" nil t)))
              (should (save-excursion (goto-char (point-min))
                                      (search-forward "SUB REPORT" nil t))))
            ;; Models: main used big, subagent used small.
            (let ((reqs (gptel-fleet--read-jsonl reqlog)))
              (should (cl-every (lambda (r) (equal (alist-get 'model r)
                                                   (if (eq (alist-get 'main r) t) "big" "small")))
                                reqs))
              (should (cl-some (lambda (r) (equal (alist-get 'model r) "small")) reqs)))
            ;; Logs: one main run, one researcher child, linked.
            (let* ((idx (gptel-fleet-log-index))
                   (main (cl-find "gptel-agent" idx :key (lambda (e) (alist-get 'agent e))
                                  :test #'equal))
                   (child (cl-find "researcher" idx :key (lambda (e) (alist-get 'agent e))
                                   :test #'equal)))
              (should main) (should child)
              (should (equal (alist-get 'parent child) (alist-get 'run main)))
              (should (string-match-p "/researcher/" (alist-get 'file child)))
              (let ((evs (gptel-fleet--read-jsonl (alist-get 'file child))))
                (should (cl-find "decision" evs :key (lambda (e) (alist-get 'ev e)) :test #'equal))
                ;; Bash with git push hit the floor rule and escalated.
                (should (cl-find-if (lambda (e) (and (equal (alist-get 'ev e) "decision")
                                                     (equal (alist-get 'tool e) "Bash")
                                                     (equal (alist-get 'level e) "escalate")))
                                    evs))
                (should (cl-find "run-end" evs :key (lambda (e) (alist-get 'ev e)) :test #'equal)))
              ;; Reconstruction splices the child into the main run.
              (with-current-buffer (gptel-fleet-log-reconstruct (alist-get 'run main))
                (goto-char (point-min))
                (should (re-search-forward "^\\* gptel-agent" nil t))
                (should (re-search-forward "spawn → researcher" nil t))
                (should (re-search-forward "^\\*\\* researcher" nil t))
                (should (re-search-forward "SECRET-CONTENT-42" nil t))
                (should (re-search-forward "escalate" nil t)))))
        (delete-process server)
        (kill-buffer "*fleet-e2e*")))))

;;;; UI smoke tests

(ert-deftest gptel-fleet-test-ui-smoke ()
  (gptel-fleet-test--with-env
    (gptel-fleet-config-set "introspector" :enabled nil)
    (gptel-fleet-apply t)
    (with-current-buffer (gptel-fleet-list-agents)
      (should (= (length tabulated-list-entries) 5))
      (should (equal (aref (cadr (assoc "introspector" tabulated-list-entries)) 1) "off"))
      (revert-buffer)
      (goto-char (point-min))
      (should (search-forward "researcher" nil t)))
    (with-current-buffer (gptel-fleet-list-tools)
      (should (assoc "Bash" tabulated-list-entries))
      (should (equal (aref (cadr (assoc "Bash" tabulated-list-entries)) 1) "shell")))
    (with-current-buffer (gptel-fleet-policy-matrix "researcher")
      (should (assoc "Read" tabulated-list-entries)))
    (with-current-buffer (gptel-fleet-list-runs)
      (should (listp tabulated-list-entries)))
    (gptel-fleet-edit-agent "researcher")
    (should (eq major-mode 'gptel-fleet-config-mode))
    (goto-char (point-max))
    (insert "\n")
    (goto-char (point-min))
    (re-search-forward ":policy ()")
    (replace-match ":policy ((sometimes :tool \"Read\"))")
    (should (eq 'blocked (let ((debug-on-error nil))
                           (condition-case nil (progn (save-buffer) 'saved)
                             (user-error 'blocked)))))
    (should (gptel-fleet-config-validate))
    (set-buffer-modified-p nil)
    (kill-buffer)))

;;;; Skills and quick assignment

(defun gptel-fleet-test--make-skills (root names)
  "Create skills NAMES under ROOT."
  (dolist (n names)
    (let ((d (expand-file-name n root)))
      (make-directory d t)
      (with-temp-file (expand-file-name "SKILL.md" d)
        (insert "---\nname: " n "\ndescription: The " n " skill\n---\nBody of " n "\n")))))

(defun gptel-fleet-test--goto-id (id)
  "Move to the tabulated-list line whose id is ID."
  (goto-char (point-min))
  (while (and (not (eobp)) (not (equal (tabulated-list-get-id) id)))
    (forward-line 1))
  (should (equal (tabulated-list-get-id) id)))

(defmacro gptel-fleet-test--with-skills (&rest body)
  (declare (indent 0))
  `(let* ((skroot (make-temp-file "fleet-skills" t))
          (gptel-agent-skill-dirs (list (file-name-as-directory skroot))))
     (gptel-fleet-test--make-skills skroot '("alpha" "beta" "g-one" "g-two"))
     (unwind-protect (progn ,@body) (delete-directory skroot t))))

(ert-deftest gptel-fleet-test-skills ()
  (gptel-fleet-test--with-skills
    (gptel-fleet-test--with-env
      (should (equal (sort (gptel-fleet-skill-names) #'string<) '("alpha" "beta" "g-one" "g-two")))
      (gptel-fleet-config-set "researcher" :skills '("alpha" "g-*"))
      (gptel-fleet-config-set "introspector" :skills nil)
      (gptel-fleet-apply t)
      (let ((r (plist-get (cdr (assoc "researcher" gptel-agent--agents)) :system))
            (m (plist-get (cdr (assoc "gptel-agent" gptel-agent--agents)) :system)))
        (should (string-match-p "<name>alpha</name>" r))
        (should (string-match-p "<name>g-two</name>" r))
        (should-not (string-match-p "<name>beta</name>" r))
        ;; Inheriting agents still see everything.
        (should (string-match-p "<name>beta</name>" m)))
      ;; Empty list: no skills block entries and no Skill tool.
      (let ((p (cdr (assoc "introspector" gptel-agent--agents))))
        (should-not (member "Skill" (plist-get p :tools)))
        (should-not (string-match-p "<name>alpha</name>" (or (plist-get p :system) ""))))
      ;; Enforcement, independent of what the prompt showed.
      (should (eq (car (gptel-fleet-decide "researcher" "Skill" '(:skill "beta"))) 'deny))
      (should (equal (cadr (gptel-fleet-decide "researcher" "Skill" '(:skill "beta"))) 'skills))
      (should-not (eq (car (gptel-fleet-decide "researcher" "Skill" '(:skill "g-one"))) 'deny))
      (should-not (eq (car (gptel-fleet-decide "gptel-agent" "Skill" '(:skill "beta") t)) 'deny))
      (gptel-fleet-config-set "main" :policy '((allow :tool "Skill")))
      (gptel-fleet-config-set "main" :skills '("alpha"))
      (should (string-match-p "not assigned" (gptel-fleet-test--call "Skill" "beta" nil)))
      (should (string-match-p "Body of alpha" (gptel-fleet-test--call "Skill" "alpha" nil)))
      ;; Validation
      (should (cl-some (lambda (p) (string-match-p "unknown skill" p))
                       (gptel-fleet-validate-config '(:skills ("nope")))))
      (should-not (gptel-fleet-validate-config '(:skills ("g-*" "alpha")))))))

(ert-deftest gptel-fleet-test-assign-checklists ()
  (gptel-fleet-test--with-skills
    (gptel-fleet-test--with-env
      (gptel-fleet-config-set "researcher" :skills '("g-*" "alpha"))
      (gptel-fleet-apply t)
      ;; Skills: globs survive, explicit names follow the marks.
      (with-current-buffer (gptel-fleet-assign "researcher" 'skills)
        (should (= 3 (hash-table-count gptel-fleet--assign-selected)))
        (gptel-fleet-test--goto-id "alpha")
        (gptel-fleet-assign-unmark)
        (gptel-fleet-test--goto-id "beta")
        (gptel-fleet-assign-mark)
        (gptel-fleet-assign-save))
      (should (equal (gptel-fleet-config-get "researcher" :skills) '("g-*" "beta")))
      ;; Tools: explicit list from marks.
      (with-current-buffer (gptel-fleet-assign "executor" 'tools)
        (gptel-fleet-assign-unmark-all)
        (dolist (n '("Read" "Grep"))
          (gptel-fleet-test--goto-id n)
          (gptel-fleet-assign-mark))
        (gptel-fleet-assign-save))
      (should (equal (sort (copy-sequence (gptel-fleet-config-get "executor" :tools)) #'string<)
                     '("Grep" "Read")))
      ;; MCP-style tools land in a package named after their category.
      (gptel-make-tool :name "gitlab_get_mr" :function (lambda (id) id)
                       :args '((:name "id" :type string :description "MR"))
                       :description "Fetch MR" :category "gitlab")
      (should (eq (gptel-fleet-tool-package "gitlab_get_mr") 'gitlab))
      (with-current-buffer (gptel-fleet-assign "researcher" 'packages)
        (gptel-fleet-test--goto-id "gitlab")
        (gptel-fleet-assign-mark)
        (gptel-fleet-assign-save))
      (should (member "gitlab_get_mr"
                      (plist-get (cdr (assoc "researcher" gptel-agent--agents)) :tools))))))

;;;; Agent-facing API

(defun gptel-fleet-test--git-authors ()
  (let ((default-directory gptel-fleet-directory))
    (split-string (shell-command-to-string "git log --format=%an 2>/dev/null") "\n" t)))

(ert-deftest gptel-fleet-test-api-direct ()
  (gptel-fleet-test--with-env
    (should (string-match-p "# gptel-fleet" (gptel-fleet-api-overview)))
    (should (string-match-p "gptel-fleet-api-assign" (gptel-fleet-api-overview)))
    (should (string-prefix-p "APPLIED" (gptel-fleet-api-set "researcher" :profile 'readonly)))
    (should (eq (gptel-fleet-config-get "researcher" :profile) 'readonly))
    (should (string-prefix-p "REJECTED" (gptel-fleet-api-set "researcher" :bogus 1)))
    (should (string-prefix-p "REJECTED" (gptel-fleet-api-set "researcher" :policy '((maybe)))))
    (should (string-match-p "deny" (gptel-fleet-api-explain "researcher" "Bash" '(:command "ls"))))
    (should (string-prefix-p "APPLIED" (gptel-fleet-api-new-agent "gitlab-review" "Reviews MRs" "You review.")))
    (should (assoc "gitlab-review" gptel-fleet--agent-files))
    (should (eq (gptel-fleet-config-get "gitlab-review" :enabled t) nil))
    (when (executable-find "git")
      (should (>= (length (gptel-fleet-test--git-authors)) 2)))))

(ert-deftest gptel-fleet-test-api-agent-requests ()
  (gptel-fleet-test--with-env
    (let ((gptel-fleet--acting (list :agent "executor" :run "r1" :file
                                     (expand-file-name "x.jsonl" gptel-fleet-test--dir))))
      ;; Widening another agent: queued with diff and warning.
      (let ((res (gptel-fleet-api-assign "researcher" 'tools '("Read" "Bash"))))
        (should (string-prefix-p "PENDING" res))
        (should-not (gptel-fleet-config-get "researcher" :tools))
        (let ((req (car gptel-fleet--queue))
              (id (cadr (split-string res "[ :]+"))))
          (should (gptel-fleet-request-no-learn req))
          (should (string-match-p "WIDENS" (gptel-fleet-request-detail req)))
          (should (string-match-p "agents/researcher.eld" (gptel-fleet-request-detail req)))
          (should (string-match-p "pending" (gptel-fleet-api-status id)))
          (let ((gptel-fleet--acting nil))   ; the human answers
            (gptel-fleet-answer req ?A))      ; learning is refused -> plain yes
          (should (equal (gptel-fleet-config-get "researcher" :tools) '("Read" "Bash")))
          (should (string-match-p "applied" (gptel-fleet-api-status id)))
          (should-not (cdr (assoc "executor" gptel-fleet--learned)))
          (when (executable-find "git")
            (should (member "executor" (gptel-fleet-test--git-authors))))))
      ;; Denial
      (let ((res (gptel-fleet-api-set "researcher" :log nil)))
        (gptel-fleet-answer (car gptel-fleet--queue) ?n)
        (should (string-match-p "denied" (gptel-fleet-api-status (cadr (split-string res "[ :]+"))))))
      ;; Self-promotion is refused outright, narrowing yourself is fine.
      ;; Restricting an inheriting agent is narrowing; adding back is widening.
      (should (string-prefix-p "PENDING" (gptel-fleet-api-assign "executor" 'skills '("alpha"))))
      (gptel-fleet-config-set "executor" :skills '("alpha"))
      (should (string-prefix-p "REFUSED" (gptel-fleet-api-assign "executor" 'skills '("alpha" "beta"))))
      (should (string-prefix-p "REFUSED" (gptel-fleet-api-assign "executor" 'skills :inherit)))
      (gptel-fleet-config-set "executor" :tools '("Read" "Grep" "Bash"))
      (should (string-prefix-p "REFUSED" (gptel-fleet-api-assign "executor" 'tools '("Read" "Grep" "Bash" "Eval"))))
      (should (string-prefix-p "PENDING" (gptel-fleet-api-assign "executor" 'tools '("Read"))))
      (should (string-prefix-p "REFUSED"
                               (gptel-fleet-api-add-rule "executor" '(allow :tool "Bash"))))
      (should (string-prefix-p "PENDING"
                               (gptel-fleet-api-add-rule "executor" '(deny :tool "Bash"))))
      ;; Agents may not define code tools; command tools are queued.
      (should (string-prefix-p "REJECTED"
                               (gptel-fleet-api-define-tool 'evil '(:package x :name "Evil" :description "d"
                                                                    :args nil :level allow
                                                                    :function (lambda () 1)))))
      (should (string-prefix-p "PENDING"
                               (gptel-fleet-api-define-tool 'say-hi '(:package hello :name "SayHi"
                                                                       :description "Say hi" :args nil
                                                                       :level allow :command ("echo" "hi"))))))
    ;; Parent agent is protected too.
    (let ((gptel-fleet--acting (list :agent "researcher" :parent-agent "gptel-agent" :run "r2")))
      (should (string-prefix-p "REFUSED" (gptel-fleet-api-set "gptel-agent" :policy '((allow))))))
    (gptel-fleet-answer (car (last gptel-fleet--queue)) ?y)
    (should (gptel-get-tool "SayHi"))))

(ert-deftest gptel-fleet-test-api-via-tools-and-side-doors ()
  (gptel-fleet-test--with-env
    ;; Through the Fleet* tool (main agent context): queued, not applied.
    (should (string-match-p "PENDING"
                            (gptel-fleet-test--call "FleetSet" "researcher" ":profile" "trusted")))
    (should-not (gptel-fleet-config-get "researcher" :profile))
    (should (string-match-p "researcher"
                            (gptel-fleet-test--call "FleetAgent" "researcher")))
    ;; Side doors: writing fleet files or poking internals via Eval asks, even
    ;; for an agent whose own policy allows everything.
    (gptel-fleet-config-set "executor" :policy '((allow)))
    (should (eq (car (gptel-fleet-decide "executor" "Write"
                                         (list :path gptel-fleet-directory :filename "x.el"
                                               :content "")))
                'confirm))
    (should (eq (car (gptel-fleet-decide "executor" "Bash"
                                         (list :command (format "rm %sagents/x.eld" gptel-fleet-directory))))
                'confirm))
    (should (eq (car (gptel-fleet-decide "executor" "Eval"
                                         '(:expression "(setq gptel-fleet-floor-rules nil)")))
                'confirm))
    (should (eq (car (gptel-fleet-decide "executor" "Eval"
                                         '(:expression "(gptel-fleet-api-overview)")))
                'allow))
    (should (eq (car (gptel-fleet-decide "executor" "Write"
                                         '(:path "/tmp" :filename "x" :content "")))
                'allow))))

;;;; gptel-agent's Edit tool through the guard

(ert-deftest gptel-fleet-test-edit-tool ()
  (gptel-fleet-test--with-env
    (let* ((f (make-temp-file "edit-target" nil ".txt" "alpha\nbeta\ngamma\n"))
           (n (file-name-nondirectory f))
           (content (lambda () (with-temp-buffer (insert-file-contents f) (buffer-string)))))
      (should (eq (car (gptel-fleet-decide "main" "Edit" (list :path f))) 'confirm))
      (gptel-fleet-config-set "main" :policy '((allow :tool "Edit")))
      (gptel-fleet-test--call "Edit" f "beta" "BETA" nil)
      (should (equal (funcall content) "alpha\nBETA\ngamma\n"))
      (gptel-fleet-test--call
       "Edit" f nil (format "--- %s\n+++ %s\n@@ -1,3 +1,3 @@\n alpha\n-BETA\n+beta2\n gamma\n" n n) t)
      (should (equal (funcall content) "alpha\nbeta2\ngamma\n"))
      (let ((r (gptel-fleet-test--call "Edit" f "missing" "x" nil)))
        (should (string-prefix-p "Error: Could not find" r))))))

;;;; Editing a pending call in a buffer

(defun gptel-fleet-test--pending (tool-name &rest arg-values)
  "Queue a confirm call of TOOL-NAME; return (REQ . RESULT-CELL)."
  (let ((cell (list :pending)))
    (apply (gptel-tool-function (gptel-get-tool tool-name))
           (lambda (r) (setcar cell r)) arg-values)
    (cons (car (last gptel-fleet--queue)) cell)))

(defun gptel-fleet-test--wait (cell)
  (let ((deadline (+ (float-time) 10)))
    (while (and (eq (car cell) :pending) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (car cell)))

(ert-deftest gptel-fleet-test-edit-buffer ()
  (gptel-fleet-test--with-env
    (gptel-fleet-config-set "main" :policy '((confirm)))
    ;; Bash: shell mode, edited command runs, submit via C-c '
    (pcase-let ((`(,req . ,cell) (gptel-fleet-test--pending "Bash" "echo WRONG")))
      (with-current-buffer (gptel-fleet--edit-args req)
        (should (derived-mode-p 'sh-base-mode 'sh-mode 'bash-ts-mode))
        (should (equal (buffer-string) "echo WRONG"))
        (should (eq (key-binding (kbd "C-c '")) #'gptel-fleet-edit-submit))
        (should (eq (key-binding (kbd "C-c C-'")) #'gptel-fleet-edit-submit))
        (erase-buffer) (insert "echo RIGHT")
        (gptel-fleet-edit-submit))
      (should (string-match-p "RIGHT" (gptel-fleet-test--wait cell)))
      (should-not (memq req gptel-fleet--queue)))
    ;; Eval: emacs-lisp-mode, unbalanced parens refuse to submit
    (pcase-let ((`(,req . ,_cell) (gptel-fleet-test--pending "Eval" "(+ 1 2)")))
      (with-current-buffer (gptel-fleet--edit-args req)
        (should (derived-mode-p 'emacs-lisp-mode))
        (goto-char (point-max)) (insert "(")
        (should (memq req gptel-fleet--queue))
        (should (condition-case nil (progn (gptel-fleet-edit-submit) nil) (error t)))
        (should (memq req gptel-fleet--queue))
        (gptel-fleet-edit-abort))
      (should (memq req gptel-fleet--queue))
      (should-not (gptel-fleet-request-deferred req))
      (gptel-fleet-answer req ?n))
    ;; Edit: mode of the target file; diff mode when :diff is set;
    ;; switching args keeps the earlier edit.
    (let* ((f (make-temp-file "edit-target" nil ".el" "(a)\n(b)\n")))
      (pcase-let ((`(,req . ,cell) (gptel-fleet-test--pending "Edit" f "(b)" "(c)" nil)))
        (with-current-buffer (gptel-fleet--edit-args req)
          (should (equal gptel-fleet--edit-arg "new_str"))
          (should (derived-mode-p 'emacs-lisp-mode))
          (erase-buffer) (insert "(d)")
          (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "old_str")))
            (gptel-fleet-edit-other-arg)))
        (with-current-buffer (get-buffer "*gptel-fleet edit: Edit old_str*")
          (should-not (get-buffer "*gptel-fleet edit: Edit new_str*"))
          (should (equal gptel-fleet--edit-arg "old_str"))
          (should (equal (buffer-string) "(b)"))
          (gptel-fleet-edit-submit))
        (gptel-fleet-test--wait cell)
        (should (equal (with-temp-buffer (insert-file-contents f) (buffer-string)) "(a)\n(d)\n")))
      (pcase-let ((`(,req . ,_) (gptel-fleet-test--pending "Edit" f nil "--- x\n+++ x\n" t)))
        (with-current-buffer (gptel-fleet--edit-args req)
          (should (derived-mode-p 'diff-mode))
          (gptel-fleet-edit-abort))
        (gptel-fleet-answer req ?n)))))

;;;; Inline (gptel-style) confirmation, end to end

(defmacro gptel-fleet-test--with-mock-llm (&rest body)
  "Run BODY with the mock LLM, `backend', `reqlog', `target' and `chat' bound."
  (declare (indent 0))
  `(let* ((port (gptel-fleet-test--free-port))
          (reqlog (expand-file-name "requests.jsonl" gptel-fleet-test--dir))
          (target (make-temp-file "fleet-target" nil ".txt" "SECRET-CONTENT-42\n"))
          (server (start-process "mock-llm" "*mock-llm*" "python3"
                                 (expand-file-name "mock_llm.py" gptel-fleet-test--test-dir)
                                 (number-to-string port) reqlog target))
          (backend (gptel-make-openai "FleetMock" :protocol "http"
                                      :host (format "127.0.0.1:%d" port)
                                      :key "x" :stream nil :models '(big small)))
          (gptel-use-curl t)
          (process-environment (append '("no_proxy=127.0.0.1,localhost"
                                         "NO_PROXY=127.0.0.1,localhost")
                                       process-environment))
          (chat (get-buffer-create "*fleet-e2e*")))
     (unwind-protect
         (progn
           (sleep-for 0.7)
           ,@body)
       (delete-process server)
       (kill-buffer chat))))

(defvar gptel-fleet-test--test-dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defun gptel-fleet-test--start-chat (chat backend)
  (with-current-buffer chat
    (erase-buffer)
    (text-mode)
    (gptel--apply-preset 'gptel-agent (lambda (sym val) (set (make-local-variable sym) val)))
    (setq-local gptel--preset 'gptel-agent)
    (setq-local gptel-backend backend gptel-model 'big gptel-stream nil
                gptel-include-reasoning nil)
    (gptel-mode 1)
    (insert "Please investigate.")
    (gptel-send)))

(defun gptel-fleet-test--wait-for (pred &optional secs)
  (let ((deadline (+ (float-time) (or secs 30))))
    (while (and (< (float-time) deadline) (not (funcall pred)))
      (accept-process-output nil 0.1))
    (funcall pred)))

(ert-deftest gptel-fleet-test-inline-confirm ()
  (gptel-fleet-test--with-env
    (let ((gptel-fleet-approval-ui 'inline))
      (gptel-fleet-test--with-mock-llm
        (gptel-fleet-config-set "researcher" :backend "FleetMock")
        (gptel-fleet-config-set "researcher" :model 'small)
        (gptel-fleet-config-set "researcher" :tools '("Read" "Bash"))
        (gptel-fleet-config-set "researcher" :policy '((confirm :tool "Read")))
        (gptel-fleet-config-set "gptel-agent" :policy '((allow :package orchestration)))
        (gptel-fleet-apply t)
        (gptel-fleet-test--start-chat chat backend)
        ;; The subagent's Read waits inline (gptel overlay), nothing is queued.
        (should (gptel-fleet-test--wait-for (lambda () (gptel-fleet--inline-pending))))
        (should (null gptel-fleet--queue))
        (gptel-fleet-update-indicator)
        (should (= gptel-fleet--pending-count 1))
        (should (string-match-p "Fleet:1" (eval (cadr gptel-fleet-mode-line) t)))
        (let ((entry (car (gptel-fleet--queue-entries))))
          (should (eq (car (car entry)) 'inline))
          (should (equal (aref (cadr entry) 1) "researcher"))   ; right agent
          (should (equal (aref (cadr entry) 2) "Read"))
          ;; Approve from the queue buffer, as `y' would.
          (gptel-fleet--answer-inline (car entry) ?y))
        (should (gptel-fleet-test--wait-for
                 (lambda () (with-current-buffer chat
                              (save-excursion (goto-char (point-min))
                                              (search-forward "MAIN DONE" nil t))))))
        (should (= 0 (progn (gptel-fleet-update-indicator) gptel-fleet--pending-count)))
        ;; The approved call ran as the researcher, and the log says how.
        (let* ((child (cl-find "researcher" (gptel-fleet-log-index)
                               :key (lambda (e) (alist-get 'agent e)) :test #'equal))
               (evs (gptel-fleet--read-jsonl (alist-get 'file child))))
          (should (cl-find-if (lambda (e) (and (equal (alist-get 'ev e) "decision")
                                               (equal (alist-get 'tool e) "Read")
                                               (string-match-p "approved inline"
                                                               (alist-get 'level e))))
                              evs))
          (should (cl-find-if (lambda (e) (and (equal (alist-get 'ev e) "tool-result")
                                               (string-match-p "SECRET-CONTENT-42"
                                                               (format "%s" (alist-get 'result e)))))
                              evs)))
        (should (= 0 (hash-table-count gptel-fleet--tokens)))))))

(ert-deftest gptel-fleet-test-inline-reject-clears-token ()
  (gptel-fleet-test--with-env
    (let ((gptel-fleet-approval-ui 'inline))
      (gptel-fleet-test--with-mock-llm
        (gptel-fleet-config-set "researcher" :backend "FleetMock")
        (gptel-fleet-config-set "researcher" :tools '("Read" "Bash"))
        (gptel-fleet-config-set "researcher" :policy '((confirm :tool "Read")))
        (gptel-fleet-config-set "gptel-agent" :policy '((allow :package orchestration)))
        (gptel-fleet-apply t)
        (gptel-fleet-test--start-chat chat backend)
        (should (gptel-fleet-test--wait-for (lambda () (gptel-fleet--inline-pending))))
        (should (= 1 (hash-table-count gptel-fleet--tokens)))
        (gptel-fleet--answer-inline (car (car (gptel-fleet--queue-entries))) ?n)
        (should (= 0 (hash-table-count gptel-fleet--tokens)))
        (should (null (gptel-fleet--inline-pending)))))))

;;;; Registering a newly created skill without restarting

(ert-deftest gptel-fleet-test-register-skill ()
  (gptel-fleet-test--with-skills
    (gptel-fleet-test--with-env
      (gptel-fleet-config-set "researcher" :skills '("g-*"))
      (gptel-fleet-config-set "executor" :skills '("alpha"))
      (gptel-fleet-apply t)
      ;; A skill appears on disk (e.g. written by an agent's skill-creator skill).
      (gptel-fleet-test--make-skills skroot '("brand-new"))
      (should-not (member "brand-new" (gptel-fleet-skill-names)))
      (let ((report (gptel-fleet-register-skill (expand-file-name "brand-new" skroot)
                                                '("executor"))))
        (should (string-match-p "brand-new registered" report)))
      (should (member "brand-new" (gptel-fleet-skill-names)))
      (should (equal (gptel-fleet-config-get "executor" :skills) '("alpha" "brand-new")))
      (should (equal (gptel-fleet-config-get "researcher" :skills) '("g-*")))   ; untouched
      (should (string-match-p "<name>brand-new</name>"
                              (plist-get (cdr (assoc "gptel-agent" gptel-agent--agents)) :system)))
      (should (string-match-p "<name>brand-new</name>"
                              (plist-get (cdr (assoc "executor" gptel-agent--agents)) :system)))
      (should-not (string-match-p "<name>brand-new</name>"
                                  (plist-get (cdr (assoc "researcher" gptel-agent--agents)) :system)))
      ;; Problems are caught before anything changes.
      (let ((noname (expand-file-name "noname" skroot)))
        (make-directory noname)
        (with-temp-file (expand-file-name "SKILL.md" noname)
          (insert "---\ndescription: x\n---\nbody\n"))
        (should (string-match-p "missing \"name:\""
                                (car (cdr (gptel-fleet-validate-skill noname)))))
        (should-error (gptel-fleet-register-skill noname)))
      (let ((outside (make-temp-file "outside-skills" t)))
        (gptel-fleet-test--make-skills outside '("elsewhere"))
        (should (string-match-p "not inside"
                                (cadr (should-error (gptel-fleet-register-skill
                                                     (expand-file-name "elsewhere" outside))))))))))

(ert-deftest gptel-fleet-test-register-skill-by-agent ()
  (gptel-fleet-test--with-skills
    (gptel-fleet-test--with-env
      (gptel-fleet-config-set "executor" :skills '("alpha"))
      (gptel-fleet-apply t)
      (gptel-fleet-test--make-skills skroot '("agent-made"))
      ;; Through the Fleet tool, as the main agent: queued, not applied.
      (let ((res (gptel-fleet-test--call "FleetRegisterSkill"
                                         (expand-file-name "agent-made" skroot) "(\"executor\")")))
        (should (string-prefix-p "PENDING" res))
        (should-not (member "agent-made" (gptel-fleet-skill-names)))
        (let ((req (car gptel-fleet--queue)))
          (should (string-match-p "Register skill" (gptel-fleet-request-detail req)))
          (should (string-match-p "WIDENS" (gptel-fleet-request-detail req)))
          (gptel-fleet-answer req ?y)))
      (should (member "agent-made" (gptel-fleet-skill-names)))
      (should (member "agent-made" (gptel-fleet-config-get "executor" :skills)))
      ;; An agent may register a skill, but not assign it to itself.
      (gptel-fleet-test--make-skills skroot '("self-serve"))
      (let ((gptel-fleet--acting (list :agent "executor" :run "r9")))
        (should (string-prefix-p "REFUSED"
                                 (gptel-fleet-api-register-skill
                                  (expand-file-name "self-serve" skroot) '("executor"))))
        (should (string-prefix-p "PENDING"
                                 (gptel-fleet-api-register-skill
                                  (expand-file-name "self-serve" skroot))))))))

(provide 'gptel-fleet-test)
;;; gptel-fleet-test.el ends here
