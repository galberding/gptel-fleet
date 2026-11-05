;;; cargo.el --- example gptel-fleet tool package  -*- lexical-binding: t; -*-
;; Copy to ~/.emacs.d/gptel-fleet/tools/ (or `gptel-fleet-tool-directories').

(gptel-fleet-define-package cargo
  :description "Rust builds, tests and lints via cargo (no shell)"
  :level confirm)

(gptel-fleet-define-tool cargo-test
  :package cargo
  :name "CargoTest"
  :description "Run `cargo test' in the project.  Prefer this over Bash for \
running Rust tests.  Output is the last 200 lines."
  :args ((:name "package" :type string :optional t
          :description "Crate to test (cargo -p)")
         (:name "filter" :type string :optional t
          :description "Only run tests whose name contains this"))
  :level notify
  :command ("cargo" "test" (:opt "-p" package) "--" filter)
  :cwd project
  :timeout 900
  :tail 200
  :inherit-env nil
  :env ("CARGO_TERM_COLOR=never")
  :examples ((:package nil :filter nil)))

(gptel-fleet-define-tool cargo-clippy
  :package cargo
  :name "CargoClippy"
  :description "Run clippy with warnings as errors.  Read-only; use after edits."
  :args ((:name "package" :type string :optional t :description "Crate (-p)"))
  :level allow
  :command ("cargo" "clippy" (:opt "-p" package) "--all-targets" "--" "-D" "warnings")
  :cwd project
  :timeout 900
  :tail 150
  :sandbox t
  :examples ((:package nil)))

(gptel-fleet-define-tool git-status
  :package cargo
  :name "GitStatus"
  :description "Short git status of the project."
  :args nil
  :level allow
  :command ("git" "status" "--short" "--branch")
  :cwd project
  :tail 100)
