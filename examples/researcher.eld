;; -*- mode: lisp-data; coding: utf-8 -*-
;; Example override: ~/.emacs.d/gptel-fleet/agents/researcher.eld
;; Research runs on the local model; web is allowed, nothing may write.
(:enabled t
 :backend "llama-server"
 :model qwen3-30b-a3b
 :profile readonly
 ;; First match wins: put denials first.
 :policy ((deny  :tool "Read" :arg :file_path :match "\\.\\(env\\|pem\\|key\\)\\'")
          (deny  :tool "Read" :arg :file_path :under "~/.ssh")
          (allow :package (fs-read web)))
 :log t
 :notes "Cheap worker for web + code lookup.  Orchestrator stays on Opus.")
