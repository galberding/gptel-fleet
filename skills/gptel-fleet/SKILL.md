---
name: gptel-fleet
description: How to work inside a gptel-fleet managed agent fleet — understand approval levels and tool-call outcomes (denied, escalate, dry-run, waiting for approval), inspect agents/tools/rules with the Fleet* tools, and request changes (models, tools, packages, skills, policy rules, new agents, command tools, skill registration) that a human approves. Use whenever a tool call was denied/escalated/not executed, when asked to change, create or inspect agents, tools, skills or permissions, or after writing a new skill that should be usable without restarting Emacs.
---

# gptel-fleet: working in a managed agent fleet

You are running inside Emacs under **gptel-agent**, and **gptel-fleet** sits
between you and every tool. It decides, per call, whether the call runs,
waits for the human, or is refused. It also lets you inspect the fleet and
*request* changes to it. You can never apply a change yourself: every
change you request waits for the human's approval.

This file is the complete reference. Read the section you need:

1. How tool calls are decided, and what each result means for you
2. Inspecting the fleet (read-only, always allowed)
3. Requesting changes (all approval-gated)
4. Rules: syntax, resolution order, profiles, packages
5. Defining command tools
6. Registering a skill you just wrote
7. What is refused outright, and why
8. Recipes
9. Troubleshooting

---

## 1. How every tool call is decided

Each call gets one **level**, from strictest to loosest:

| Level      | Runs? | What you get back |
|------------|-------|-------------------|
| `deny`     | no    | `Error: tool call X was denied for agent "A" by policy (RULE). Do not retry the same call; ...` |
| `escalate` | no    | `Not executed: agent "A" may not call X on its own (escalate). ...` plus instructions to add an `<escalation>` block (see below) |
| `dry-run`  | no    | `[dry-run] X would have been called with ARGS. It was NOT executed; ...` |
| `confirm`  | after the human answers | The call waits. You get the real result once it is approved, or a denial (possibly with a reason from the human) |
| `notify`   | yes   | normal result; the human sees an echo-area note |
| `allow`    | yes   | normal result |

Every call is logged, whatever its level.

### How to react to each result

- **Denied.** Do **not** retry the same call, and do not try to reach the
  same effect through another tool. For example, don't use `Bash` to write a
  file after `Write` was denied, and don't use `Eval` to run a shell command
  after `Bash` was denied. Doing so is circumventing policy, and the side-door
  rules (section 4) will usually catch it anyway. Choose another approach,
  or say in your answer what you would need and why. If you think the
  policy is wrong for your task, you may *request* a rule change for
  another agent (section 3), never for yourself.
- **Escalate** (only subagents get this; main agents get `confirm` instead).
  Continue without the call where you can. If the call is essential, finish
  the task and append exactly one block per needed call to your final
  report:

  ```
  <escalation>
  tool: Bash
  args: (:command "git push origin main")
  why: <one sentence>
  </escalation>
  ```

  The orchestrating agent decides what to do with it. Nothing is
  re-executed automatically.
- **Dry-run.** The human is watching what you would do. Plan as if you
  did not have the result. Don't treat it as an error and don't repeat it.
- **Confirm.** Nothing for you to do: the call either returns its result or
  comes back denied. The human may **edit** a call before it runs. Then the
  result starts with
  `[The user edited this call before it ran.  Arguments actually used: ...]`.
  Trust those arguments over the ones you sent.
- **Denied by the human with a reason.** Treat the reason as an
  instruction.

### Who you are

The fleet knows which agent made each call: you, or the subagent you
delegated to through `Agent`. Rules, logs and "self" checks (section 7) use
that identity. Main agents are `gptel-agent` and `gptel-plan`; every other
agent is a subagent. The agent that spawned a subagent is its **parent**.

---

## 2. Inspecting the fleet (read-only)

These run without approval (package `fleet`, level `allow`). **Call
`FleetOverview` first** whenever you need to reason about the fleet: it
returns the live state, so you don't have to guess.

| Tool | Lisp function | Returns |
|------|---------------|---------|
| `FleetOverview` | `(gptel-fleet-api-overview)` | architecture, level order, matcher keys, override keys, the API, and the live list of agents (state, backend:model, #tools, skills, profile), packages, skills, profiles and backends with their models |
| `FleetAgent` `agent` | `(gptel-fleet-api-agent "researcher")` | one agent: its override plist, effective backend/model/tools/skills, and the level and deciding rule for each of its tools |
| `FleetExplain` `agent` `tool` `args` | `(gptel-fleet-api-explain "researcher" "Bash" '(:command "ls"))` | `Bash → deny (profile readonly: ...)`: the level a concrete call would get, and which rule decides it |
| `FleetStatus` `id` | `(gptel-fleet-api-status "chg-...")` | status of a change you requested: `pending`, `applied`, `denied`, `refused` or `failed`, plus the reason if any |

Use `FleetExplain` **before** a call you suspect might be refused, and
**after** a denial, to see which rule fired.

The Lisp functions can also be called through `Eval`. `Eval` has its own
level (usually `confirm`), so the `Fleet*` tools are the cheaper path. Only
`gptel-fleet-api-*` symbols are fine in `Eval`; touching any other
`gptel-fleet-*` symbol from `Eval` triggers a floor rule (section 4).

---

## 3. Requesting changes

Every changing function builds a change, validates it, simulates its
effect, and then does one of the following:

| Result string | Meaning | What you do |
|---------------|---------|-------------|
| `PENDING chg-XXXX: SUMMARY. Queued for human approval ...` | The human will see a diff and decide. **Nothing has changed yet.** | Note the id and continue with other work. Check with `FleetStatus` later, e.g. before you finish your task. Don't poll in a loop, and don't assume it will be approved. |
| `APPLIED chg-XXXX: ...` | Only happens when the *human* calls the function directly. You will not normally see this. | — |
| `REJECTED (invalid): ...` | Malformed request (unknown key, bad level, bad regexp, unknown agent/tool/skill, ...). | Fix it and resubmit. |
| `REFUSED chg-XXXX: ...` | Not allowed at all (section 7). | Do not resubmit in another form. Tell the human what you need. |

The human sees, for each request:

- who asked and through which tool;
- a **⚠ WIDENS** list: anything that gains capability;
- every (agent, tool) level that changes, before → after;
- the unified diff of each agent's override file.

The human cannot "always allow" these requests or edit them; they can only
apply or reject. Applied changes take effect immediately: agents are
re-registered, and live chat buffers are refreshed. Each applied change is
committed to git in the fleet directory with **you** as author.

### Change tools

All arguments are **strings**. Arguments that carry Lisp data are read with
`read`, so write them as Lisp: `t`, `nil`, symbols, `"strings"`, `(lists)`.

| Tool (Lisp function) | Arguments | Notes |
|----------------------|-----------|-------|
| `FleetSet` (`gptel-fleet-api-set agent key value`) | `agent`, `key` (e.g. `":model"` or `"model"`), `value` (Lisp data, or `:unset` to remove the key so the agent inherits again) | Any override key (table below) |
| `FleetAssign` (`gptel-fleet-api-assign agent kind names`) | `kind` ∈ `tools` / `packages` / `skills`; `names` = Lisp list like `("Read" "Grep")`, or `:inherit` | Replaces the whole list |
| `FleetAddRule` (`gptel-fleet-api-add-rule agent rule &optional position`) | `rule` as Lisp, e.g. `(deny :tool "Bash" :arg :command :any-segment "^rm ")`; `position` integer, default 0 (= first, highest priority) | Inserts into the agent's `:policy` |
| — (`gptel-fleet-api-remove-rule agent index`) | 0-based index into `:policy` | Via `Eval` only |
| `FleetNewAgent` (`gptel-fleet-api-new-agent name description system-prompt`) | `name`: lowercase letters, digits, dashes; `description`: one line; `system_prompt` | Created **disabled**, profile `readonly`, tools Read/Grep/Glob. Enabling it is a second, separate request. |
| `FleetDefineTool` (`gptel-fleet-api-define-tool id spec`) | `id` like `cargo-test`; `spec` plist (section 5) | Command tools only |
| `FleetRegisterSkill` (`gptel-fleet-api-register-skill path &optional agents`) | `path` = skill folder or its `SKILL.md`; `agents` = Lisp list | Section 6 |
| — (`gptel-fleet-api-set-tool-level tool level`) | a level symbol, or `nil` to reset | Default level of a tool for everyone; via `Eval` only |

### Override keys (per agent)

| Key | Value | Effect |
|-----|-------|--------|
| `:enabled` | `t` / `nil` | Disabled agents are removed from the fleet and from every agent's list |
| `:delegate` | `t` / `nil` | Whether other agents may delegate to it through `Agent` |
| `:backend` | string, a gptel backend name from `FleetOverview` | Must exist |
| `:model` | symbol, e.g. `qwen3-30b-a3b` | Should be listed for that backend (a warning otherwise) |
| `:tools` | list of tool names | **Replaces** the agent's frontmatter tools |
| `:tool-packages` | list of package symbols, e.g. `(web cargo)` | **Added** on top of `:tools` |
| `:skills` | absent = all skills; `()` = none (also removes the `Skill` tool); list of names and globs like `("pdf" "gitlab-*")` | Hides *and* enforces skills |
| `:profile` | `readonly` / `executor` / `trusted` | Named rule set (section 4) |
| `:policy` | list of rules | The agent's own rules, checked before learned rules and profile |
| `:log` | `t` / `nil` | JSONL logging |
| `:log-dir` | directory string | Dedicated log directory |
| `:preset` | gptel preset plist, e.g. `(:temperature 0.2)` | Merged into the agent |
| `:notes` | string | Free text for humans |

---

## 4. Rules

### Syntax

A rule is `(LEVEL . MATCHER)`. An empty matcher matches everything:
`(allow)` allows every call.

| Matcher key | Value |
|-------------|-------|
| `:tool` | tool name or list of names |
| `:package` | package symbol or list |
| `:agent` | agent name or list (useful in global rules) |
| `:arg KEY` + one test | test one argument, e.g. `:arg :command` |
| `:args ((KEY TEST VALUE) ...)` | several argument tests, all must hold |
| `:pred FN` | function called with `TOOL ARGS AGENT` (humans only; agents can't add code) |

Argument tests:

| Test | Holds when |
|------|------------|
| `:match REGEXP` | the value matches (Emacs regexp syntax: `\\(`, `\\|`, `\\'`, `` \\` ``) |
| `:equal VALUE` | the value is equal |
| `:under DIR` / `:not-under DIR` | the path is (not) inside DIR, after expansion |
| `:any-segment REGEXP` | **any** shell segment matches |
| `:all-segments REGEXP` | **every** shell segment matches |

Shell commands are split into segments on `&&`, `||`, `;`, `|` and
newlines, outside quotes. Heredoc bodies become extra segments.

- **Deny rules should use `:any-segment`**, so that `make && git push` is
  caught.
- **Allow rules should use `:all-segments`**, so that `ls && rm -rf x` is
  *not* allowed by an `ls` rule.

Example policy:

```lisp
((deny  :tool "Read" :arg :file_path :match "\\.\\(env\\|pem\\)\\'")
 (allow :tool "Bash" :arg :command :all-segments "\\`\\(ls\\|git \\(status\\|log\\|diff\\)\\)\\b")
 (confirm :tool "Bash")
 (allow :package (fs-read web)))
```

### Resolution order (first match wins)

1. **Floor rules**, global, and nothing can loosen them:
   - `rm -rf /` → `deny`; `git push` → `escalate` (`confirm` for main agents);
   - side doors → `confirm`: `Write`/`Edit`/`Insert`/`Mkdir`/`Bash` whose
     arguments point into the fleet directory, and `Eval` expressions using
     `gptel-fleet-*` symbols other than `gptel-fleet-api-*`.
2. **Skill gate**: a `Skill` call for a skill outside the agent's `:skills`
   → `deny`.
3. The agent's `:policy`.
4. **Learned rules** (the human's "always allow" answers): this session's
   first, then the saved ones.
5. The agent's `:profile`.
6. Global rules.
7. Tool default level: per-tool setting, then the tool definition, then
   the package level.
8. For tools that existed before fleet: the tool's original confirm setting
   (`confirm` or `allow`).
9. The default level, `confirm`.

### Profiles

| Profile | Rules |
|---------|-------|
| `readonly` | allow `fs-read`, `emacs-introspection`, `web`; deny `fs-write`, `shell`, `emacs` |
| `executor` | allow `fs-read`, `emacs-introspection`; confirm `fs-write`; escalate `shell` |
| `trusted` | allow `fs-read`, `fs-write`, `emacs-introspection`, `orchestration`; notify `web`, `shell` |

### Built-in packages

| Package | Tools |
|---------|-------|
| `fs-read` | Read Glob Grep |
| `fs-write` | Write Edit Insert Mkdir |
| `web` | WebSearch WebFetch (a prompt-injection surface) |
| `shell` | Bash |
| `emacs` | Eval Diagnostics |
| `emacs-introspection` | read-only Emacs introspection tools |
| `orchestration` | Agent TodoWrite Skill |
| `fleet` | the Fleet* tools |

Tools without a package (e.g. from MCP servers) are put in a package named
after their gptel category, usually the MCP server name, such as `gitlab`.
`FleetOverview` lists the real packages.

---

## 5. Defining command tools

When you (or another agent) keep issuing the same shell command, a
dedicated tool is safer and cheaper:

- it runs an **argv list without a shell** (no quoting, no `&&` chains);
- it rejects argument values starting with `-` (option injection) unless
  the argument has `:allow-dash t`;
- it can run with a minimal environment or in a sandbox;
- it gets its own approval level.

`FleetDefineTool` takes an `id` and a `spec` plist:

```lisp
(:package cargo
 :name "CargoTest"
 :description "Run `cargo test' in the project.  Prefer this over Bash for
running Rust tests.  Output is the last 200 lines."
 :args ((:name "package" :type string :optional t :description "Crate to test (cargo -p)")
        (:name "filter"  :type string :optional t :description "Only tests whose name contains this"))
 :level notify
 :command ("cargo" "test" (:opt "-p" package) "--" filter)
 :cwd project
 :timeout 900
 :tail 200
 :inherit-env nil
 :env ("CARGO_TERM_COLOR=never")
 :examples ((:package nil :filter nil)))
```

`:command` template elements:

| Element | Becomes |
|---------|---------|
| `"literal"` | itself |
| `ARG` (a symbol) | the argument's value; dropped if nil |
| `(:opt "-p" ARG)` | `-p VALUE`, or nothing if nil |
| `(:flag "--x" ARG)` | `--x` if ARG is true |
| `(:split ARG)` | the value split into several argv elements |
| `(:fmt "%s=%s" A B)` | one formatted element |

Other keys:

| Key | Meaning |
|-----|---------|
| `:cwd` | `project` / `default` / `"dir"` / an argument name |
| `:timeout` | seconds |
| `:tail` / `:head` / `:max-output` | output trimming |
| `:stderr` | `merge` / `drop` |
| `:env` | list of `"K=V"` strings |
| `:inherit-env nil` | start from a minimal environment (no tokens leak) |
| `:sandbox t` | bubblewrap: read-only root, writable cwd, no network |

Rules for agent-defined tools:

- `:name` must be alphanumeric CamelCase.
- `:package` must be a symbol.
- `:level` must be a valid level.
- Every template symbol must be a declared argument.
- The program must exist.
- `:function` (Lisp code) tools are **refused** from agents.
- Defining or changing a tool is always shown to the human as widening.
- Assigning the new package to an agent is a **separate** request
  (`FleetAssign` with kind `packages`), and you can't request it for
  yourself.

---

## 6. Registering a skill you just wrote

gptel-agent only scans skill folders when it builds its agents. A
`SKILL.md` you just created is invisible until it is registered.

1. Write the skill as `<skill-dir>/<name>/SKILL.md`, inside one of the
   configured skill directories (`gptel-agent-skill-dirs`), with
   frontmatter:

   ```
   ---
   name: <name, same as the folder>
   description: <when to use it>
   ---
   ```

   Writes inside the fleet directory need confirmation; skill directories
   usually aren't there.
2. Call `FleetRegisterSkill` with the folder (or the `SKILL.md`). Optionally
   pass `agents`: other agents with an explicit `:skills` list that should
   get it, e.g. `("researcher")`.
3. You get `PENDING chg-...`. The human sees the full `SKILL.md` and which
   agents will see it.

After approval, agents that inherit all skills see it immediately, and so do
agents whose `:skills` contains a matching glob. A missing `name:` or
`description:`, or a folder outside the skill directories, gives
`REJECTED`. You may register a skill, but you may **not** add it to your
own `:skills` list.

---

## 7. Refused outright

`REFUSED` means the request will never be queued:

- **Widening yourself or your parent agent.** This includes looser levels for
  any tool, added tools/packages/skills, a different model or backend,
  enabling or delegation, and removing a restricting key so defaults are
  inherited. The check runs on the simulated result, so indirect routes,
  such as a global tool level that loosens your own tools, are refused too.
  **Narrowing yourself is allowed**: e.g. adding a deny rule to your own
  policy, or dropping a tool.
- **`:function` tools** from agents.
- **Floor rules**: these are not exposed through the API.

If your own task needs more capability, say so in your answer, with the
exact change (e.g. the `FleetExplain` output and the rule you'd need). The
human can apply it.

---

## 8. Recipes

**Give a subagent a cheaper model**
1. `FleetOverview` → find the backend name and a listed model.
2. `FleetSet` with `agent` = `researcher`, `key` = `:backend`, and raw
   `value` = `"llama-server"` (a Lisp string, so keep the quotes).
3. `FleetSet` with `agent` = `researcher`, `key` = `:model`, and raw
   `value` = `qwen3-30b-a3b` (a symbol, so no quotes).

That is two requests. Both show as "cost/capability change".

*Raw value* means the exact text of the argument. Your tool-call encoding
(JSON) adds its own escaping on top; don't add another layer.

**Make an agent read-only**
`FleetSet` with `agent` = `reviewer`, `key` = `:profile`, `value` = `readonly`. This narrows,
unless the agent previously had looser rules from somewhere else; the diff
will show it.

**Allow safe git commands for one agent, keep everything else asking**

Call `FleetAddRule` with `agent` = `executor`, `position` = `0`, and this as
the raw `rule` argument value:

```lisp
(allow :tool "Bash" :arg :command :all-segments "\\`git \\(status\\|log\\|diff\\)\\b")
```

Then verify with `FleetExplain`, `agent` = `executor`, `tool` = `Bash` and
raw `args`:

```lisp
(:command "git status && rm x")
```

It must **not** say `allow`, because the `rm` segment fails `:all-segments`.

**Create a specialised agent**
1. `FleetNewAgent` with `name` = `gitlab-review`, `description` =
   `Reviews GitLab MRs`, and `system_prompt`.
2. After approval, request the tools: `FleetAssign` with `agent` =
   `gitlab-review`, `kind` = `packages`, `names` = `(gitlab)`.
3. Then enable it: `FleetSet` with `agent` = `gitlab-review`, `key` =
   `:enabled`, `value` = `t`.

Each step is a separate request.

**Turn a repeated Bash command into a tool**
`FleetDefineTool` (section 5), then `FleetAssign` the package to the agents
that should have it (not yourself).

**Find out why something was denied**
`FleetExplain` with the exact args → the deciding rule → `FleetAgent` for
the full picture. Report it. Don't work around it.

---

## 9. Troubleshooting

| Symptom | Cause / fix |
|---------|-------------|
| `REJECTED ... unknown key` | Use only the override keys from section 3 |
| `REJECTED ... backend` | Use a backend name exactly as `FleetOverview` lists it |
| `REJECTED ... invalid regexp` | Use Emacs regexp syntax written as a Lisp string, e.g. `` "\\`git \\(status\\|log\\)\\b" `` (backslashes doubled once, as in Lisp source) |
| `REFUSED ... would loosen X for <you>` | Self-promotion; ask the human instead |
| A change stays `pending` | The human hasn't looked yet. Continue and mention the id in your final answer |
| `denied` with no reason | The human rejected it. Don't resubmit unchanged |
| A call to a fleet file or a `gptel-fleet-*` internal waits for confirmation | Side-door floor rule; use the `Fleet*` tools instead |
| A skill doesn't show up | Not registered yet (section 6), or your `:skills` list excludes it |
| `Not executed ... (escalate)` | You are a subagent; add an `<escalation>` block to your final report |
