#!/usr/bin/env python3
"""Drive gptel-fleet in a real terminal Emacs (emacs -nw) inside tmux.

Each scenario starts a fresh Emacs with test/interactive/fleet-init.el and
a fresh mock LLM, sends real key sequences with `tmux send-keys`, waits for
text on the screen with `tmux capture-pane`, and saves every screen it
looked at under test/interactive/screens/ so a human can review them.

Usage:  FLEET_LOADPATH=../gptel:../gptel-agent:test/stubs \
        python3 test/interactive/tmux_scenarios.py [scenario ...]
"""
import os
import shutil
import socket
import subprocess
import sys
import tempfile
import time

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
HERE = os.path.dirname(os.path.abspath(__file__))
SCREENS = os.path.join(HERE, "screens")
COLS, ROWS = 200, 45


def free_port():
    s = socket.socket()
    s.bind(("127.0.0.1", 0))
    port = s.getsockname()[1]
    s.close()
    return port


class Session:
    def __init__(self, name, scenario="delegate", ui="inline"):
        self.name = name
        self.tmp = tempfile.mkdtemp(prefix="fleet-tmux-")
        self.port = free_port()
        self.shot = 0
        self.findings = []
        env = dict(os.environ,
                   FLEET_ROOT=ROOT, FLEET_DIR=self.tmp, FLEET_PORT=str(self.port),
                   FLEET_SCENARIO=scenario, FLEET_UI=ui,
                   no_proxy="127.0.0.1,localhost", NO_PROXY="127.0.0.1,localhost")
        target = os.path.join(self.tmp, "target.txt")
        self.mock = subprocess.Popen(
            [sys.executable, os.path.join(ROOT, "test", "mock_llm.py"),
             str(self.port), os.path.join(self.tmp, "requests.jsonl"), target],
            env=env)
        time.sleep(0.5)
        self.tmux("kill-session", "-t", name, check=False)
        cmd = (f"emacs -nw -Q -l {os.path.join(HERE, 'fleet-init.el')}")
        envargs = []
        for k in ("FLEET_ROOT", "FLEET_DIR", "FLEET_PORT", "FLEET_SCENARIO", "FLEET_UI",
                  "FLEET_LOADPATH", "no_proxy", "NO_PROXY"):
            envargs += ["-e", f"{k}={env.get(k, '')}"]
        self.tmux("new-session", "-d", "-s", name, "-x", str(COLS), "-y", str(ROWS),
                  *envargs, cmd)
        self.wait_for("fleet test session ready", 30)

    def tmux(self, *args, check=True):
        return subprocess.run(["tmux", *args], check=check, capture_output=True, text=True)

    def screen(self):
        return self.tmux("capture-pane", "-p", "-t", self.name).stdout

    def snap(self, label):
        self.shot += 1
        os.makedirs(SCREENS, exist_ok=True)
        path = os.path.join(SCREENS, f"{self.name}-{self.shot:02d}-{label}.txt")
        with open(path, "w") as f:
            f.write(self.screen())
        return path

    def keys(self, *keys, pause=0.4):
        for k in keys:
            self.tmux("send-keys", "-t", self.name, k)
            time.sleep(0.15)
        time.sleep(pause)

    def type(self, text, pause=0.3):
        self.tmux("send-keys", "-t", self.name, "-l", text)
        time.sleep(pause)

    def mx(self, command):
        self.keys("M-x")
        self.type(command)
        self.keys("Enter", pause=0.8)

    def wait_for(self, text, timeout=20):
        end = time.time() + timeout
        while time.time() < end:
            if text in self.screen():
                return True
            time.sleep(0.25)
        self.snap("TIMEOUT-" + "".join(c for c in text if c.isalnum())[:20])
        raise AssertionError(f"[{self.name}] '{text}' not on screen after {timeout}s")

    def expect(self, text, present=True):
        scr = self.screen()
        if (text in scr) != present:
            self.snap("FAILED-expect")
            raise AssertionError(f"[{self.name}] expected '{text}' "
                                 f"{'on' if present else 'NOT on'} screen")

    def note(self, finding):
        """Record a UX observation (not a failure)."""
        self.findings.append(finding)

    def window_count(self):
        """Count Emacs windows by counting mode lines ('-UUU' or ' U:' markers)."""
        scr = self.screen().splitlines()
        return sum(1 for line in scr if line.startswith("-UUU") or line.startswith(" U:")
                   or line.startswith("-U:") or line.startswith("-:"))

    def close(self):
        self.tmux("kill-session", "-t", self.name, check=False)
        self.mock.terminate()
        shutil.rmtree(self.tmp, ignore_errors=True)


# ---------------------------------------------------------------------------
# Scenarios

def scenario_inline_ack():
    """Subagent Read waits inline in the chat; approve with gptel's C-c C-c."""
    s = Session("inline-ack", "delegate", "inline")
    try:
        windows_before = s.window_count()
        s.keys("F5")
        s.wait_for("Run tools")
        s.snap("waiting-inline")
        s.expect("Fleet:1")
        s.expect("*gptel-fleet", present=False)          # nothing popped up
        if s.window_count() != windows_before:
            s.note("window layout changed while a call was waiting")
        # Press C-c C-c where point is right after sending.
        s.keys("C-c", "C-c", pause=1.0)
        s.snap("after-C-c-C-c-at-point")
        if "Run tools" in s.screen():
            s.note("C-c C-c at point after sending did not approve: point is outside "
                   "gptel's tool overlay; the user must move into the overlay first")
            s.keys("M-<")                                  # beginning of buffer
            s.keys("C-s"); s.type("Run tools"); s.keys("Enter")
            s.keys("C-c", "C-c", pause=1.0)
        s.wait_for("MAIN DONE", 30)
        s.snap("done")
        s.expect("Fleet:1", present=False)
    finally:
        s.close()
    return s


def scenario_queue_ack():
    """Same waiting call, approved from the queue buffer with y."""
    s = Session("queue-ack", "delegate", "inline")
    try:
        s.keys("F5")
        s.wait_for("Run tools")
        s.mx("gptel-fleet-approvals")
        s.wait_for("Fleet-Approvals")
        s.snap("queue")
        s.expect("researcher")
        s.expect("inline *fleet-chat*")
        s.keys("y", pause=1.0)
        s.wait_for("MAIN DONE", 30)
        s.snap("done")
    finally:
        s.close()
    return s


def scenario_edit_bash():
    """Bash call waits in the queue; edit it in sh-mode, submit with C-c '."""
    s = Session("edit-bash", "bash", "queue")
    try:
        s.keys("F5")
        s.wait_for("Fleet:1")
        s.snap("waiting-mode-line")
        s.expect("*gptel-fleet", present=False)
        s.mx("gptel-fleet-approvals")
        s.wait_for("echo WRONG-COMMAND")
        s.snap("queue")
        s.keys("e", pause=1.0)
        s.wait_for("submit")
        s.snap("edit-buffer")
        scr = s.screen()
        if "Shell-script" not in scr and "Bash" not in scr and "bash" not in scr:
            s.note("edit buffer mode line does not show a shell mode")
        s.keys("C-a", "C-k")
        s.type("echo RIGHT-COMMAND")
        s.keys("C-c", "'", pause=1.5)
        s.snap("after-submit")
        s.keys("C-x", "b"); s.type("*fleet-chat*"); s.keys("Enter")
        s.wait_for("MAIN DONE", 30)
        s.snap("done")
        s.expect("MAIN DONE")
        s.expect("RIGHT-COMMAND")
        # The chat's tool header still shows the model's original call; the
        # result must say that the user changed it.
        s.expect("user edited this call")
        if "approvals*" in s.screen():
            s.note("queue window still open after the last call was answered")
    finally:
        s.close()
    return s


def scenario_agent_list():
    """Agent list: change a subagent's model and tools interactively."""
    s = Session("agent-list", "delegate", "inline")
    try:
        s.mx("gptel-fleet-list-agents")
        s.wait_for("Fleet-Agents")
        s.snap("list")
        s.keys("C-s"); s.type("researcher"); s.keys("Enter")
        s.keys("C-a")
        s.keys("M", pause=0.8)
        s.snap("model-prompt")
        s.type("FleetMock:big"); s.keys("Enter", pause=1.0)
        s.wait_for("FleetMock")
        s.snap("after-model")
        s.expect("big")
        s.keys("t", pause=1.0)
        s.wait_for("Fleet-Assign")
        s.snap("tools-checklist")
        s.keys("C-s"); s.type("Grep"); s.keys("Enter", "C-a")
        s.keys("m", pause=0.5)
        s.keys("C-c", "C-c", pause=1.0)
        s.wait_for("Fleet-Agents")
        s.snap("after-tools")
    finally:
        s.close()
    return s


def scenario_fleet_api():
    """Main agent asks to change another agent; the change waits for approval."""
    s = Session("fleet-api", "fleet", "inline")
    try:
        s.keys("F5")
        s.wait_for("MAIN DONE", 30)
        s.snap("agent-got-pending")
        s.expect("PENDING")
        s.expect("Fleet:1")
        s.mx("gptel-fleet-approvals")
        s.wait_for("fleet-change")
        s.snap("queue")
        s.keys("Enter", pause=1.0)
        s.wait_for("Requested by")
        s.snap("detail")
        s.expect("agents/researcher.eld")
        if "-;; " in s.screen():
            s.note("diff shows header comments being deleted although the write keeps them")
        s.keys("y", pause=1.0)
        s.snap("after-approve")
        s.mx("gptel-fleet-list-agents")
        s.wait_for("Fleet-Agents")
        s.snap("list-after")
        row = next((l for l in s.screen().splitlines() if "researcher " in l and "FleetMock" in l), "")
        if "readonly" not in row:
            s.snap("FAILED-profile")
            raise AssertionError(f"researcher row lacks profile readonly: {row!r}")
        s.expect("*gptel-fleet call*", present=False)   # detail view closed on approve
    finally:
        s.close()
    return s


SCENARIOS = {
    "inline-ack": scenario_inline_ack,
    "queue-ack": scenario_queue_ack,
    "edit-bash": scenario_edit_bash,
    "agent-list": scenario_agent_list,
    "fleet-api": scenario_fleet_api,
}


def main(argv):
    names = argv or list(SCENARIOS)
    failed, findings = [], []
    for n in names:
        try:
            s = SCENARIOS[n]()
            findings += [f"{n}: {f}" for f in s.findings]
            print(f"PASS {n}")
        except Exception as e:  # noqa: BLE001
            failed.append(n)
            print(f"FAIL {n}: {e}")
    if findings:
        print("\nUX findings:")
        for f in findings:
            print(" -", f)
    print(f"\n{len(names) - len(failed)}/{len(names)} scenarios passed; screens in {SCREENS}")
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
