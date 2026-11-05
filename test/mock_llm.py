#!/usr/bin/env python3
"""Deterministic OpenAI-compatible mock LLM for gptel-fleet integration tests.

Behaviour (non-streaming /v1/chat/completions).  The first user message
selects a scenario: "scenario:bash" (main calls Bash), "scenario:fleet"
(main calls FleetSet), anything else (delegation below):
  * Requests offering the "Agent" tool are the MAIN agent:
      - last message from user  -> tool call Agent(researcher, read TARGET)
      - last message is a tool  -> final text "MAIN DONE: <tool result head>"
  * Other requests are the SUBAGENT:
      - last message from user  -> tool call Read(TARGET), then Bash(echo hi)
      - after both tool results -> final text "SUB REPORT: <read result head>"
Every request is appended as a JSON line to the file given as argv[2],
so tests can assert which model served which agent.
"""
import json
import sys
from http.server import BaseHTTPRequestHandler, HTTPServer

PORT = int(sys.argv[1])
REQLOG = sys.argv[2]
TARGET = sys.argv[3]


def tool_call(name, args, cid):
    return {"id": cid, "type": "function",
            "function": {"name": name, "arguments": json.dumps(args)}}


def reply(model, content=None, calls=None):
    msg = {"role": "assistant", "content": content}
    if calls:
        msg["tool_calls"] = calls
    return {"id": "mock", "object": "chat.completion", "model": model,
            "choices": [{"index": 0, "message": msg,
                         "finish_reason": "tool_calls" if calls else "stop"}],
            "usage": {"prompt_tokens": 1, "completion_tokens": 1, "total_tokens": 2}}


class H(BaseHTTPRequestHandler):
    def log_message(self, *a):
        pass

    def do_POST(self):
        body = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
        tools = [t["function"]["name"] for t in body.get("tools", [])]
        msgs = body["messages"]
        main = "Agent" in tools
        tool_msgs = [m for m in msgs if m["role"] == "tool"]
        with open(REQLOG, "a") as f:
            f.write(json.dumps({"model": body["model"], "main": main,
                                "n_tool_msgs": len(tool_msgs)}) + "\n")
        last = msgs[-1]
        first_user = next((str(m.get("content")) for m in msgs if m["role"] == "user"), "")
        if main and "scenario:bash" in first_user:
            if last["role"] == "tool":
                out = reply(body["model"], "MAIN DONE: " + str(last["content"])[:240])
            else:
                out = reply(body["model"], None, [tool_call(
                    "Bash", {"command": "echo WRONG-COMMAND"}, "call_bash_1")])
        elif main and "scenario:fleet" in first_user:
            if last["role"] == "tool":
                out = reply(body["model"], "MAIN DONE: " + str(last["content"])[:80])
            else:
                out = reply(body["model"], None, [tool_call(
                    "FleetSet", {"agent": "researcher", "key": ":profile",
                                 "value": "readonly"}, "call_fleet_1")])
        elif main:
            if last["role"] == "tool":
                out = reply(body["model"], "MAIN DONE: " + str(last["content"])[:60])
            else:
                out = reply(body["model"], None, [tool_call(
                    "Agent", {"subagent_type": "researcher",
                              "description": "read the target",
                              "prompt": "Read " + TARGET}, "call_main_1")])
        else:
            if len(tool_msgs) == 0:
                out = reply(body["model"], None, [tool_call(
                    "Read", {"file_path": TARGET}, "call_sub_1")])
            elif len(tool_msgs) == 1:
                out = reply(body["model"], None, [tool_call(
                    "Bash", {"command": "echo hi && git push origin main"}, "call_sub_2")])
            else:
                out = reply(body["model"],
                            "SUB REPORT: " + str(tool_msgs[0]["content"])[:40])
        data = json.dumps(out).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)


HTTPServer(("127.0.0.1", PORT), H).serve_forever()
