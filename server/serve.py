#!/usr/bin/env python3
"""SISMID 2026 Codex credential broker.

Hands out ONE distinct Codex auth.json per student, gated by a class passcode AND
the student's email. The email is the identity key: the server never gives the same
credential to two different emails (no clash), and a repeat request from the same
email (after a restart, rebuild, or a brand-new Codespace) returns the SAME
credential instead of draining the pool. state.json doubles as your roster.

Credentials are plain auth.json files named auth-01.json, auth-02.json, ... in
CRED_DIR. Generate them yourself with `codex login --device-auth` (once per file,
each from a device authorization) and copy them into CRED_DIR.

Config via environment:
  SISMID_PASSCODE   required. The class passcode students type.
  SISMID_CRED_DIR   dir with auth-*.json           (default: ./creds)
  SISMID_STATE      claims/roster file             (default: ./state.json)
  SISMID_PORT       listen port                    (default: 8080)
  SISMID_LOG        append-only log file           (default: ./broker.log)
  SISMID_ALLOW      optional file of allowed emails (one per line). If set, only
                    those emails may claim; otherwise any valid email is accepted.

Endpoints (passcode and email are sent as HTTP headers, never in the URL):
  GET /claim     headers X-Passcode, X-Email -> that email's auth.json bytes
  GET /status    header  X-Passcode          -> JSON: totals + email->cred roster
  GET /healthz                                -> "ok"
"""
import glob
import hmac
import json
import os
import sys
import threading
import time
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

PASSCODE = os.environ.get("SISMID_PASSCODE", "")
CRED_DIR = os.environ.get("SISMID_CRED_DIR", "creds")
STATE = os.environ.get("SISMID_STATE", "state.json")
PORT = int(os.environ.get("SISMID_PORT", "8080"))
LOG = os.environ.get("SISMID_LOG", "broker.log")

_lock = threading.Lock()
ALLOW = None  # set in main(): None = any valid email, else a set of allowed emails


def _norm_email(s):
    s = (s or "").strip().lower()
    if not s or " " in s or s.count("@") != 1:
        return ""
    local, _, dom = s.partition("@")
    if not local or "." not in dom:
        return ""
    return s


def _load_allow():
    p = os.environ.get("SISMID_ALLOW", "")
    if not p:
        return None
    if not os.path.exists(p):
        sys.exit("SISMID_ALLOW=%s does not exist" % p)
    allowed = set()
    with open(p) as f:
        for line in f:
            e = _norm_email(line)
            if e:
                allowed.add(e)
    return allowed


def _creds():
    return sorted(os.path.basename(p) for p in glob.glob(os.path.join(CRED_DIR, "auth-*.json")))


def _load_state():
    try:
        with open(STATE) as f:
            return json.load(f)
    except Exception:
        return {"assign": {}}  # email -> credential filename


def _save_state(st):
    tmp = STATE + ".tmp"
    with open(tmp, "w") as f:
        json.dump(st, f, indent=2)
    os.replace(tmp, STATE)  # atomic


def _log(msg):
    line = "%s %s\n" % (time.strftime("%Y-%m-%d %H:%M:%S"), msg)
    try:
        with open(LOG, "a") as f:
            f.write(line)
    except Exception:
        pass
    sys.stderr.write(line)


def _ok_passcode(pc):
    return bool(PASSCODE) and hmac.compare_digest(pc, PASSCODE)


class Handler(BaseHTTPRequestHandler):
    def log_message(self, *a):  # silence default stderr spam; we log ourselves
        pass

    def _send(self, code, body, ctype="application/json"):
        b = body if isinstance(body, (bytes, bytearray)) else body.encode()
        self.send_response(code)
        self.send_header("Content-Type", ctype)
        self.send_header("Content-Length", str(len(b)))
        self.end_headers()
        try:
            self.wfile.write(b)
        except Exception:
            pass

    def do_GET(self):
        path = self.path.split("?", 1)[0]
        pc = self.headers.get("X-Passcode", "")
        email = _norm_email(self.headers.get("X-Email", ""))
        ip = self.client_address[0]

        if path == "/healthz":
            return self._send(200, "ok\n", "text/plain")

        if path == "/status":
            if not _ok_passcode(pc):
                _log("STATUS denied ip=%s" % ip)
                return self._send(403, '{"error":"forbidden"}')
            with _lock:
                st = _load_state()
                creds = _creds()
                assigned = st.get("assign", {})
                used = set(assigned.values())
                body = {
                    "total": len(creds),
                    "claimed": len(assigned),
                    "free": len([c for c in creds if c not in used]),
                    "assignments": assigned,
                }
            return self._send(200, json.dumps(body, indent=2))

        if path == "/claim":
            if not _ok_passcode(pc):
                _log("CLAIM denied (passcode) ip=%s email=%s" % (ip, email or "?"))
                return self._send(403, '{"error":"bad passcode"}')
            if not email:
                return self._send(400, '{"error":"missing or invalid X-Email"}')
            if ALLOW is not None and email not in ALLOW:
                _log("CLAIM denied (not on roster) ip=%s email=%s" % (ip, email))
                return self._send(403, '{"error":"email not on the class roster"}')
            with _lock:
                st = _load_state()
                creds = _creds()
                assigned = st.setdefault("assign", {})
                if email in assigned and assigned[email] in creds:
                    fn = assigned[email]
                    _log("CLAIM repeat ip=%s email=%s -> %s" % (ip, email, fn))
                else:
                    used = set(assigned.values())
                    free = [c for c in creds if c not in used]
                    if not free:
                        _log("CLAIM exhausted ip=%s email=%s" % (ip, email))
                        return self._send(409, '{"error":"no credentials left, ask instructor"}')
                    fn = free[0]
                    assigned[email] = fn
                    _save_state(st)
                    _log("CLAIM new ip=%s email=%s -> %s (%d/%d used)"
                         % (ip, email, fn, len(assigned), len(creds)))
                with open(os.path.join(CRED_DIR, fn), "rb") as f:
                    data = f.read()
            return self._send(200, data)

        return self._send(404, '{"error":"not found"}')


def main():
    global ALLOW
    if not PASSCODE:
        sys.exit("Set SISMID_PASSCODE before starting.")
    if not _creds():
        sys.exit("No auth-*.json found in %s" % CRED_DIR)
    ALLOW = _load_allow()
    _log("start port=%d creds=%d dir=%s allow=%s"
         % (PORT, len(_creds()), CRED_DIR, ("%d emails" % len(ALLOW)) if ALLOW is not None else "any"))
    ThreadingHTTPServer(("0.0.0.0", PORT), Handler).serve_forever()


if __name__ == "__main__":
    main()
