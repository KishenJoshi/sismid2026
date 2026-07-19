#!/usr/bin/env bash
# Fetch THIS student's Codex credential from the class broker and install it.
#
#   bash scripts/codex-login.sh http://HOST:PORT      # server URL from instructor
#   SISMID_CODEX_SERVER=http://HOST:PORT bash scripts/codex-login.sh
#
# You give your EMAIL and the class PASSCODE. The server assigns you one credential
# and remembers it by your email, so if your Codespace restarts (or you make a new
# one) and you enter the same email, you get the SAME credential back. After it runs,
# just launch `codex`.
set -euo pipefail

SERVER="${1:-${SISMID_CODEX_SERVER:-}}"
if [ -z "$SERVER" ]; then
  echo "Server URL not set." >&2
  echo "  bash scripts/codex-login.sh http://HOST:PORT" >&2
  exit 1
fi
SERVER="${SERVER%/}"

CODEX_HOME="${CODEX_HOME:-$HOME/.codex}"
mkdir -p "$CODEX_HOME"; chmod 700 "$CODEX_HOME" 2>/dev/null || true
AUTH="$CODEX_HOME/auth.json"

# Never clobber an auth.json that Codex may have refreshed (OpenAI's guidance).
if [ -s "$AUTH" ]; then
  echo "Codex already has $AUTH; leaving it in place."
  echo "(Delete it and re-run only if the instructor tells you to.)"
  exit 0
fi

# Read email (visible) and passcode (hidden), from the tty if available.
if { : < /dev/tty; } 2>/dev/null; then _tty=/dev/tty; else _tty=/dev/stdin; fi
printf 'Your email: '
IFS= read -r EMAIL < "$_tty"
printf 'Class passcode (input hidden): '
IFS= read -rs PASS < "$_tty"; echo
case "$EMAIL" in
  *@*.*) : ;;
  *) echo "That does not look like an email address." >&2; exit 1 ;;
esac
[ -n "$PASS" ] || { echo "No passcode entered." >&2; exit 1; }

TMP="$(mktemp)"
code="$(curl -fsS -o "$TMP" -w '%{http_code}' \
          -H "X-Passcode: $PASS" -H "X-Email: $EMAIL" \
          "$SERVER/claim" 2>/dev/null || true)"
unset PASS

case "$code" in
  200)
    if head -c 1 "$TMP" | grep -q '{'; then
      install -m 600 "$TMP" "$AUTH"; rm -f "$TMP"
      echo "Installed your Codex credential to $AUTH"
      echo "Start Codex with:  codex"
    else
      rm -f "$TMP"; echo "Unexpected response from server (not JSON)." >&2; exit 1
    fi ;;
  400) rm -f "$TMP"; echo "Server rejected the email as invalid." >&2; exit 1 ;;
  403) rm -f "$TMP"; echo "Rejected: wrong passcode, or your email is not on the class roster." >&2; exit 1 ;;
  409) rm -f "$TMP"; echo "No credentials left in the pool. Tell the instructor." >&2; exit 1 ;;
  000|"") rm -f "$TMP"; echo "Could not reach the server at $SERVER (network/firewall?)." >&2; exit 1 ;;
  *)   rm -f "$TMP"; echo "Server error (HTTP $code)." >&2; exit 1 ;;
esac
