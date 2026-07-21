#!/usr/bin/env bash
# Encrypt the Google Trends API key into secrets/gt-api/GT_API.enc so it can be
# committed safely.
#
# Run this on YOUR machine (the instructor's). It never prints the key, never
# writes plaintext to disk, and never passes the key or passcode on a command
# line (so neither can leak via `ps`). Only the encrypted, base64-armored .enc
# file is written, and that is the only thing safe to commit.
#
# Usage:
#   scripts/gt-api-encrypt.sh
#
# Students unlock with:  source scripts/unlock-gt-api-key.sh
#
# This key is kept separate from the agent tokens in secrets/*.enc on purpose:
# different secret, different passcode. scripts/claude-login.sh does not touch it.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
OUT_DIR="$REPO_ROOT/secrets/gt-api"
OUT="$OUT_DIR/GT_API.enc"
mkdir -p "$OUT_DIR"

# Prefer /dev/tty (hidden, immune to a piped stdin); fall back to stdin if no tty.
if { : < /dev/tty; } 2>/dev/null; then TTY=/dev/tty; else TTY=/dev/stdin; fi

printf 'Paste the Google Trends API key (input hidden): ' >&2
IFS= read -rs KEY_VALUE < "$TTY"; echo >&2
if [[ -z "$KEY_VALUE" ]]; then echo "Empty value, aborting." >&2; exit 1; fi

printf 'Google Trends passcode (input hidden): ' >&2
IFS= read -rs PASSPHRASE < "$TTY"; echo >&2
printf 'Confirm passcode: ' >&2
IFS= read -rs PASSPHRASE2 < "$TTY"; echo >&2
if [[ "$PASSPHRASE" != "$PASSPHRASE2" ]]; then echo "Passcodes do not match, aborting." >&2; exit 1; fi
if [[ -z "$PASSPHRASE" ]]; then echo "Empty passcode, aborting." >&2; exit 1; fi

# AES-256-CBC, PBKDF2 (SHA-256, 600k iterations), random salt, base64 armor.
# Key goes in via stdin (printf is a builtin -> not visible in `ps`);
# passcode goes in via env (also not on any command line).
export SECRET_ENC_PASS="$PASSPHRASE"
if printf '%s' "$KEY_VALUE" \
  | openssl enc -aes-256-cbc -md sha256 -pbkdf2 -iter 600000 -salt -a \
      -pass env:SECRET_ENC_PASS > "$OUT"; then
  unset SECRET_ENC_PASS KEY_VALUE PASSPHRASE PASSPHRASE2
  echo "Wrote $OUT" >&2
  echo "Safe to commit. Students unlock with: source scripts/unlock-gt-api-key.sh" >&2
else
  unset SECRET_ENC_PASS KEY_VALUE PASSPHRASE PASSPHRASE2
  rm -f "$OUT"
  echo "Encryption failed." >&2
  exit 1
fi
