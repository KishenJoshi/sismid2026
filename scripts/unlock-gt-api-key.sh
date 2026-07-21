# Unlock the class Google Trends API key into your current shell.
#
# IMPORTANT: run this with `source`, not `bash`, or the variable won't stick:
#
#     source scripts/unlock-gt-api-key.sh
#
# It asks for the Google Trends passcode (given by the instructor in class),
# decrypts secrets/gt-api/GT_API.enc, and exports GT_API. It also appends the
# key to ~/.bashrc so new terminals and a Codespace restart stay unlocked.
#
# This is deliberately SEPARATE from scripts/claude-login.sh (the agent tokens):
# different secret, different passcode, different session. Unlocking one does
# not unlock the other.
#
# NOTE: start Jupyter (or restart the kernel) AFTER running this, or the kernel
# will not have GT_API in its environment.

# Resolve repo root whether sourced from bash or zsh.
if [ -n "${BASH_SOURCE:-}" ]; then
  _gt_src="${BASH_SOURCE[0]}"
elif [ -n "${ZSH_VERSION:-}" ]; then
  _gt_src="${(%):-%N}"
else
  _gt_src="$0"
fi
_GT_ROOT="$(cd "$(dirname "$_gt_src")/.." && pwd)"
_GT_ENC="$_GT_ROOT/secrets/gt-api/GT_API.enc"

_gt_cleanup() { unset _gt_src _GT_ROOT _GT_ENC _GT_PASS _gt_val _gt_ok _gt_cleanup; }

if [ ! -f "$_GT_ENC" ]; then
  echo "No encrypted key found at $_GT_ENC" >&2
  echo "Instructor: create it with  scripts/gt-api-encrypt.sh" >&2
  _gt_cleanup; return 1 2>/dev/null || exit 1
fi

printf 'Google Trends passcode (input hidden): '
if { : < /dev/tty; } 2>/dev/null; then
  IFS= read -rs _GT_PASS < /dev/tty; echo
else
  IFS= read -rs _GT_PASS; echo
fi
if [ -z "$_GT_PASS" ]; then
  echo "No passcode entered." >&2
  _gt_cleanup; return 1 2>/dev/null || exit 1
fi

export SECRET_ENC_PASS="$_GT_PASS"
# Two independent checks. A wrong passcode makes openssl exit non-zero, but it
# still streams the first blocks of GARBAGE to stdout before it fails, so
# "non-empty" alone is not enough: we check the exit status AND the format.
if _gt_val="$(openssl enc -d -aes-256-cbc -md sha256 -pbkdf2 -iter 600000 -a \
                -in "$_GT_ENC" -pass env:SECRET_ENC_PASS 2>/dev/null)"; then
  _gt_ok=1
else
  _gt_ok=0
fi
unset SECRET_ENC_PASS _GT_PASS

# Google API keys look like: AIza + 35 chars of [A-Za-z0-9_-]
case "$_gt_val" in
  AIza*) : ;;
  *) _gt_ok=0 ;;
esac
if [ "${#_gt_val}" -ne 39 ]; then _gt_ok=0; fi

if [ "$_gt_ok" -ne 1 ]; then
  echo "FAILED to decrypt (wrong passcode?). Check with your instructor." >&2
  unset _gt_ok
  _gt_cleanup; return 1 2>/dev/null || exit 1
fi
unset _gt_ok

export GT_API="$_gt_val"
# Masked confirmation: first 4 and last 4 characters only.
printf '  set GT_API = %s...%s\n' \
  "$(printf '%s' "$GT_API" | cut -c1-4)" \
  "$(printf '%s' "$GT_API" | rev | cut -c1-4 | rev)"

# Persist for new terminals / Codespace restarts. Idempotent managed block.
if command -v python3 >/dev/null 2>&1; then
  python3 - <<'PY' 2>/dev/null || true
import os, re
brc = os.path.expanduser("~/.bashrc")
beg, end = "# >>> sismid gt api key >>>", "# <<< sismid gt api key <<<"
try:
    txt = open(brc).read() if os.path.exists(brc) else ""
except Exception:
    txt = ""
txt = re.sub(re.escape(beg) + r".*?" + re.escape(end) + r"\n?", "", txt, flags=re.S)
v = os.environ.get("GT_API", "")
if v:
    vq = v.replace("'", "'\"'\"'")
    block = "%s\nexport GT_API='%s'\n%s" % (beg, vq, end)
    if txt and not txt.endswith("\n"):
        txt += "\n"
    open(brc, "w").write(txt + block + "\n")
PY
fi

cat <<'EOS'
  Unlocked. Quota: 1,000 requests/day, health-related topics only.
  Start (or restart) Jupyter now so the kernel picks up GT_API.
EOS
_gt_cleanup; return 0 2>/dev/null || exit 0
