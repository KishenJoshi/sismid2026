# secrets/

Encrypted agent credentials for the course. **Only `*.enc` files live here**, and
each one is AES-256 encrypted with the class passcode (PBKDF2-SHA256, 600k
iterations, base64 armored). Plaintext is blocked from this folder by `.gitignore`.

- Instructor creates a blob:  `scripts/secret-encrypt.sh CLAUDE_CODE_OAUTH_TOKEN`

- Student unlocks in class:    `source scripts/claude-login.sh`  (enter the passcode)

The `.enc` files are safe to commit. The passcode is **never** stored in the repo;
the instructor says it out loud on the day. See `docs/agent-setup.md` and
`docs/google-trends-api.md`.

Current secrets:

- `CLAUDE_CODE_OAUTH_TOKEN.enc` - Claude Code login for the class
- `gt-api/GT_API.enc` - Google Trends API key, in its own subfolder with a **separate
  passcode**. Instructor: `scripts/gt-api-encrypt.sh`. Student: `source
  scripts/unlock-gt-api-key.sh`. Kept out of `secrets/*.enc` so `claude-login.sh`
  does not try (and fail) to decrypt it. Quota 1,000/day, health topics only.
