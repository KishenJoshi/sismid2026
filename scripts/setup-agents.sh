#!/usr/bin/env bash
# Install the AI coding agents used in SISMID 2026.
# This installs the CLIs only. It does NOT authenticate you; see docs/agent-setup.md.
set -euo pipefail

echo "==> Checking Node / npm..."
if ! command -v npm >/dev/null 2>&1; then
  echo "npm not found. This should be present in the Codespace. If running locally,"
  echo "install Node.js LTS first: https://nodejs.org/"
  exit 1
fi
node --version
npm --version

echo "==> Installing OpenAI Codex CLI (@openai/codex)..."
npm install -g @openai/codex

echo "==> Installing Anthropic Claude Code (@anthropic-ai/claude-code)..."
npm install -g @anthropic-ai/claude-code

echo "==> Installing Google Antigravity CLI (agy)..."
# Antigravity CLI is not on npm; it ships its own installer (macOS/Linux).
# It replaces the Gemini CLI, whose free individual login is retired on 2026-06-18.
# Non-fatal: Antigravity is the optional backup agent, so a hiccup here must not abort
# the script after Codex and Claude Code already installed successfully.
curl -fsSL https://antigravity.google/cli/install.sh | bash \
  || echo "   (Antigravity install failed - it is the optional backup agent; retry later or skip.)"

echo
echo "==> Installed versions:"
codex --version 2>/dev/null || echo "codex: run 'codex --version' to check"
claude --version 2>/dev/null || echo "claude: run 'claude --version' to check"
agy --version 2>/dev/null || echo "agy: run 'agy --version' to check"

echo
echo "Done. All three agents are installed but NOT logged in."
echo "Authentication will be provided in class. See docs/agent-setup.md."
echo "Never commit API keys or paste them into notebook cells."
