#!/usr/bin/env bash
# setup_git.sh — initialise this folder as a git repo and push to GitHub.
#
# Usage:
#   bash setup_git.sh GITHUB_USER REPO_NAME
#
# Example:
#   bash setup_git.sh hgrueso bolivia-aula-conectada
#
# Prerequisites:
#   - You are inside the analysis/ folder (the one with .here and code/)
#   - You have either:
#       (a) `gh` CLI installed AND authenticated (`gh auth login`), OR
#       (b) SSH keys set up for GitHub on this machine
#
# What this script does:
#   1. Validates you're in the right folder
#   2. Creates .gitignore if missing (with safe defaults)
#   3. Initialises git, configures user (or prompts you to)
#   4. Stages files and refuses to commit raw microdata
#   5. Commits with a sensible default message
#   6. Creates the GitHub repo (via gh CLI) or sets up the remote manually
#   7. Pushes to GitHub

set -euo pipefail

# ===== Parse arguments =====
if [[ $# -lt 2 ]]; then
  cat <<EOF
Usage: bash setup_git.sh GITHUB_USER REPO_NAME [COMMIT_MSG]

Examples:
  bash setup_git.sh hgrueso bolivia-aula-conectada
  bash setup_git.sh hgrueso bolivia-aula-conectada "Initial commit message"
EOF
  exit 1
fi

GITHUB_USER="$1"
REPO_NAME="$2"
COMMIT_MSG="${3:-Initial commit: Aula Conectada investment case pipeline}"
DEFAULT_BRANCH="main"

echo "================================================================"
echo "  GitHub setup for: ${GITHUB_USER}/${REPO_NAME}"
echo "================================================================"
echo ""

# ===== Step 1: validate location =====
echo "→ Step 1: Checking we're in the right directory…"
if [[ ! -f ".here" ]] || [[ ! -d "code" ]] || [[ ! -d "R" ]]; then
  echo "✗ This doesn't look like the analysis/ folder."
  echo "  Run this script from the directory containing .here, code/, R/"
  echo "  Current directory: $(pwd)"
  exit 1
fi
echo "  ✓ In the right folder ($(pwd))"
echo ""

# ===== Step 2: ensure .gitignore exists =====
echo "→ Step 2: Checking .gitignore…"
if [[ ! -f ".gitignore" ]]; then
  echo "  No .gitignore found. Creating one with safe defaults…"
  cat > .gitignore <<'GITIGNORE'
# macOS / system
.DS_Store
.Rhistory
.RData
.Rapp.history
.Ruserdata

# Quarto / R build artefacts
*.html
*.knit.md
*_files/
*_cache/
.quarto/
*.tex
*.log
*.aux

# Raw EH 2024 microdata (large, redistribution-restricted by INE)
data/BD_EH2024/
data/BD_EH2024.zip
data/*.sav
data/*.csv.gz

# Generated outputs (re-creatable by running the pipeline)
output/
!output/.gitkeep

# Renv / virtual environments
renv/library/
renv/local/
renv/cellar/
renv/python/
.venv/

# Editor / IDE
.vscode/
.idea/
*.swp
*~

# Optional: do NOT commit rendered PDFs (large, re-creatable)
*.pdf

# But DO commit the curated lit-review CSVs
!data/lit_review_papers.csv
!data/lit_review_effect_modifiers.csv
GITIGNORE
  echo "  ✓ Created .gitignore"
else
  echo "  ✓ .gitignore already exists"
fi
echo ""

# ===== Step 3: git init =====
echo "→ Step 3: Initialising git…"
if [[ ! -d ".git" ]]; then
  git init -b "$DEFAULT_BRANCH"
  echo "  ✓ Initialised new git repo on branch '$DEFAULT_BRANCH'"
else
  echo "  ✓ Already a git repo"
fi
echo ""

# ===== Step 4: configure user identity =====
echo "→ Step 4: Checking git user identity…"
if [[ -z "$(git config user.email 2>/dev/null || true)" ]]; then
  echo "✗ git user.email not set. Run these two commands first, then re-run:"
  echo "    git config --global user.email 'hgrueso@unicef.org'"
  echo "    git config --global user.name  'Hernando Grueso'"
  exit 1
fi
echo "  ✓ Configured as: $(git config user.name) <$(git config user.email)>"
echo ""

# ===== Step 5: stage files =====
echo "→ Step 5: Staging files…"
git add .
echo "  Staged (first 20 entries):"
git status --short | head -20
echo ""

# ===== Step 6: safety check =====
echo "→ Step 6: Safety check — looking for raw microdata in staged files…"
if git diff --cached --name-only | grep -q "BD_EH2024"; then
  echo "✗ Raw EH 2024 microdata is staged. .gitignore is wrong or being overridden."
  echo "  Files matched:"
  git diff --cached --name-only | grep "BD_EH2024" | head -5
  exit 1
fi
echo "  ✓ No raw microdata staged"
echo ""

# ===== Step 7: commit =====
echo "→ Step 7: Committing…"
if git diff --cached --quiet; then
  echo "  Nothing to commit — already up to date"
else
  git commit -m "$COMMIT_MSG"
  echo "  ✓ Committed: $COMMIT_MSG"
fi
echo ""

# ===== Step 8: create GitHub repo & push =====
echo "→ Step 8: Pushing to GitHub…"

if command -v gh >/dev/null 2>&1; then
  if ! gh auth status >/dev/null 2>&1; then
    echo "✗ gh CLI is installed but not authenticated."
    echo "  Run: gh auth login"
    echo "  Then re-run this script."
    exit 1
  fi
  echo "  Using GitHub CLI (gh)…"

  if gh repo view "${GITHUB_USER}/${REPO_NAME}" >/dev/null 2>&1; then
    echo "  Repo ${GITHUB_USER}/${REPO_NAME} already exists on GitHub."
    if git remote | grep -q "^origin$"; then
      echo "  Remote 'origin' already configured. Pushing…"
    else
      echo "  Adding remote and pushing…"
      git remote add origin "https://github.com/${GITHUB_USER}/${REPO_NAME}.git"
    fi
    git push -u origin "$DEFAULT_BRANCH"
  else
    echo "  Creating private repo and pushing…"
    gh repo create "${GITHUB_USER}/${REPO_NAME}" --private --source=. \
        --description "Bolivia Aula Conectada — UNICEF investment case analysis" \
        --remote=origin --push
  fi

  echo ""
  echo "================================================================"
  echo "  ✓ Done. Your repo is at:"
  echo "     https://github.com/${GITHUB_USER}/${REPO_NAME}"
  echo "================================================================"
  exit 0
fi

# Fallback: no gh CLI
echo "  gh CLI not installed. Falling back to SSH push."
echo "  Make sure the repo exists at github.com/new BEFORE continuing."
echo ""

REMOTE_URL_SSH="git@github.com:${GITHUB_USER}/${REPO_NAME}.git"

if git remote | grep -q "^origin$"; then
  echo "  Remote 'origin' already configured: $(git remote get-url origin)"
else
  echo "  Adding remote: $REMOTE_URL_SSH"
  git remote add origin "$REMOTE_URL_SSH"
fi

echo "  Pushing…"
echo "  (If this fails with 'Permission denied (publickey)' you need to set up SSH keys"
echo "   or install gh CLI: 'brew install gh && gh auth login')"
git push -u origin "$DEFAULT_BRANCH"

echo ""
echo "================================================================"
echo "  ✓ Done. Your repo is at:"
echo "     https://github.com/${GITHUB_USER}/${REPO_NAME}"
echo "================================================================"
