#!/usr/bin/env bash
# render_pdf.sh — render the Quarto deck(s) to PDF.
#
# Usage:
#   bash render_pdf.sh           # Spanish (default)
#   bash render_pdf.sh es        # Spanish
#   bash render_pdf.sh en        # English
#   bash render_pdf.sh both      # Both languages
#
# Strategy:
#   1. Quarto renders reveal.js to HTML.
#   2. If `decktape` is installed, use it — purpose-built for reveal.js → PDF.
#   3. Otherwise, headless Chrome with flags tuned for reveal.js's JS layout.
#   4. If resulting PDF is < 5 KB, warn that it's likely blank.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
mkdir -p "$ROOT/output"

LANG_ARG="${1:-es}"

render_one() {
  local lang="$1"
  local qmd="$ROOT/code/06_slides_${lang}.qmd"
  local html="$ROOT/code/06_slides_${lang}.html"
  local pdf="$ROOT/output/06_slides_${lang}.pdf"

  if [[ ! -f "$qmd" ]]; then
    echo "✗ Source not found: $qmd" >&2
    return 1
  fi

  echo "→ Rendering Quarto deck ($lang)…"
  quarto render "$qmd"

  # --- Path A: decktape (preferred) ---
  if command -v decktape >/dev/null 2>&1; then
    echo "→ Converting with decktape…"
    decktape reveal --size '1280x720' "$html" "$pdf"
    echo "✓ PDF → $pdf"
    mkdir -p "$ROOT/slides"
    cp -f "$pdf" "$ROOT/slides/aulas_conectadas_${lang}.pdf"
    echo "✓ Copied to slides/aulas_conectadas_${lang}.pdf"
    return 0
  fi

  # --- Path B: headless Chrome ---
  local chrome_mac="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
  local chrome_linux
  chrome_linux="$(command -v google-chrome 2>/dev/null || true)"
  local chrome

  if [[ -x "$chrome_mac" ]]; then
    chrome="$chrome_mac"
  elif [[ -n "$chrome_linux" ]]; then
    chrome="$chrome_linux"
  else
    echo "✗ Neither decktape nor Chrome is available." >&2
    return 1
  fi

  echo "→ Converting with headless Chrome (install decktape for better results)…"
  local tmp
  tmp="$(mktemp -d)"
  trap 'rm -rf "$tmp"' RETURN

  "$chrome" \
    --headless=new \
    --no-sandbox \
    --disable-gpu \
    --disable-dev-shm-usage \
    --disable-software-rasterizer \
    --disable-background-networking \
    --disable-component-update \
    --disable-default-apps \
    --no-first-run \
    --no-default-browser-check \
    --run-all-compositor-stages-before-draw \
    --virtual-time-budget=30000 \
    --user-data-dir="$tmp" \
    --no-pdf-header-footer \
    --print-to-pdf="$pdf" \
    "file://$html?print-pdf" \
    >/dev/null 2>&1

  local size
  size=$(wc -c < "$pdf" | tr -d ' ')
  if (( size < 5000 )); then
    cat >&2 <<EOF

⚠ The PDF is only $size bytes — almost certainly blank.

Headless Chrome printed before reveal.js finished laying out the slides.
Most reliable fix:

    npm install -g @astefanutti/decktape
    bash render_pdf.sh $lang

Manual fallback:

    open '$html'
    # In Chrome, append  ?print-pdf  and hit return
    # Cmd+P  →  Save as PDF · Landscape · Background graphics ON

EOF
    return 1
  fi

  echo "✓ PDF → $pdf ($size bytes)"

  # Mirror the deck into slides/ for version-controlled distribution
  mkdir -p "$ROOT/slides"
  cp -f "$pdf" "$ROOT/slides/aulas_conectadas_${lang}.pdf"
  echo "✓ Copied to slides/aulas_conectadas_${lang}.pdf"
}

case "$LANG_ARG" in
  es)    render_one es ;;
  en)    render_one en ;;
  both)  render_one es && render_one en ;;
  *)     echo "Unknown lang: $LANG_ARG  (use es | en | both)" >&2; exit 1 ;;
esac
