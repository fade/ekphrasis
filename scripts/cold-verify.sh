#!/usr/bin/env bash
# scripts/cold-verify.sh: load ekphrasis cold and run both test suites.
#
# Starts a fresh SBCL with no init file and a private, empty fasl tree, so a
# missing :import-from or a stale fasl cannot hide behind a warm image. Checks
# that loading the core and its tests brings in no CLIM, then runs the core
# suite and the McCLIM adapter suite. Prints "cold-verify: ok" on success.
#
# Env (all optional):
#   LISP_WORKSPACE   local-projects tree    (default ~/SourceCode/lisp/)
#   QUICKLISP_SETUP  quicklisp setup.lisp   (default ~/quicklisp/setup.lisp)
#
# SPDX-License-Identifier: AGPL-3.0-or-later

set -euo pipefail

PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
QUICKLISP_SETUP="${QUICKLISP_SETUP:-${HOME}/quicklisp/setup.lisp}"
LISP_WORKSPACE="${LISP_WORKSPACE:-${HOME}/SourceCode/lisp}"
LISP_WORKSPACE="${LISP_WORKSPACE/#\~/$HOME}"
LISP_WORKSPACE="${LISP_WORKSPACE%/}"

if [ ! -f "$QUICKLISP_SETUP" ]; then
  echo "ERROR: quicklisp setup.lisp not found at $QUICKLISP_SETUP" >&2
  exit 1
fi

TMP_PREFIX="${TMPDIR:-/tmp}"
TMP_PREFIX="${TMP_PREFIX%/}/ekphrasis-cold."
FASL_DIR="$(mktemp -d "${TMP_PREFIX}XXXXXX")"

cleanup() {
  if [ -n "$FASL_DIR" ] && [ -d "$FASL_DIR" ] && [[ "$FASL_DIR" == "$TMP_PREFIX"* ]]; then
    rm -rf -- "$FASL_DIR"
  fi
}
trap cleanup EXIT INT TERM

sbcl --noinform --non-interactive --no-userinit \
     --eval "(require :asdf)" \
     --eval "(load \"$QUICKLISP_SETUP\")" \
     --eval "(asdf:initialize-output-translations
               '(:output-translations
                 (t (\"$FASL_DIR/\" :implementation :**/ :*.*.*))
                 :ignore-inherited-configuration))" \
     --eval "(push \"$LISP_WORKSPACE/\" asdf:*central-registry*)" \
     --eval "(asdf:load-asd \"$PROJECT_ROOT/ekphrasis.asd\")" \
     --eval "(asdf:load-system \"ekphrasis/tests\")" \
     --eval "(when (or (find-package \"CLIM\") (asdf:component-loaded-p \"clim-core\"))
               (format *error-output* \"~&cold-verify: core loaded CLIM~%\")
               (uiop:quit 3))" \
     --eval "(asdf:test-system \"ekphrasis\")" \
     --eval "(asdf:test-system \"ekphrasis/mcclim\")" \
     --eval "(format t \"~&cold-verify: ok~%\")"
