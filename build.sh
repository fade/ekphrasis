#!/usr/bin/env bash
# build.sh: produce a standalone ekphrasis binary via SBCL.
#
# Usage: ./build.sh [PATH]
#   PATH   where to write the binary (default: bin/ekphrasis)
#
# Requires SBCL and all ASDF deps available (via LISP_WORKSPACE or Quicklisp).
# The binary keeps the runtime options it was built with, so every argument,
# --help and --version included, reaches the toplevel instead of the SBCL runtime.
#
# SPDX-License-Identifier: AGPL-3.0-or-later

set -euo pipefail

PROJECT_ROOT="$(cd "$(dirname "$0")" && pwd)"
OUTPUT="${1:-$PROJECT_ROOT/bin/ekphrasis}"
QUICKLISP_SETUP="${QUICKLISP_SETUP:-${HOME}/quicklisp/setup.lisp}"
LISP_WORKSPACE="${LISP_WORKSPACE:-${HOME}/SourceCode/lisp}"
LISP_WORKSPACE="${LISP_WORKSPACE/#\~/$HOME}"

mkdir -p "$(dirname "$OUTPUT")"

sbcl --noinform \
     --no-userinit \
     --disable-debugger \
     --eval "(require :asdf)" \
     --eval "(when (probe-file \"$QUICKLISP_SETUP\") (load \"$QUICKLISP_SETUP\"))" \
     --eval "(push \"$LISP_WORKSPACE/\" asdf:*central-registry*)" \
     --eval "(asdf:load-asd \"$PROJECT_ROOT/ekphrasis.asd\")" \
     --eval "(asdf:load-system :ekphrasis)" \
     --eval "(sb-ext:save-lisp-and-die \"$OUTPUT\"
               :toplevel #'ekphrasis/src/main:main
               :executable t
               :save-runtime-options t
               :compression t)"

if [ ! -f "$OUTPUT" ]; then
  echo "ERROR: no binary at $OUTPUT; the build did not produce one" >&2
  exit 1
fi

echo "Built: $OUTPUT"
