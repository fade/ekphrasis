#!/usr/bin/env bash
# scripts/dev-boot.sh: bring up an ekphrasis development image.
#
# Starts SBCL with LISP_WORKSPACE source-registry, loads ekphrasis,
# then starts a Slynk listener so an editor or dsmr-mcp can attach.
# A project-specific dev-tooling hook is called if it exists.
#
# Pass --background to detach; logs go to /tmp/ekphrasis-dev.log.
#
# Env (all optional):
#   SLYNK_PORT       slynk listen port      (default: project-specific)
#   SLYNK_HOST       slynk listen address   (default 127.0.0.1)
#   LISP_WORKSPACE   local-projects tree    (default ~/SourceCode/lisp/)
#   QUICKLISP_SETUP  quicklisp setup.lisp   (default ~/quicklisp/setup.lisp)
#
# SPDX-License-Identifier: AGPL-3.0-or-later

set -euo pipefail

PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SLYNK_PORT="${SLYNK_PORT:-26457}"
SLYNK_HOST="${SLYNK_HOST:-127.0.0.1}"
LISP_WORKSPACE="${LISP_WORKSPACE:-${HOME}/SourceCode/lisp}"
QUICKLISP_SETUP="${QUICKLISP_SETUP:-${HOME}/quicklisp/setup.lisp}"
BACKGROUND=0

LISP_WORKSPACE="${LISP_WORKSPACE/#\~/$HOME}"
LISP_WORKSPACE="${LISP_WORKSPACE%/}"

for arg in "$@"; do
  case "$arg" in
    --background|-b) BACKGROUND=1 ;;
    --help|-h)
      sed -n '3,/^$/p' "$0" | sed 's/^# \{0,1\}//'
      exit 0
      ;;
    *)
      echo "unknown arg: $arg (try --help)" >&2
      exit 2
      ;;
  esac
done

if [ ! -f "$QUICKLISP_SETUP" ]; then
  echo "ERROR: quicklisp setup.lisp not found at $QUICKLISP_SETUP" >&2
  exit 1
fi

if [ ! -d "$LISP_WORKSPACE" ]; then
  echo "ERROR: LISP_WORKSPACE=$LISP_WORKSPACE does not exist" >&2
  exit 1
fi

# Bump SLYNK_PORT past any occupied port so concurrent projects use distinct listeners.
while nc -z "${SLYNK_HOST}" "${SLYNK_PORT}" 2>/dev/null; do
  echo "[dev-boot] port ${SLYNK_PORT} is occupied, bumping to $((SLYNK_PORT + 1))" >&2
  SLYNK_PORT=$((SLYNK_PORT + 1))
done

# Remove any stale handshake on exit so dsmr-mcp does not follow a dead pointer.
trap 'rm -f "${PROJECT_ROOT}/.dsmr-slynk.port"' EXIT INT TERM

export SLYNK_PORT SLYNK_HOST LISP_WORKSPACE PROJECT_ROOT

SBCL_ARGS=(
  --noinform
  --dynamic-space-size 2048
  --disable-debugger
  --load "$QUICKLISP_SETUP"
  --eval "(push \"$LISP_WORKSPACE/\" asdf:*central-registry*)"
  --eval "(asdf:load-asd \"$PROJECT_ROOT/ekphrasis.asd\")"
  --eval "(asdf:load-system :ekphrasis)"
  --eval "(asdf:load-system :slynk)"
  --eval "(let ((bound-port (uiop:symbol-call :slynk :create-server
                               :port (parse-integer (uiop:getenv \"SLYNK_PORT\"))
                               :interface (uiop:getenv \"SLYNK_HOST\")
                               :dont-close t)))
             (with-open-file (f (concatenate 'string
                                              (uiop:getenv \"PROJECT_ROOT\")
                                              \"/.dsmr-slynk.port\")
                                :direction :output :if-exists :supersede
                                :if-does-not-exist :create)
               (format f \"~A:~A~%\" (uiop:getenv \"SLYNK_HOST\") bound-port))
             (format t \"~&ekphrasis dev image live, Slynk on ~A:~A~%\"
                       (uiop:getenv \"SLYNK_HOST\") bound-port))"
  --eval ";; Project dev-tooling hook: add your dev-only setup in scripts/dev-init.lisp"
  --eval "(let ((hook \"$PROJECT_ROOT/scripts/dev-init.lisp\"))
             (when (probe-file hook) (load hook)))"
  --eval "(loop (sleep 60))"
)

if [ "$BACKGROUND" -eq 1 ]; then
  LOG=/tmp/ekphrasis-dev.log
  PIDFILE=/tmp/ekphrasis-dev.pid
  echo "[dev-boot] starting in background; log: $LOG"
  : > "$LOG"
  setsid sbcl "${SBCL_ARGS[@]}" >"$LOG" 2>&1 < /dev/null &
  echo $! > "$PIDFILE"
  disown
  echo "[dev-boot] pid $(cat "$PIDFILE"); tail -f $LOG to follow"
else
  echo "[dev-boot] starting in foreground (Ctrl-C to stop)"
  exec sbcl "${SBCL_ARGS[@]}"
fi
