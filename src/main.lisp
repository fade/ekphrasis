;;;; src/main.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; Entry point for ekphrasis. Edit the run/daemon/dev mode bodies below;
;;;; the dispatch skeleton is wired; add your own defuns/defclasses here
;;;; and in additional src/*.lisp files.

(defpackage #:ekphrasis/src/main
  (:use #:cl)
  ;; MAIN is the project's entry point: build.sh names it as the saved
  ;; image's toplevel, so it has to be reachable from outside this package.
  (:export #:main))

(in-package #:ekphrasis/src/main)

;;; Keep this defvar here so --version works before any other load.
(defvar *version* "0.1.0"
  "Version string for ekphrasis.")

(defun show-help ()
  "Print usage information and exit."
  (format t "ekphrasis v~A~%~%" *version*)
  (format t "Usage: ekphrasis [options]~%~%")
  (format t "Options:~%")
  (format t "  --daemon   Run headless in the background~%")
  (format t "  --dev      Start with Slynk dev listener (SLYNK_PORT, SLYNK_HOST)~%")
  (format t "  --version  Print version and exit~%")
  (format t "  --help     Show this message~%"))

(defun run-foreground ()
  "Foreground run mode. Replace this with your application logic."
  (format t "ekphrasis: running in foreground mode~%")
  ;; TODO: add foreground run logic here.
  )

(defun run-daemon ()
  "Headless daemon mode. Replace this with your background logic."
  (format t "ekphrasis: running in daemon mode~%")
  ;; TODO: add daemon/background logic here.
  )

(defun start-dev ()
  "Dev mode: load Slynk and start a listener so an editor or dsmr-mcp can attach."
  (let ((port (or (ignore-errors
                   (parse-integer (uiop:getenv "SLYNK_PORT")))
                  4005))
        (host (or (uiop:getenv "SLYNK_HOST") "127.0.0.1")))
    (asdf:load-system :slynk)
    (uiop:symbol-call :slynk :create-server :port port :interface host :dont-close t)
    (format t "ekphrasis: Slynk listening on ~A:~A~%" host port)
    ;; Fall through to foreground run so the process stays alive.
    (run-foreground)))

(defun main (&rest args)
  "Main entry point. Dispatches on command-line arguments."
  (let ((mode :run))
    (loop for arg in args do
      (cond ((string= arg "--daemon")  (setf mode :daemon))
            ((string= arg "--dev")     (setf mode :dev))
            ((string= arg "--version")
             (format t "ekphrasis v~A~%" *version*)
             (return-from main 0))
            ((string= arg "--help")
             (show-help)
             (return-from main 0))))
    (ecase mode
      (:run    (run-foreground))
      (:daemon (run-daemon))
      (:dev    (start-dev))))
  0)
