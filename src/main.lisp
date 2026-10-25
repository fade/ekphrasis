;;;; src/main.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; Toplevel of the binary that build.sh produces. ekphrasis is a library,
;;;; so the binary prints a usage line that points at the documentation,
;;;; prints the version, or rejects its arguments.

(defpackage #:ekphrasis/src/main
  (:use #:cl)
  ;; build.sh names MAIN as the saved image's toplevel.
  (:export #:main
           #:run-command-line))

(in-package #:ekphrasis/src/main)

(defvar *version* "0.1.0"
  "Version string for ekphrasis. Matches :version in ekphrasis.asd.")

(defun usage-line ()
  "Return the one-line usage message, without a trailing newline."
  (format nil "ekphrasis ~A is a Lisp library with no command line yet; ~
               see README.org and docs/getting-started.org."
          *version*))

(defun run-command-line (args &key (out *standard-output*) (err *error-output*))
  "Act on the command-line strings ARGS and return the process exit code.

No arguments, \"--help\", or \"-h\": write the usage line to OUT and return 0.
\"--version\": write the version line to OUT and return 0.
Anything else, including more than one argument: write the usage line to
ERR and return 2.
Each line ends in a newline, and nothing else is written."
  (let ((arg (and (= (length args) 1) (first args))))
    (cond ((or (null args)
               (member arg '("--help" "-h") :test #'equal))
           (format out "~A~%" (usage-line))
           0)
          ((equal arg "--version")
           (format out "ekphrasis ~A~%" *version*)
           0)
          (t
           (format err "~A~%" (usage-line))
           2))))

(defun main ()
  "Toplevel of the saved image: act on the process arguments and exit.
SBCL calls the toplevel with no arguments, so the arguments come from
UIOP:COMMAND-LINE-ARGUMENTS, which excludes the program name."
  (uiop:quit (run-command-line (uiop:command-line-arguments))))
