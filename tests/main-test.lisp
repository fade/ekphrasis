;;;; tests/main-test.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; Tests for the binary's command line: the main package loads, and
;;;; RUN-COMMAND-LINE writes the right line to the right stream and returns
;;;; the right exit code for each kind of argument list.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((pkg (find-package '#:ekphrasis/tests/main-test)))
    (when pkg
      (sb-ext:without-package-locks (delete-package pkg)))))

(defpackage #:ekphrasis/tests/main-test
  (:use #:cl #:zebra)
  (:import-from #:ekphrasis/src/main
                #:run-command-line))

(in-package #:ekphrasis/tests/main-test)

(define-test scaffold-smoke
  "Asserts the project main package loaded cleanly."
  (true (find-package :ekphrasis/src/main)))

(defparameter *usage-line*
  (format nil "ekphrasis 0.1.0 is a Lisp library with no command line yet; ~
               see README.org and docs/getting-started.org.~%")
  "The usage line the binary writes, newline included.")

(defun call-command-line (&rest args)
  "Call RUN-COMMAND-LINE on ARGS with string streams.
Return the exit code, the text written to OUT, and the text written to ERR."
  (let* ((out (make-string-output-stream))
         (err (make-string-output-stream))
         (code (run-command-line args :out out :err err)))
    (values code
            (get-output-stream-string out)
            (get-output-stream-string err))))

(define-test no-arguments-print-usage-and-succeed
  "With no arguments, the usage line goes to OUT, ERR stays empty, and the code is 0."
  (multiple-value-bind (code out err) (call-command-line)
    (is = 0 code)
    (is string= *usage-line* out)
    (is string= "" err)))

(define-test help-flags-print-usage-and-succeed
  "\"--help\" and \"-h\" write the usage line to OUT and return 0."
  (dolist (flag '("--help" "-h"))
    (multiple-value-bind (code out err) (call-command-line flag)
      (is = 0 code "~A" flag)
      (is string= *usage-line* out "~A" flag)
      (is string= "" err "~A" flag))))

(define-test version-flag-prints-version-and-succeeds
  "\"--version\" writes the version line to OUT and returns 0."
  (multiple-value-bind (code out err) (call-command-line "--version")
    (is = 0 code)
    (is string= (format nil "ekphrasis 0.1.0~%") out)
    (is string= "" err)))

(define-test unknown-arguments-print-usage-to-err-and-fail
  "An unknown argument, or more than one argument, writes the usage line to
ERR, leaves OUT empty, and returns 2."
  (dolist (args '(("--daemon") ("--help" "--version")))
    (multiple-value-bind (code out err) (apply #'call-command-line args)
      (is = 2 code "~S" args)
      (is string= "" out "~S" args)
      (is string= *usage-line* err "~S" args))))
