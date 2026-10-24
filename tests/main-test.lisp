;;;; tests/main-test.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; Smoke test for ekphrasis: asserts the main package loaded cleanly.
;;;; Replace or extend this file to add real tests for your code.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((pkg (find-package '#:ekphrasis/tests/main-test)))
    (when pkg
      (sb-ext:without-package-locks (delete-package pkg)))))

(defpackage #:ekphrasis/tests/main-test
  (:use #:cl #:zebra))

(in-package #:ekphrasis/tests/main-test)

(define-test scaffold-smoke
  "Asserts the project main package loaded cleanly."
  (true (find-package :ekphrasis/src/main)))
