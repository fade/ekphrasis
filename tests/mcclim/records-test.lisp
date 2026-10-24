;;;; tests/mcclim/records-test.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; The McCLIM internal classes and slots the walker reads must exist in this image. A fork that
;;;; moves one fails here, naming what moved and the fork commit the walker was read from.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((pkg (find-package '#:ekphrasis/tests/mcclim/records-test)))
    (when pkg
      (sb-ext:without-package-locks (delete-package pkg)))))

(defpackage #:ekphrasis/tests/mcclim/records-test
  (:use #:cl #:zebra)
  (:import-from #:ekphrasis/mcclim)
  (:import-from #:ekphrasis/src/mcclim/records
                #:*mcclim-fork-commit*
                #:*record-internals*
                #:missing-record-internals))

(in-package #:ekphrasis/tests/mcclim/records-test)

(defun missing-internals-description (missing)
  "Return the failure text for the internals MISSING: what is gone and the pinned fork commit."
  (format nil "McCLIM internals the walker reads are missing: ~{~A~^; ~}. ~
The walker was read from McCLIM fork commit ~A."
          missing *mcclim-fork-commit*))

(define-test pinned-record-internals-present
  "Every McCLIM internal class and slot the walker reads exists in this image."
  (let ((missing (missing-record-internals)))
    (true (null missing) "~A" (missing-internals-description missing))))

(define-test missing-internals-are-named
  "An absent class and an absent slot are each named, so the pinned test's failure says what moved."
  (let* ((present-class (first (first *record-internals*)))
         (missing (missing-record-internals
                   (list (list 'absent-record-class 'absent-slot)
                         (list present-class 'absent-slot))))
         (text (missing-internals-description missing)))
    (is = 2 (length missing))
    (true (search "ABSENT-RECORD-CLASS" (first missing)) "~A" text)
    (true (search "ABSENT-SLOT" (second missing)) "~A" text)
    (true (search *mcclim-fork-commit* text) "~A" text)))
