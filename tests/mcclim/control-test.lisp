;;;; tests/mcclim/control-test.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; Controls: each fixture variant carries one defect the checks must find, and the clean
;;;; variant, its twin, must pass the same spec.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((pkg (find-package '#:ekphrasis/tests/mcclim/control-test)))
    (when pkg
      (sb-ext:without-package-locks (delete-package pkg)))))

(defpackage #:ekphrasis/tests/mcclim/control-test
  (:use #:cl #:zebra)
  (:import-from #:ekphrasis
                #:finding-check
                #:finding-expected
                #:finding-measured
                #:finding-spec-id)
  (:import-from #:ekphrasis/mcclim)
  (:import-from #:ekphrasis/tests/mcclim/fixture
                #:check-fixture
                #:fixture-spec))

(in-package #:ekphrasis/tests/mcclim/control-test)

(defun clean-twin-passes ()
  "Assert that the clean fixture yields no findings against the fixture spec."
  (multiple-value-bind (findings report) (check-fixture (fixture-spec) :variant :clean)
    (true (null findings) "clean twin: ~A" report)))

(define-test missing-element-control
  "The header without the bravo pill yields one :MISSING finding for :BRAVO; its clean twin none."
  (multiple-value-bind (findings report) (check-fixture (fixture-spec) :variant :missing)
    (is = 1 (length findings) "~A" report)
    (is eq :missing (finding-check (first findings)) "~A" report)
    (is eq :bravo (finding-spec-id (first findings)) "~A" report))
  (clean-twin-passes))

(define-test relabelled-element-control
  "The header labelling bravo \"brovo\" yields one :LABEL finding for :BRAVO; its clean twin none."
  (multiple-value-bind (findings report) (check-fixture (fixture-spec) :variant :relabelled)
    (is = 1 (length findings) "~A" report)
    (let ((finding (first findings)))
      (is eq :label (finding-check finding) "~A" report)
      (is eq :bravo (finding-spec-id finding) "~A" report)
      (is string= "bravo" (finding-expected finding) "~A" report)
      (is string= "brovo" (finding-measured finding) "~A" report)))
  (clean-twin-passes))
