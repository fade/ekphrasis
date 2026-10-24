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
                #:contrast-ratio
                #:failing-findings
                #:finding-check
                #:finding-container
                #:finding-detail
                #:finding-element-id
                #:finding-expected
                #:finding-measured
                #:finding-severity
                #:finding-spec-id
                #:parse-rgb)
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

(define-test low-contrast-text-control
  "The title drawn in 10% grey on gray20 yields one :CONTRAST finding for :TITLE, with its ratio
against the text threshold and both colours; its clean twin none."
  (multiple-value-bind (findings report)
      (check-fixture (fixture-spec) :variant :low-contrast-text)
    (is = 1 (length findings) "~A" report)
    (let ((finding (first findings)))
      (is eq :contrast (finding-check finding) "~A" report)
      (is eq :error (finding-severity finding) "~A" report)
      (is eq :title (finding-spec-id finding) "~A" report)
      ;; McCLIM stores a 10% grey as 0.102, so the reference is the drawn ink as reported.
      (true (< (abs (- (finding-measured finding)
                       (contrast-ratio (parse-rgb "#1A1A1A") (parse-rgb "#333333"))))
               0.005)
            "~A" report)
      (is = 4.5 (finding-expected finding) "~A" report)
      (is string= "#1A1A1A" (getf (finding-detail finding) :ink) "~A" report)
      (is string= "#333333" (getf (finding-detail finding) :background) "~A" report)))
  (clean-twin-passes))

(define-test low-contrast-outline-control
  "The target box drawn in black on gray20 yields one :CONTRAST finding on that top-level
outline, with its ratio against the non-text threshold; its clean twin none."
  (multiple-value-bind (findings report)
      (check-fixture (fixture-spec) :variant :low-contrast-outline)
    (is = 1 (length findings) "~A" report)
    (let ((finding (first findings)))
      (is eq :contrast (finding-check finding) "~A" report)
      (is eq :outline (getf (finding-detail finding) :kind) "~A" report)
      (is eql 44 (finding-element-id finding) "~A" report)
      (false (finding-container finding) "~A" report)
      (true (< (abs (- (finding-measured finding) 1.662)) 0.005) "~A" report)
      (is = 3.0 (finding-expected finding) "~A" report)
      (is string= "#000000" (getf (finding-detail finding) :ink) "~A" report)
      (is string= "#333333" (getf (finding-detail finding) :background) "~A" report)))
  (clean-twin-passes))

(define-test per-call-threshold-clears-outline-control
  "A non-text threshold of 1.5 given to the call clears the black target box; at the default it
is found."
  (multiple-value-bind (findings report)
      (check-fixture (fixture-spec) :variant :low-contrast-outline
                                    :thresholds (list :non-text 1.5))
    (true (null findings) "~A" report))
  (is = 1 (length (check-fixture (fixture-spec) :variant :low-contrast-outline))))

(define-test per-spec-threshold-clears-text-control
  "A spec setting the text threshold to 1.3 clears the faint title, at 1.38 to 1; the fixture
spec without it finds the title."
  (multiple-value-bind (findings report)
      (check-fixture (append (fixture-spec) (list :contrast (list :text 1.3)))
                     :variant :low-contrast-text)
    (true (null findings) "~A" report))
  (is = 1 (length (check-fixture (fixture-spec) :variant :low-contrast-text))))

(define-test flipping-ink-is-reported-not-failed
  "The target box drawn in the flipping ink has no fixed colour: it yields one :UNRESOLVED-INK
warning, and nothing that fails; its clean twin none."
  (multiple-value-bind (findings report)
      (check-fixture (fixture-spec) :variant :flipping-outline)
    (is = 1 (length findings) "~A" report)
    (is eq :unresolved-ink (finding-check (first findings)) "~A" report)
    (is eq :warning (finding-severity (first findings)) "~A" report)
    (is eql 44 (finding-element-id (first findings)) "~A" report)
    (is equal '() (failing-findings findings) "~A" report))
  (clean-twin-passes))

(define-test conforming-spec-with-order-and-states-passes
  "The clean header meets the fixture spec's order and states, whether the pills report their
state or the drawing alone shows it."
  (multiple-value-bind (findings report) (check-fixture (fixture-spec) :variant :clean)
    (true (null findings) "reported: ~A" report))
  (multiple-value-bind (findings report)
      (check-fixture (fixture-spec) :variant :clean :render (list :state-function nil))
    (true (null findings) "drawn only: ~A" report)))

(define-test swapped-elements-control
  "The header drawing bravo left of alpha yields one :ORDER finding for the pill group, with the
drawn sequence; its clean twin none."
  (multiple-value-bind (findings report) (check-fixture (fixture-spec) :variant :swapped)
    (is = 1 (length findings) "~A" report)
    (let ((finding (first findings)))
      (is eq :order (finding-check finding) "~A" report)
      (is equal '(:alpha :bravo :charlie) (finding-expected finding) "~A" report)
      (is equal '(:bravo :alpha :charlie) (finding-measured finding) "~A" report)))
  (clean-twin-passes))

(define-test wrong-state-control
  "The header with bravo attached, reported and drawn so, yields one :STATE finding for :BRAVO,
expected fenced and measured attached; its clean twin none."
  (multiple-value-bind (findings report) (check-fixture (fixture-spec) :variant :wrong-state)
    (is = 1 (length findings) "~A" report)
    (let ((finding (first findings)))
      (is eq :state (finding-check finding) "~A" report)
      (is eq :bravo (finding-spec-id finding) "~A" report)
      (is eq :fenced (finding-expected finding) "~A" report)
      (is eq :attached (finding-measured finding) "~A" report)))
  (clean-twin-passes))

(define-test state-drawn-wrongly-control
  "The header with bravo fenced but its pill drawn attached yields one :STATE-APPEARANCE finding
for :BRAVO, reported fenced and drawn attached; its clean twin none."
  (multiple-value-bind (findings report)
      (check-fixture (fixture-spec) :variant :state-drawn-wrongly)
    (is = 1 (length findings) "~A" report)
    (let ((finding (first findings)))
      (is eq :state-appearance (finding-check finding) "~A" report)
      (is eq :bravo (finding-spec-id finding) "~A" report)
      (is eq :fenced (getf (finding-detail finding) :reported) "~A" report)
      (is eq :attached (getf (finding-detail finding) :drawn) "~A" report)))
  (clean-twin-passes))

(define-test drawn-state-alone-control
  "With no state reported, the header with bravo drawn attached yields one :STATE finding judged
from the drawing alone; its clean twin, also unreported, none."
  (let ((unreported (list :state-function nil)))
    (multiple-value-bind (findings report)
        (check-fixture (fixture-spec) :variant :wrong-state :render unreported)
      (is = 1 (length findings) "~A" report)
      (let ((finding (first findings)))
        (is eq :state (finding-check finding) "~A" report)
        (true (member :reported (finding-detail finding)) "~A" report)
        (false (getf (finding-detail finding) :reported) "~A" report)
        (is eq :attached (getf (finding-detail finding) :drawn) "~A" report)))
    (multiple-value-bind (findings report)
        (check-fixture (fixture-spec) :variant :clean :render unreported)
      (true (null findings) "clean twin: ~A" report))))
