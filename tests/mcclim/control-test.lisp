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
                #:element-background
                #:element-container
                #:element-id
                #:element-ink
                #:element-kind
                #:element-state
                #:element-text
                #:failing-findings
                #:finding-check
                #:finding-container
                #:finding-detail
                #:finding-element-id
                #:finding-expected
                #:finding-measured
                #:finding-severity
                #:finding-spec-id
                #:format-finding
                #:parse-rgb
                #:rgb-match-p
                #:snapshot-elements)
  (:import-from #:ekphrasis/mcclim)
  (:import-from #:ekphrasis/tests/mcclim/fixture
                #:check-fixture
                #:dark-ink-fixture-view
                #:fixture-battery
                #:fixture-spec))

(in-package #:ekphrasis/tests/mcclim/control-test)

(defparameter *resting* (list :exercise-feedback nil)
  "Render arguments that judge the resting drawing alone, with no highlight feedback exercised.")

(defun clean-twin-passes ()
  "Assert that the clean fixture, at rest, yields no findings against the fixture spec."
  (multiple-value-bind (findings report)
      (check-fixture (fixture-spec) :variant :clean :render *resting*)
    (true (null findings) "clean twin: ~A" report)))

(define-test missing-element-control
  "The header without the bravo pill yields one :MISSING finding for :BRAVO; its clean twin none."
  (multiple-value-bind (findings report)
      (check-fixture (fixture-spec) :variant :missing :render *resting*)
    (is = 1 (length findings) "~A" report)
    (is eq :missing (finding-check (first findings)) "~A" report)
    (is eq :bravo (finding-spec-id (first findings)) "~A" report))
  (clean-twin-passes))

(define-test relabelled-element-control
  "The header labelling bravo \"brovo\" yields one :LABEL finding for :BRAVO; its clean twin none."
  (multiple-value-bind (findings report)
      (check-fixture (fixture-spec) :variant :relabelled :render *resting*)
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
      (check-fixture (fixture-spec) :variant :low-contrast-text :render *resting*)
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
      (check-fixture (fixture-spec) :variant :low-contrast-outline :render *resting*)
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
      (check-fixture (fixture-spec) :variant :low-contrast-outline :render *resting*
                                    :thresholds (list :non-text 1.5))
    (true (null findings) "~A" report))
  (is = 1 (length (check-fixture (fixture-spec) :variant :low-contrast-outline
                                                :render *resting*))))

(define-test per-spec-threshold-clears-text-control
  "A spec setting the text threshold to 1.3 clears the faint title, at 1.38 to 1; the fixture
spec without it finds the title."
  (multiple-value-bind (findings report)
      (check-fixture (append (fixture-spec) (list :contrast (list :text 1.3)))
                     :variant :low-contrast-text :render *resting*)
    (true (null findings) "~A" report))
  (is = 1 (length (check-fixture (fixture-spec) :variant :low-contrast-text
                                                :render *resting*))))

(define-test flipping-ink-is-reported-not-failed
  "The target box drawn in the flipping ink has no fixed colour: it yields one :UNRESOLVED-INK
warning, and nothing that fails; its clean twin none."
  (multiple-value-bind (findings report)
      (check-fixture (fixture-spec) :variant :flipping-outline :render *resting*)
    (is = 1 (length findings) "~A" report)
    (is eq :unresolved-ink (finding-check (first findings)) "~A" report)
    (is eq :warning (finding-severity (first findings)) "~A" report)
    (is eql 44 (finding-element-id (first findings)) "~A" report)
    (is equal '() (failing-findings findings) "~A" report))
  (clean-twin-passes))

(define-test mark-on-a-fill-of-unknown-colour-is-unresolved
  "The title drawn over a flipping-ink fill, or over a pattern, has no known background: each
yields one :UNRESOLVED-INK warning on the title, unresolved in its background, and nothing that
fails; its clean twin none."
  (dolist (variant '(:flipping-fill :pattern-fill))
    (multiple-value-bind (findings report snapshot)
        (check-fixture (fixture-spec) :variant variant :render *resting*)
      (is = 1 (length findings) "~A: ~A" variant report)
      (is equal '() (failing-findings findings) "~A: ~A" variant report)
      (let ((finding (first findings)))
        (when finding
          (is eq :unresolved-ink (finding-check finding) "~A: ~A" variant report)
          (is eq :warning (finding-severity finding) "~A: ~A" variant report)
          (is eq :title (finding-spec-id finding) "~A: ~A" variant report)
          (is equal '(:background) (getf (finding-detail finding) :unresolved)
              "~A: ~A" variant report)
          (let ((title (found-element finding snapshot)))
            (is eq :text (element-kind title) "~A" variant)
            (is eq nil (element-container title) "~A" variant)
            (is string= "EKPHRASIS" (element-text title) "~A" variant)
            (true (rgb-match-p '(1 1 1) (element-ink title))
                  "~A ink ~S" variant (element-ink title))
            (is eq nil (element-background title) "~A" variant))))))
  (clean-twin-passes))

(define-test conforming-spec-with-order-and-states-passes
  "The clean header meets the fixture spec's order and states, whether the pills report their
state or the drawing alone shows it."
  (multiple-value-bind (findings report)
      (check-fixture (fixture-spec) :variant :clean :render *resting*)
    (true (null findings) "reported: ~A" report))
  (multiple-value-bind (findings report)
      (check-fixture (fixture-spec) :variant :clean
                                    :render (list :state-function nil :exercise-feedback nil))
    (true (null findings) "drawn only: ~A" report)))

(define-test swapped-elements-control
  "The header drawing bravo left of alpha yields one :ORDER finding for the pill group, with the
drawn sequence; its clean twin none."
  (multiple-value-bind (findings report)
      (check-fixture (fixture-spec) :variant :swapped :render *resting*)
    (is = 1 (length findings) "~A" report)
    (let ((finding (first findings)))
      (is eq :order (finding-check finding) "~A" report)
      (is equal '(:alpha :bravo :charlie) (finding-expected finding) "~A" report)
      (is equal '(:bravo :alpha :charlie) (finding-measured finding) "~A" report)))
  (clean-twin-passes))

(define-test wrong-state-control
  "The header with bravo attached, reported and drawn so, yields one :STATE finding for :BRAVO,
expected fenced and measured attached; its clean twin none."
  (multiple-value-bind (findings report)
      (check-fixture (fixture-spec) :variant :wrong-state :render *resting*)
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
      (check-fixture (fixture-spec) :variant :state-drawn-wrongly :render *resting*)
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
  (let ((unreported (list :state-function nil :exercise-feedback nil)))
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

(defun found-element (finding snapshot)
  "Return the element of SNAPSHOT that FINDING names by id."
  (find (finding-element-id finding) (snapshot-elements snapshot) :key #'element-id))

(define-test highlight-that-covers-the-label-hides-it
  "Exercising the default pills, whose highlight paints every band over the label, yields one
:CONTRAST finding per pill on its label re-judged as highlighted, white on white at 1.0 against
4.5; the single-box and bordered pills yield none."
  (multiple-value-bind (findings report snapshot) (check-fixture (fixture-spec) :variant :clean)
    (is = 3 (length findings) "~A" report)
    (let ((pills (remove :session-pill (snapshot-elements snapshot)
                         :key #'element-kind :test-not #'eq)))
      (dolist (pill pills)
        (let ((own (remove-if-not (lambda (finding)
                                    (eql (element-id pill) (finding-container finding)))
                                  findings)))
          (is = 1 (length own) "~A: ~A" (element-text pill) report)
          (let* ((finding (first own))
                 (label (found-element finding snapshot)))
            (is eq :contrast (finding-check finding) "~A" report)
            (is eq :text (element-kind label) "~A" report)
            (is eq :highlighted (element-state label) "~A" report)
            (is eql (element-id pill) (element-container label) "~A" report)
            (is string= (element-text pill) (element-text label) "~A" report)
            (is string= "#FFFFFF" (getf (finding-detail finding) :ink) "~A" report)
            (is string= "#FFFFFF" (getf (finding-detail finding) :background) "~A" report)
            (true (< (abs (- 1 (finding-measured finding))) 1d-9) "~A" report)
            (is = 4.5 (finding-expected finding) "~A" report))))))
  (dolist (variant '(:single-box-pills :bordered-pills))
    (multiple-value-bind (findings report) (check-fixture (fixture-spec) :variant variant)
      (true (null findings) "~A: ~A" variant report))))

(define-test interaction-only-outline-control
  "On a black foreground over a near-black page, single-box pills pass at rest; exercised, each
pill's black highlight frame is a :CONTRAST finding on a highlighted :OUTLINE inside the pill, at
the ratio of black on #14151D against 3.0. The same pills on the light-foreground frame pass."
  (multiple-value-bind (findings report)
      (check-fixture (fixture-spec) :variant :single-box-pills :frame 'dark-ink-fixture-view
                                    :render *resting*)
    (true (null findings) "at rest: ~A" report))
  (multiple-value-bind (findings report snapshot)
      (check-fixture (fixture-spec) :variant :single-box-pills :frame 'dark-ink-fixture-view)
    (is = 3 (length findings) "~A" report)
    (let ((expected (contrast-ratio '(0 0 0) (parse-rgb "#14151D"))))
      (dolist (finding findings)
        (let ((frame (found-element finding snapshot)))
          (is eq :contrast (finding-check finding) "~A" report)
          (is eq :outline (element-kind frame) "~A" report)
          (is eq :highlighted (element-state frame) "~A" report)
          (is eq :session-pill
              (element-kind (find (element-container frame) (snapshot-elements snapshot)
                                  :key #'element-id))
              "~A" report)
          (true (< (abs (- expected (finding-measured finding))) 0.01) "~A" report)
          (is = 3.0 (finding-expected finding) "~A" report)))))
  (multiple-value-bind (findings report) (check-fixture (fixture-spec) :variant :single-box-pills)
    (true (null findings) "clean twin: ~A" report)))

(define-test interaction-only-lines-say-when-and-which-label
  "Exercised, each label a band pill's highlight hides has a line naming the label's text and its
pill's spec id and ending \", only when highlighted\"; each invisible frame of the dark-ink pills
has a line ending the same way with no label clause."
  (flet ((ends-with-p (suffix line)
           (and (>= (length line) (length suffix))
                (string= suffix line :start2 (- (length line) (length suffix))))))
    (multiple-value-bind (findings report snapshot) (check-fixture (fixture-spec) :variant :clean)
      (let ((lines (mapcar (lambda (finding) (format-finding finding nil)) findings))
            (clauses (getf (fixture-spec) :elements)))
        (is = 3 (length lines) "~A" report)
        (is string= (concatenate 'string
                                 "  error contrast #46 (text) at (814 15 870 28) in header #2: "
                                 "1.00 below 4.50 (ink #FFFFFF on #FFFFFF), "
                                 "label \"charlie\" of :charlie, only when highlighted")
            (first lines))
        (dolist (pill (remove :session-pill (snapshot-elements snapshot)
                              :key #'element-kind :test-not #'eq))
          (let* ((text (element-text pill))
                 (id (car (find text clauses :key (lambda (clause) (getf (cdr clause) :label))
                                             :test #'equal)))
                 (finding (find (element-id pill) findings :key #'finding-container))
                 (line (and finding (format-finding finding nil)))
                 (suffix (format nil ", label ~S of :~(~A~), only when highlighted" text id)))
            (true id "no spec element labelled ~S" text)
            (true (and line (ends-with-p suffix line)) "~A: ~A" text report)))))
    (multiple-value-bind (findings report)
        (check-fixture (fixture-spec) :variant :single-box-pills :frame 'dark-ink-fixture-view)
      (let ((lines (mapcar (lambda (finding) (format-finding finding nil)) findings)))
        (is = 3 (length lines) "~A" report)
        (is string= (concatenate 'string
                                 "  error contrast #45 (outline) at (802 8 882 30) in header #2: "
                                 "1.15 below 3.00 (ink #000000 on #14151D), only when highlighted")
            (first lines))
        (dolist (line lines)
          (true (ends-with-p ", only when highlighted" line) "~A" line)
          (false (search "label" line) "~A" line))))))

(defun check-case (case)
  "Judge the battery CASE, a plist (:FRAME frame :VARIANT variant :RENDER render), against the
fixture spec and return what CHECK-FIXTURE returns."
  (check-fixture (fixture-spec) :frame (getf case :frame)
                                :variant (getf case :variant)
                                :render (getf case :render)))

(define-test each-class-has-a-control-and-clean-twin
  "Every defect class has a control that yields at least one error of its check, on an element of
the kind and state the class names, and clean twins that yield no findings."
  (dolist (entry (fixture-battery))
    (destructuring-bind (&key class check mark control twins) entry
      (multiple-value-bind (findings report snapshot) (check-case control)
        (let ((found (remove-if-not
                      (lambda (finding)
                        (and (eq check (finding-check finding))
                             (eq :error (finding-severity finding))
                             (or (null mark)
                                 (let ((element (found-element finding snapshot)))
                                   (and element
                                        (eq (first mark) (element-kind element))
                                        (eq (second mark) (element-state element)))))))
                      findings)))
          (true found "~A control: ~A" class report)))
      (dolist (twin twins)
        (multiple-value-bind (findings report) (check-case twin)
          (true (null findings) "~A twin ~S: ~A" class (getf twin :variant) report))))))
