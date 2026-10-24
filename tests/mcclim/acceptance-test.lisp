;;;; tests/mcclim/acceptance-test.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; End to end: a spec naming a title, judged against headless renders of the fixture header.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((pkg (find-package '#:ekphrasis/tests/mcclim/acceptance-test)))
    (when pkg
      (sb-ext:without-package-locks (delete-package pkg)))))

(defpackage #:ekphrasis/tests/mcclim/acceptance-test
  (:use #:cl #:zebra)
  (:import-from #:ekphrasis
                #:element-background
                #:element-container
                #:element-evidence
                #:element-id
                #:element-ink
                #:element-kind
                #:element-state
                #:element-text
                #:element-box
                #:failing-findings
                #:finding-box
                #:finding-check
                #:finding-container
                #:finding-detail
                #:finding-element-id
                #:finding-evidence
                #:finding-expected
                #:finding-measured
                #:finding-severity
                #:finding-spec-id
                #:finding-view
                #:format-finding
                #:snapshot-elements)
  (:import-from #:ekphrasis/mcclim)
  (:import-from #:ekphrasis/tests/mcclim/fixture
                #:check-fixture
                #:fixture-battery
                #:fixture-spec
                #:fixture-view
                #:title-spec))

(in-package #:ekphrasis/tests/mcclim/acceptance-test)

(defvar *demonstrate-failing-report* nil
  "When true, REPORT-DEMONSTRATION asserts on a render that has a finding, so it fails on purpose.")

(defparameter *resting* (list :exercise-feedback nil)
  "Render arguments that judge the resting drawing alone, with no highlight feedback exercised.")

(defun colour-near-p (expected actual)
  "True when the sRGB lists EXPECTED and ACTUAL agree within 1/255 on every channel."
  (and (= (length expected) (length actual))
       (every (lambda (e a) (<= (abs (- e a)) 1/255)) expected actual)))

(defun view-text-elements (snapshot)
  "Return the :TEXT elements SNAPSHOT draws directly on the view, outside any container."
  (remove-if-not (lambda (element)
                   (and (eq (element-kind element) :text)
                        (null (element-container element))))
                 (snapshot-elements snapshot)))

(define-test conforming-render-has-no-findings
  "The clean header draws the title as one text element with a real box, white ink on gray20."
  (multiple-value-bind (findings report snapshot)
      (check-fixture (title-spec) :variant :clean :render *resting*)
    (true (null findings) "~A" report)
    (let ((texts (view-text-elements snapshot)))
      (is = 1 (length texts))
      (let* ((title (first texts))
             (box (element-box title)))
        (is string= "EKPHRASIS" (element-text title))
        (true (> (third box) (first box)))
        (true (> (fourth box) (second box)))
        (true (colour-near-p '(1.0 1.0 1.0) (element-ink title)))
        (true (colour-near-p '(0.2 0.2 0.2) (element-background title)))
        (is eq :recorded (element-evidence title))))))

(define-test missing-title-is-located
  "The untitled header yields exactly one :missing finding saying what, where, and why."
  (let* ((findings (check-fixture (title-spec) :variant :untitled :render *resting*))
         (finding (first findings)))
    (is = 1 (length findings))
    (is eq :missing (finding-check finding))
    (is eq :error (finding-severity finding))
    (is eq :title (finding-spec-id finding))
    (is eq :header (finding-view finding))
    (false (finding-container finding))
    (false (finding-element-id finding))
    (false (finding-box finding))
    (is string= "EKPHRASIS" (finding-expected finding))
    (false (finding-measured finding))
    (is equal '(:recorded) (finding-evidence finding))
    (is equal '(:kind :text) (finding-detail finding))))

(define-test report-demonstration
  "With *DEMONSTRATE-FAILING-REPORT* true, fail on the untitled header so zebra prints the report."
  (if *demonstrate-failing-report*
      (multiple-value-bind (findings report)
          (check-fixture (title-spec) :variant :untitled :render *resting*)
        (true (null (failing-findings findings)) "~A" report))
      (true t)))

(define-test failing-assertion-prints-report
  "A failing zebra assertion over the untitled header prints the finding's report line."
  (let* ((missing (first (check-fixture (title-spec) :variant :untitled :render *resting*)))
         (line (format-finding missing nil))
         (result nil)
         (output (with-output-to-string (s)
                   ;; Without both bindings the inner run's results land in this suite's report.
                   (let ((*demonstrate-failing-report* t)
                         (zebra:*parent* nil)
                         (zebra:*context* nil))
                     (setf result (zebra:test 'report-demonstration
                                              :report 'zebra:plain :stream s))))))
    (is eq :failed (zebra:status result))
    (true (search line output) "~A" output)))

(define-test observer-watches-run
  "An observer sees the run's events in order; the same run with no observer finds the same.
The resting untitled header gives one :FINDING event. The single-box header, its highlight
feedback exercised, gives none, and its :ELEMENT-EXAMINED events end with the :HIGHLIGHTED
feedback elements."
  (flet ((watched (variant render)
           (let ((events '()))
             (multiple-value-bind (findings report snapshot)
                 (check-fixture (title-spec)
                                :variant variant
                                :render render
                                :observers (list (lambda (event) (push event events))))
               (declare (ignore report))
               (values findings snapshot (nreverse events)))))
         (kinds (events) (mapcar (lambda (event) (getf event :event)) events)))
    (multiple-value-bind (findings snapshot events) (watched :untitled *resting*)
      (declare (ignore snapshot))
      (let* ((kinds (kinds events))
             (finding-events (remove :finding events
                                     :key (lambda (event) (getf event :event))
                                     :test-not #'eq))
             (headless (check-fixture (title-spec) :variant :untitled :render *resting*)))
        (is eq :view-rendering (first kinds))
        (is eq :run-end (car (last kinds)))
        (is = 1 (length finding-events))
        (true (equalp (getf (first finding-events) :finding) (first findings)))
        (true (equalp findings headless))))
    (multiple-value-bind (findings snapshot events) (watched :single-box-pills nil)
      (let* ((kinds (kinds events))
             (examined (remove :element-examined events
                               :key (lambda (event) (getf event :event))
                               :test-not #'eq))
             (examined-ids (mapcar (lambda (event) (getf event :element-id)) examined))
             (highlighted-ids (mapcar #'element-id
                                      (remove :highlighted (snapshot-elements snapshot)
                                              :key #'element-state :test-not #'eq)))
             (headless (check-fixture (title-spec) :variant :single-box-pills)))
        (is eq :view-rendering (first kinds))
        (is eq :run-end (car (last kinds)))
        (false (member :finding kinds))
        (is equal (mapcar #'element-id (snapshot-elements snapshot)) examined-ids)
        (true highlighted-ids)
        (is equal highlighted-ids (last examined-ids (length highlighted-ids)))
        (true (equalp findings headless))))))

(define-test combined-defects-each-found-once
  "The header with charlie absent, bravo left of alpha, alpha fenced, and the target box in black
yields at rest exactly one :MISSING, :ORDER, :STATE, and :CONTRAST finding, and no warning."
  (multiple-value-bind (findings report)
      (check-fixture (fixture-spec) :variant :combined :render *resting*)
    (is = 4 (length findings) "~A" report)
    (is equal '(:contrast :missing :order :state)
        (sort (mapcar #'finding-check findings) #'string<)
        "~A" report)
    (true (every (lambda (finding) (eq :error (finding-severity finding))) findings) "~A" report)
    (flet ((one (check) (find check findings :key #'finding-check)))
      (is eq :charlie (finding-spec-id (one :missing)) "~A" report)
      (is equal '(:bravo :alpha) (finding-measured (one :order)) "~A" report)
      (is eq :alpha (finding-spec-id (one :state)) "~A" report)
      (is eq :fenced (finding-measured (one :state)) "~A" report)
      (is eq :outline (getf (finding-detail (one :contrast)) :kind) "~A" report)
      (false (finding-container (one :contrast)) "~A" report))))

(defun battery-cases ()
  "Return every control and clean-twin case of FIXTURE-BATTERY, and the combined case."
  (append (loop for entry in (fixture-battery)
                collect (getf entry :control)
                append (getf entry :twins))
          (list (list :frame 'fixture-view :variant :combined :render *resting*))))

(define-test battery-findings-identical-headless-and-watched
  "Every battery case finds the same with no observer as with one. The watched run starts with
:VIEW-RENDERING, examines every element in painting order, reports each finding in order, and
ends with :RUN-END counting the findings."
  (dolist (case (battery-cases))
    (destructuring-bind (&key frame variant render) case
      (flet ((run (observers)
               (check-fixture (fixture-spec) :frame frame :variant variant :render render
                                             :observers observers))
             (of-kind (kind events)
               (remove kind events :key (lambda (event) (getf event :event)) :test-not #'eq)))
        (let* ((headless (run nil))
               (events '())
               (label (list frame variant)))
          (multiple-value-bind (findings report snapshot)
              (run (list (lambda (event) (push event events))))
            (setf events (nreverse events))
            (true (equalp headless findings) "~S: ~A" label report)
            (is eq :view-rendering (getf (first events) :event) "~S" label)
            (let ((end (car (last events))))
              (is eq :run-end (getf end :event) "~S" label)
              (is = (length findings) (getf end :findings) "~S" label))
            (true (equalp findings (mapcar (lambda (event) (getf event :finding))
                                           (of-kind :finding events)))
                  "~S: ~A" label report)
            (is equal (mapcar #'element-id (snapshot-elements snapshot))
                (mapcar (lambda (event) (getf event :element-id))
                        (of-kind :element-examined events))
                "~S" label)))))))
