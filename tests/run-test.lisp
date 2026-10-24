;;;; tests/run-test.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; The run's event contract over hand-built snapshots: order, headless identity, and observer
;;;; objects.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((pkg (find-package '#:ekphrasis/tests/run-test)))
    (when pkg
      (sb-ext:without-package-locks (delete-package pkg)))))

(defpackage #:ekphrasis/tests/run-test
  (:use #:cl #:zebra)
  (:import-from #:ekphrasis
                #:finding-check
                #:finding-spec-id
                #:make-element
                #:make-snapshot
                #:observe-run-event
                #:run-check))

(in-package #:ekphrasis/tests/run-test)

(defun header-snapshot ()
  "Return a snapshot of the header view drawing two text elements."
  (make-snapshot :view :header
                 :elements (list (make-element :id 1 :kind :text :text "EKPHRASIS"
                                               :box (list 16 14 133 36))
                                 (make-element :id 2 :kind :text :text "offline"
                                               :box (list 16 40 72 56)))))

(defun header-spec ()
  "Return a spec naming one element the header draws and one it does not."
  (list :view :header
        :elements (list (list :title :kind :text :label "EKPHRASIS")
                        (list :status :kind :text :label "ONLINE"))))

(defun event-kinds (events)
  "Return the :EVENT value of each event in EVENTS."
  (mapcar (lambda (event) (getf event :event)) events))

(defun events-of-kind (kind events)
  "Return the events in EVENTS whose :EVENT value is KIND."
  (remove kind events :key (lambda (event) (getf event :event)) :test-not #'eq))

(defun watched-run ()
  "Run the header check with a recording function observer.
Return the findings, the report, and the events in delivery order."
  (let ((events '()))
    (multiple-value-bind (findings report)
        (run-check (header-spec) (header-snapshot)
                   :observers (list (lambda (event) (push event events))))
      (values findings report (nreverse events)))))

(defclass recording-observer ()
  ((events :initform '() :accessor recorded-events))
  (:documentation "An observer object that keeps every event it receives, newest first."))

(defmethod observe-run-event ((observer recording-observer) event)
  (push event (recorded-events observer)))

(define-test observer-receives-events-in-order
  "The run delivers :view-rendering, the :missing finding, one :element-examined per element in
painting order, then :run-end with the counts."
  (multiple-value-bind (findings report events) (watched-run)
    (declare (ignore report))
    (is equal '(:view-rendering :finding :element-examined :element-examined :run-end)
        (event-kinds events))
    (is eq :header (getf (first events) :view))
    (let ((finding (getf (first (events-of-kind :finding events)) :finding)))
      (is eq :missing (finding-check finding))
      (is eq :status (finding-spec-id finding))
      (is eq (first findings) finding))
    (is equal '(1 2)
        (mapcar (lambda (event) (getf event :element-id))
                (events-of-kind :element-examined events)))
    (is equal '(:text :text)
        (mapcar (lambda (event) (getf event :kind))
                (events-of-kind :element-examined events)))
    (let ((end (car (last events))))
      (is = 1 (getf end :findings))
      (is = 1 (getf end :errors))
      (is eq :header (getf end :view)))))

(define-test headless-run-returns-identical-findings
  "The run with no observer returns equalp findings and the same report as the watched run."
  (multiple-value-bind (watched-findings watched-report) (watched-run)
    (multiple-value-bind (findings report) (run-check (header-spec) (header-snapshot))
      (is = 1 (length findings))
      (true (equalp watched-findings findings))
      (is string= watched-report report))))

(define-test observer-object-receives-events
  "An observer object with an OBSERVE-RUN-EVENT method receives the same events as a function."
  (let ((observer (make-instance 'recording-observer)))
    (run-check (header-spec) (header-snapshot) :observers (list observer))
    (multiple-value-bind (findings report events) (watched-run)
      (declare (ignore findings report))
      (is equal (event-kinds events)
          (event-kinds (reverse (recorded-events observer)))))))
