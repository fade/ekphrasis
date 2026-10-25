;;;; src/run.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; A run judges one view and reports its progress as events to the attached observers.
;;;; Headless is the run with no observers: the same path, with no event consed.

(defpackage #:ekphrasis/src/run
  (:use #:cl)
  (:import-from #:ekphrasis/src/compare
                #:compare-snapshot)
  (:import-from #:ekphrasis/src/report
                #:format-report)
  (:import-from #:ekphrasis/src/spec
                #:parse-spec
                #:spec-view)
  (:import-from #:ekphrasis/src/model
                #:element-id
                #:element-kind
                #:failing-findings
                #:snapshot-p)
  (:export #:*run-observers*
           #:observe-run-event
           #:run-check))

(in-package #:ekphrasis/src/run)

(defvar *run-observers* '()
  "The observers attached to the current run. Each receives every run event via OBSERVE-RUN-EVENT.")

(defgeneric observe-run-event (observer event)
  (:documentation "Deliver EVENT to OBSERVER.
EVENT is a plist whose :EVENT value is one of :VIEW-RENDERING, :ELEMENT-EXAMINED, :FINDING,
or :RUN-END. Every attached observer receives exactly one :RUN-END per run that begins, whose
:OUTCOME is :COMPLETED or :ABORTED; an aborted end counts the :FINDING events already delivered.
Errors signalled by an observer propagate to the caller of RUN-CHECK, with two exceptions.
An error while receiving an aborted end is discarded, so the condition that aborted the run
propagates. An error while receiving the completed end is held until every observer has
received it; the first one held is then signalled again.")
  (:method ((observer function) event)
    (funcall observer event)))

(defmacro notify (&rest event)
  "Deliver the plist EVENT to each attached observer. EVENT is built only when one is attached."
  `(when *run-observers*
     (let ((event (list ,@event)))
       (dolist (observer *run-observers*)
         (observe-run-event observer event)))))

(defun take-snapshot (source)
  "Return the snapshot SOURCE designates: SOURCE itself, or the result of calling it."
  (let ((snapshot (if (functionp source) (funcall source) source)))
    (unless (snapshot-p snapshot)
      (error 'type-error :datum snapshot :expected-type 'ekphrasis/src/model:snapshot))
    snapshot))

(defun run-check (spec source &key observers thresholds)
  "Judge the view SOURCE renders against SPEC, notifying OBSERVERS and any already attached.
SPEC is a spec record or spec data. SOURCE is a snapshot or a function of no arguments
returning one. OBSERVERS is a list of objects with an OBSERVE-RUN-EVENT method.
THRESHOLDS is a plist (:TEXT r :NON-TEXT r) of contrast thresholds passed to COMPARE-SNAPSHOT;
each key it sets overrides the spec's.
Every attached observer receives exactly one :RUN-END event, whose :OUTCOME is :COMPLETED or
:ABORTED. An aborted end counts the :FINDING events already delivered in :FINDINGS and :ERRORS.
Return three values: the findings, the report string, and the snapshot.
Signal MALFORMED-SPEC for a malformed spec, before any event. A condition that aborts the run
propagates unchanged: an observer error while receiving the aborted end is discarded.
The completed end reaches every observer even when some signal on it; the first such error is
then signalled again, the same condition object. Observer errors on every other event
propagate at once, and the aborted end follows."
  (let* ((spec (parse-spec spec))
         (view (spec-view spec))
         (*run-observers* (append observers *run-observers*))
         (delivered '())
         (ended nil))
    (unwind-protect
         (progn
           (notify :event :view-rendering :view view)
           (let* ((snapshot (take-snapshot source))
                  (findings
                    (compare-snapshot
                     snapshot spec
                     :thresholds thresholds
                     :on-examine (when *run-observers*
                                   (lambda (element)
                                     (notify :event :element-examined
                                             :view view
                                             :element-id (element-id element)
                                             :kind (element-kind element))))
                     :on-finding (when *run-observers*
                                   (lambda (finding)
                                     (push finding delivered)
                                     (notify :event :finding :view view :finding finding))))))
             (setf ended t)
             (when *run-observers*
               (let ((event (list :event :run-end
                                  :view view
                                  :outcome :completed
                                  :findings (length findings)
                                  :errors (length (failing-findings findings))))
                     (first-error nil))
                 (dolist (observer *run-observers*)
                   (handler-case (observe-run-event observer event)
                     (error (condition)
                       (unless first-error (setf first-error condition)))))
                 (when first-error (error first-error))))
             (values findings (format-report view findings) snapshot)))
      (when (and *run-observers* (not ended))
        (let ((event (list :event :run-end
                           :view view
                           :outcome :aborted
                           :findings (length delivered)
                           :errors (length (failing-findings delivered)))))
          (dolist (observer *run-observers*)
            (ignore-errors (observe-run-event observer event))))))))
