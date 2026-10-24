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
or :RUN-END. Every run that begins ends with exactly one :RUN-END whose :OUTCOME is :COMPLETED
or :ABORTED; an aborted end counts the :FINDING events already delivered.
Errors signalled by an observer propagate to the caller of RUN-CHECK, except an error while
receiving an aborted end, which is discarded so the condition that aborted the run propagates.")
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
Every run that begins ends with exactly one :RUN-END event whose :OUTCOME is :COMPLETED or
:ABORTED. An aborted end counts the :FINDING events already delivered in :FINDINGS and :ERRORS.
Return three values: the findings, the report string, and the snapshot.
Signal MALFORMED-SPEC for a malformed spec, before any event. A condition that aborts the run
propagates unchanged: an observer error while receiving the aborted end is discarded, and
observer errors on every other event propagate."
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
             (notify :event :run-end
                     :view view
                     :outcome :completed
                     :findings (length findings)
                     :errors (length (failing-findings findings)))
             (values findings (format-report view findings) snapshot)))
      (when (and *run-observers* (not ended))
        (let ((event (list :event :run-end
                           :view view
                           :outcome :aborted
                           :findings (length delivered)
                           :errors (length (failing-findings delivered)))))
          (dolist (observer *run-observers*)
            (ignore-errors (observe-run-event observer event))))))))
