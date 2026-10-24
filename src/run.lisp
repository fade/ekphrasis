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
or :RUN-END. Errors signalled by an observer propagate to the caller of RUN-CHECK.")
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
Return three values: the findings, the report string, and the snapshot.
Signal MALFORMED-SPEC for a malformed spec; observer errors propagate unchanged."
  (let* ((spec (parse-spec spec))
         (view (spec-view spec))
         (*run-observers* (append observers *run-observers*)))
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
                              (notify :event :finding :view view :finding finding))))))
      (notify :event :run-end
              :view view
              :findings (length findings)
              :errors (length (failing-findings findings)))
      (values findings (format-report view findings) snapshot))))
