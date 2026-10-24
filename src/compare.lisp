;;;; src/compare.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; Judge a snapshot against a spec. Pure over its arguments apart from the optional callbacks.

(defpackage #:ekphrasis/src/compare
  (:use #:cl)
  (:import-from #:ekphrasis/src/model
                #:element-container
                #:element-kind
                #:element-text
                #:make-finding
                #:snapshot-elements
                #:snapshot-view)
  (:import-from #:ekphrasis/src/spec
                #:parse-spec
                #:spec-elements
                #:spec-element-id
                #:spec-element-kind
                #:spec-element-label)
  (:export #:compare-snapshot))

(in-package #:ekphrasis/src/compare)

(defun pairs-with-p (element spec-element)
  "True when ELEMENT can stand for SPEC-ELEMENT: drawn on the view itself, same kind, same text."
  (let ((text (element-text element)))
    (and (null (element-container element))
         (eq (element-kind element) (spec-element-kind spec-element))
         text
         (string= text (spec-element-label spec-element)))))

(defun missing-finding (spec-element view)
  "Return the :MISSING finding for SPEC-ELEMENT, which no drawn element in VIEW stands for."
  (make-finding :check :missing
                :severity :error
                :spec-id (spec-element-id spec-element)
                :view view
                :container nil
                :element-id nil
                :box nil
                :expected (spec-element-label spec-element)
                :measured nil
                :evidence (list :recorded)
                :detail (list :kind (spec-element-kind spec-element))))

(defun compare-snapshot (snapshot spec &key on-examine on-finding)
  "Judge SNAPSHOT against SPEC and return the findings in the order made.
SPEC is a spec record or spec data; it is parsed with PARSE-SPEC, which may signal MALFORMED-SPEC.
ON-EXAMINE, when non-NIL, is called with each element of SNAPSHOT in painting order.
ON-FINDING, when non-NIL, is called with each finding as it is made."
  (let* ((spec (parse-spec spec))
         (view (snapshot-view snapshot))
         (elements (snapshot-elements snapshot))
         (paired '())
         (findings '()))
    (flet ((emit (finding)
             (when on-finding
               (funcall on-finding finding))
             (push finding findings)))
      (dolist (spec-element (spec-elements spec))
        (let ((match (find-if (lambda (element)
                                (and (not (member element paired :test #'eq))
                                     (pairs-with-p element spec-element)))
                              elements)))
          (if match
              (push match paired)
              (emit (missing-finding spec-element view)))))
      (when on-examine
        (dolist (element elements)
          (funcall on-examine element))))
    (nreverse findings)))
