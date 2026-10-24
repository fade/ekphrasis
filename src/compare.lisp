;;;; src/compare.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; Judge a snapshot against a spec. Pure over its arguments apart from the optional callbacks.

(defpackage #:ekphrasis/src/compare
  (:use #:cl)
  (:import-from #:ekphrasis/src/model
                #:element-box
                #:element-container
                #:element-id
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

(defun label-matches-p (label text)
  "True when drawn TEXT is LABEL. A NIL label matches NIL or empty text."
  (if label
      (and text (string= label text))
      (or (null text) (string= text ""))))

(defun reading-order-p (a b)
  "True when element A comes before element B in reading order: by box top, then by box left.
An element with no box reads as if at the origin."
  (let ((top-a (or (second (element-box a)) 0))
        (top-b (or (second (element-box b)) 0)))
    (or (< top-a top-b)
        (and (= top-a top-b)
             (< (or (first (element-box a)) 0) (or (first (element-box b)) 0))))))

(defun pair-elements (spec-elements elements)
  "Pair each of SPEC-ELEMENTS with the drawn element among ELEMENTS that stands for it.
ELEMENTS are the drawn elements in scope, in painting order.
First, each spec element in spec order takes the first unpaired drawn element of its kind whose
text matches its label. Then, per kind, the spec elements still unpaired take the unpaired drawn
elements of that kind position by position, in reading order.
Return two values: a list of conses (spec-element . element) in spec order, with element NIL
when nothing stands for it; and the drawn elements left unpaired, in painting order."
  (let ((pairs (mapcar (lambda (spec-element) (cons spec-element nil)) spec-elements))
        (taken '()))
    (flet ((free-p (element)
             (not (member element taken :test #'eq)))
           (of-kind-p (kind element)
             (eq kind (element-kind element))))
      (dolist (pair pairs)
        (let* ((spec-element (car pair))
               (match (find-if (lambda (element)
                                 (and (free-p element)
                                      (of-kind-p (spec-element-kind spec-element) element)
                                      (label-matches-p (spec-element-label spec-element)
                                                       (element-text element))))
                               elements)))
          (when match
            (setf (cdr pair) match)
            (push match taken))))
      (dolist (kind (remove-duplicates (mapcar #'spec-element-kind spec-elements)
                                       :from-end t))
        (let ((unpaired (remove-if-not (lambda (pair)
                                         (and (null (cdr pair))
                                              (eq kind (spec-element-kind (car pair)))))
                                       pairs))
              (free (stable-sort (remove-if-not (lambda (element)
                                                  (and (free-p element)
                                                       (of-kind-p kind element)))
                                                (copy-list elements))
                                 #'reading-order-p)))
          (loop for pair in unpaired
                for element in free
                do (setf (cdr pair) element)
                   (push element taken))))
      (values pairs (remove-if-not #'free-p elements)))))

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

(defun label-finding (spec-element element view)
  "Return the :LABEL finding for SPEC-ELEMENT, which ELEMENT in VIEW stands for with other text."
  (make-finding :check :label
                :severity :error
                :spec-id (spec-element-id spec-element)
                :view view
                :container (element-container element)
                :element-id (element-id element)
                :box (element-box element)
                :expected (spec-element-label spec-element)
                :measured (element-text element)
                :evidence (list :recorded)
                :detail (list :kind (spec-element-kind spec-element))))

(defun unexpected-finding (element view)
  "Return the :UNEXPECTED warning for ELEMENT, drawn in VIEW with no spec element it stands for."
  (make-finding :check :unexpected
                :severity :warning
                :spec-id nil
                :view view
                :container (element-container element)
                :element-id (element-id element)
                :box (element-box element)
                :expected nil
                :measured (element-text element)
                :evidence (list :recorded)
                :detail (list :kind (element-kind element))))

(defun compare-snapshot (snapshot spec &key on-examine on-finding)
  "Judge SNAPSHOT against SPEC and return the findings in the order made.
SPEC is a spec record or spec data; it is parsed with PARSE-SPEC, which may signal MALFORMED-SPEC.
Presence is judged over the elements drawn on the view itself, outside any container: a spec
element nothing stands for is :MISSING; one stood for by an element with other text is a :LABEL
error; an unpaired element of a kind the spec names is an :UNEXPECTED warning. Kinds the spec
does not name are not judged for presence.
ON-EXAMINE, when non-NIL, is called with each element of SNAPSHOT in painting order.
ON-FINDING, when non-NIL, is called with each finding as it is made."
  (let* ((spec (parse-spec spec))
         (view (snapshot-view snapshot))
         (elements (snapshot-elements snapshot))
         (spec-elements (spec-elements spec))
         (named-kinds (mapcar #'spec-element-kind spec-elements))
         (findings '()))
    (flet ((emit (finding)
             (when on-finding
               (funcall on-finding finding))
             (push finding findings)))
      (multiple-value-bind (pairs unpaired)
          (pair-elements spec-elements (remove-if #'element-container elements))
        (loop for (spec-element . element) in pairs
              do (cond ((null element)
                        (emit (missing-finding spec-element view)))
                       ((not (label-matches-p (spec-element-label spec-element)
                                              (element-text element)))
                        (emit (label-finding spec-element element view)))))
        (dolist (element unpaired)
          (when (member (element-kind element) named-kinds :test #'eq)
            (emit (unexpected-finding element view)))))
      (when on-examine
        (dolist (element elements)
          (funcall on-examine element))))
    (nreverse findings)))
