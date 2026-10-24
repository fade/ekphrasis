;;;; src/spec.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; A spec states what a view should show, as plain Lisp data. PARSE-SPEC validates it and never
;;;; evaluates any part of it.

(defpackage #:ekphrasis/src/spec
  (:use #:cl)
  (:export #:malformed-spec
           #:malformed-spec-clause
           #:malformed-spec-reason
           #:spec
           #:make-spec
           #:spec-p
           #:spec-view
           #:spec-elements
           #:spec-order
           #:spec-states
           #:spec-contrast
           #:spec-element
           #:make-spec-element
           #:spec-element-p
           #:spec-element-id
           #:spec-element-kind
           #:spec-element-label
           #:spec-element-state
           #:spec-element-states
           #:parse-spec))

(in-package #:ekphrasis/src/spec)

(define-condition malformed-spec (error)
  ((clause :initarg :clause :reader malformed-spec-clause)
   (reason :initarg :reason :reader malformed-spec-reason))
  (:report (lambda (condition stream)
             (let ((*print-length* 10)
                   (*print-level* 4))
               (format stream "Malformed spec clause ~S: ~A"
                       (malformed-spec-clause condition)
                       (malformed-spec-reason condition)))))
  (:documentation "Signalled by PARSE-SPEC when a spec is not well formed.
CLAUSE is the offending sub-form itself, or the whole form when a required key is absent.
REASON is a string saying what is wrong."))

(defstruct (spec (:copier nil))
  "A parsed spec.
VIEW is the keyword naming the view; ELEMENTS is a list of spec-element records in spec order.
ORDER, STATES, and CONTRAST hold the spec's ordering, state, and contrast clauses, or NIL."
  (view nil :type (or null keyword))
  (elements '() :type list)
  (order nil)
  (states nil)
  (contrast nil))

(defstruct (spec-element (:copier nil))
  "One element a spec expects.
ID is a keyword naming it; KIND is the expected element kind; LABEL is the expected text.
STATE is the expected state, or NIL; STATES lists the states it may be in, or NIL."
  (id nil :type (or null keyword))
  (kind nil :type (or null keyword))
  (label nil :type (or null string))
  (state nil)
  (states nil))

(defun malformed (clause reason)
  "Signal MALFORMED-SPEC naming CLAUSE and REASON."
  (error 'malformed-spec :clause clause :reason reason))

(defun proper-list-p (object)
  "True when OBJECT is a proper list: neither dotted nor circular."
  (and (listp object)
       (handler-case (list-length object)
         (type-error () nil))))

(defun plist-shaped-p (object)
  "True when OBJECT is a proper list of even length."
  (let ((length (proper-list-p object)))
    (and length (evenp length))))

(defun clause-value (plist key)
  "Return the value of KEY in PLIST and a second value, true when KEY is present."
  (loop for (k v) on plist by #'cddr
        when (eq k key)
          do (return (values v t))
        finally (return (values nil nil))))

(defun parse-element-clause (clause)
  "Validate one element clause (id :kind kind :label label ...) and return a spec-element."
  (unless (and (consp clause) (proper-list-p clause))
    (malformed clause "an element clause must be a proper list"))
  (let ((id (first clause))
        (options (rest clause)))
    (unless (keywordp id)
      (malformed clause "an element clause must start with a keyword id"))
    (unless (evenp (length options))
      (malformed clause "an element clause must hold an even-length plist after its id"))
    (multiple-value-bind (kind kind-present) (clause-value options :kind)
      (unless kind-present
        (malformed clause "an element clause needs :kind"))
      (unless (keywordp kind)
        (malformed kind "an element :kind must be a keyword"))
      (multiple-value-bind (label label-present) (clause-value options :label)
        (unless label-present
          (malformed clause "an element clause needs :label"))
        (unless (stringp label)
          (malformed label "an element :label must be a string"))
        (make-spec-element :id id :kind kind :label label)))))

(defun parse-spec (form)
  "Return the spec FORM describes, as a spec record.
FORM is a spec record, returned unchanged, or a plist (:view keyword :elements (clause ...)).
Signal MALFORMED-SPEC naming the offending clause when FORM is not well formed.
Never evaluates or reads any part of FORM."
  (when (spec-p form)
    (return-from parse-spec form))
  (unless (plist-shaped-p form)
    (malformed form "a spec must be a proper list of even length"))
  (multiple-value-bind (view view-present) (clause-value form :view)
    (unless view-present
      (malformed form "a spec needs :view"))
    (unless (keywordp view)
      (malformed view "a spec :view must be a keyword"))
    (multiple-value-bind (elements elements-present) (clause-value form :elements)
      (unless elements-present
        (malformed form "a spec needs :elements"))
      (unless (proper-list-p elements)
        (malformed elements "a spec :elements must be a proper list"))
      (make-spec :view view
                 :elements (mapcar #'parse-element-clause elements)))))
