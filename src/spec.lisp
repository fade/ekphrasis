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
CLAUSE is the offending sub-form itself, or the whole form when the fault is at the top level.
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
ID is a keyword naming it; KIND is the expected element kind; LABEL is the expected text, or
NIL for an element with no text.
STATE is the expected state, or NIL; STATES lists the states it may be in, or NIL."
  (id nil :type (or null keyword))
  (kind nil :type (or null keyword))
  (label nil :type (or null string))
  (state nil)
  (states nil))

(defparameter *spec-keys* '(:view :elements :contrast)
  "The top-level keys a spec may hold.")

(defparameter *element-keys* '(:kind :label)
  "The keys an element clause may hold after its id.")

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

(defun first-bad-key (plist known)
  "Return the first key of PLIST that is not in KNOWN or repeats an earlier key, and a second
value, :UNKNOWN or :REPEATED, saying which. Return NIL when every key is known and distinct.
PLIST is a proper list of even length."
  (let ((seen '()))
    (loop for key in plist by #'cddr
          do (cond ((not (member key known :test #'eq))
                    (return (values key :unknown)))
                   ((member key seen :test #'eq)
                    (return (values key :repeated)))
                   (t
                    (push key seen))))))

(defun parse-element-clause (clause)
  "Validate one element clause (id :kind kind [:label label]) and return a spec-element.
ID is a keyword; :KIND is required and a keyword; :LABEL is optional and a string, or NIL for an
element with no text. Signal MALFORMED-SPEC naming CLAUSE when it is not well formed."
  (cond ((null clause)
         (malformed clause "an element clause needs a keyword id"))
        ((not (and (consp clause) (proper-list-p clause)))
         (malformed clause "an element clause must be a proper list")))
  (let ((id (first clause))
        (options (rest clause)))
    (unless (keywordp id)
      (malformed clause "element id must be a keyword"))
    (unless (evenp (length options))
      (malformed clause "an element clause must hold a plist of even length after its id"))
    (multiple-value-bind (key problem) (first-bad-key options *element-keys*)
      (case problem
        (:unknown (malformed clause (format nil "unknown element key ~S" key)))
        (:repeated (malformed clause (format nil "element key ~S appears twice" key)))))
    (multiple-value-bind (kind kind-present) (clause-value options :kind)
      (unless kind-present
        (malformed clause "an element clause needs :kind"))
      (unless (keywordp kind)
        (malformed clause "element :kind must be a keyword"))
      (let ((label (clause-value options :label)))
        (unless (or (null label) (stringp label))
          (malformed clause "element :label must be a string"))
        (make-spec-element :id id :kind kind :label label)))))

(defparameter *contrast-keys* '(:text :non-text)
  "The keys a :CONTRAST clause may hold.")

(defun parse-contrast-clause (clause)
  "Validate the value of a spec's :CONTRAST key and return it.
CLAUSE is a plist whose keys are :TEXT and :NON-TEXT, each at most once, and whose values are
positive reals; NIL sets no threshold. Signal MALFORMED-SPEC naming CLAUSE when it is not."
  (unless (plist-shaped-p clause)
    (malformed clause "spec :contrast must be a plist of even length"))
  (multiple-value-bind (key problem) (first-bad-key clause *contrast-keys*)
    (case problem
      (:unknown (malformed clause (format nil "unknown contrast key ~S" key)))
      (:repeated (malformed clause (format nil "contrast key ~S appears twice" key)))))
  (loop for (key value) on clause by #'cddr
        unless (and (realp value) (plusp value))
          do (malformed clause (format nil "contrast ~S must be a positive real" key)))
  clause)

(defun parse-spec (form)
  "Return the spec FORM describes, as a spec record.
FORM is a spec record, returned unchanged, or a plist (:view keyword :elements (clause ...)),
optionally with :contrast (:text r :non-text r).
:ELEMENTS is a non-empty proper list of element clauses with distinct ids. :CONTRAST sets
positive real thresholds for either key.
Signal MALFORMED-SPEC naming the offending clause when FORM is not well formed: the whole form
for a top-level key that is absent, unknown, or repeated, or a :VIEW that is not a keyword; the
:ELEMENTS value when it is not a proper list; the :CONTRAST value when it is malformed; the
element clause at fault otherwise, the later one for a repeated id. A circular list signals
rather than looping.
Never evaluates or reads any part of FORM."
  (when (spec-p form)
    (return-from parse-spec form))
  (unless (plist-shaped-p form)
    (malformed form "a spec must be a proper list of even length"))
  (multiple-value-bind (key problem) (first-bad-key form *spec-keys*)
    (case problem
      (:unknown (malformed form (format nil "unknown spec key ~S" key)))
      (:repeated (malformed form (format nil "spec key ~S appears twice" key)))))
  (multiple-value-bind (view view-present) (clause-value form :view)
    (unless view-present
      (malformed form "a spec needs :view"))
    (unless (keywordp view)
      (malformed form "spec :view must be a keyword"))
    (multiple-value-bind (elements elements-present) (clause-value form :elements)
      (unless elements-present
        (malformed form "a spec needs :elements"))
      (when (null elements)
        (malformed form "spec :elements must name at least one element"))
      (unless (and (consp elements) (proper-list-p elements))
        (malformed elements "spec :elements must be a proper list"))
      (let ((parsed '()))
        (dolist (clause elements)
          (let ((element (parse-element-clause clause)))
            (when (find (spec-element-id element) parsed :key #'spec-element-id)
              (malformed clause (format nil "element id ~S is used twice"
                                        (spec-element-id element))))
            (push element parsed)))
        (make-spec :view view
                   :elements (nreverse parsed)
                   :contrast (parse-contrast-clause (clause-value form :contrast)))))))
