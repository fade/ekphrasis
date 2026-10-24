;;;; src/spec.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; A spec states what a view should show, as plain Lisp data. PARSE-SPEC validates it and never
;;;; evaluates any part of it.

(defpackage #:ekphrasis/src/spec
  (:use #:cl)
  (:import-from #:ekphrasis/src/rgb
                #:parse-rgb)
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

(defparameter *spec-keys* '(:view :elements :order :states :contrast)
  "The top-level keys a spec may hold.")

(defparameter *element-keys* '(:kind :label :state :states)
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
  "Validate one element clause (id :kind kind [:label label] [:state s] [:states (s ...)]) and
return a spec-element.
ID is a keyword; :KIND is required and a keyword; :LABEL is optional and a string, or NIL for an
element with no text. :STATE is the expected state, a keyword; :STATES lists the states the
element may be in, as keywords. Whether those states are declared for the kind is checked by
CHECK-ELEMENT-STATES. Signal MALFORMED-SPEC naming CLAUSE when it is not well formed."
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
        (multiple-value-bind (state state-present) (clause-value options :state)
          (when (and state-present (not (keywordp state)))
            (malformed clause "element :state must be a keyword"))
          (let ((states (clause-value options :states)))
            (unless (and (proper-list-p states) (every #'keywordp states))
              (malformed clause "element :states must be a proper list of keywords"))
            (make-spec-element :id id :kind kind :label label
                               :state state :states states)))))))

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

(defparameter *order-axes* '(:left-to-right :top-to-bottom)
  "The axes an :ORDER group may be judged along.")

(defun parse-order-group (group ids)
  "Validate one :ORDER group (axis id id ...) and return it.
AXIS is one of *ORDER-AXES*; at least two ids follow, each in IDS, none repeated.
Signal MALFORMED-SPEC naming GROUP when it is not."
  (unless (and (consp group) (proper-list-p group))
    (malformed group "an order group must be a proper list (axis id id ...)"))
  (unless (member (first group) *order-axes* :test #'eq)
    (malformed group (format nil "order axis must be one of ~{~S~^ or ~}" *order-axes*)))
  (let ((members (rest group)))
    (when (< (length members) 2)
      (malformed group "an order group must name at least two elements"))
    (loop for (id . later) on members
          do (unless (and (keywordp id) (member id ids :test #'eq))
               (malformed group (format nil "order names ~S, which :elements does not declare"
                                        id)))
             (when (member id later :test #'eq)
               (malformed group (format nil "order names ~S twice" id))))
    group))

(defun parse-order-clause (clause ids)
  "Validate the value of a spec's :ORDER key and return it.
CLAUSE is a proper list of groups, each checked by PARSE-ORDER-GROUP against IDS, the element
ids the spec declares. Signal MALFORMED-SPEC naming CLAUSE when it is not a proper list."
  (unless (proper-list-p clause)
    (malformed clause "spec :order must be a proper list of groups"))
  (dolist (group clause clause)
    (parse-order-group group ids)))

(defparameter *appearance-keys* '(:ink :background)
  "The keys a state's appearance may hold in a :STATES clause.")

(defun parse-state-entry (entry)
  "Validate one state entry (state [:ink colour] [:background colour]) and return
(state . appearance), APPEARANCE a plist whose colours are sRGB lists.
STATE is a keyword; each colour is anything PARSE-RGB accepts. Signal MALFORMED-SPEC naming
ENTRY when it is not well formed."
  (unless (and (consp entry) (proper-list-p entry))
    (malformed entry "a state entry must be a proper list (state :ink colour :background colour)"))
  (let ((state (first entry))
        (appearance (rest entry)))
    (unless (keywordp state)
      (malformed entry "a state name must be a keyword"))
    (unless (evenp (length appearance))
      (malformed entry "a state's appearance must be a plist of even length"))
    (multiple-value-bind (key problem) (first-bad-key appearance *appearance-keys*)
      (case problem
        (:unknown (malformed entry (format nil "unknown appearance key ~S" key)))
        (:repeated (malformed entry (format nil "appearance key ~S appears twice" key)))))
    (cons state
          (loop for (key value) on appearance by #'cddr
                for rgb = (parse-rgb value)
                unless rgb
                  do (malformed entry (format nil "appearance ~S is not a colour" key))
                collect key
                collect rgb))))

(defun parse-states-clause (clause)
  "Validate the value of a spec's :STATES key and return it as an alist from kind to a list of
(state . appearance) in the order declared.
CLAUSE is a proper list of kind entries (kind state-entry ...): KIND a keyword named by no
earlier entry, followed by at least one state entry, each checked by PARSE-STATE-ENTRY, with
state names distinct within the kind. Signal MALFORMED-SPEC naming CLAUSE when it is not a
proper list, the kind entry at fault when its shape or kind is wrong, and the later state entry
for a repeated state name."
  (unless (proper-list-p clause)
    (malformed clause "spec :states must be a proper list of kind entries"))
  (let ((kinds '()))
    (dolist (entry clause (nreverse kinds))
      (unless (and (consp entry) (proper-list-p entry))
        (malformed entry "a kind entry must be a proper list (kind (state ...) ...)"))
      (let ((kind (first entry)))
        (unless (keywordp kind)
          (malformed entry "a kind entry's kind must be a keyword"))
        (when (assoc kind kinds)
          (malformed entry (format nil "kind ~S is given states twice" kind)))
        (when (null (rest entry))
          (malformed entry "a kind entry must declare at least one state"))
        (let ((states '()))
          (dolist (state-entry (rest entry))
            (let ((state (parse-state-entry state-entry)))
              (when (assoc (car state) states)
                (malformed state-entry (format nil "state ~S is declared twice for ~S"
                                               (car state) kind)))
              (push state states)))
          (push (cons kind (nreverse states)) kinds))))))

(defun check-element-states (element clause states)
  "Signal MALFORMED-SPEC naming CLAUSE when ELEMENT names a state, by :STATE or :STATES, that
STATES, a parsed :STATES alist, does not declare for ELEMENT's kind."
  (let ((declared (mapcar #'car (cdr (assoc (spec-element-kind element) states)))))
    (dolist (state (append (and (spec-element-state element)
                                (list (spec-element-state element)))
                           (spec-element-states element)))
      (unless (member state declared :test #'eq)
        (malformed clause (format nil "state ~S is not declared for kind ~S"
                                  state (spec-element-kind element)))))))

(defun parse-spec (form)
  "Return the spec FORM describes, as a spec record.
FORM is a spec record, returned unchanged, or a plist (:view keyword :elements (clause ...)),
optionally with :order ((axis id id ...) ...), :states ((kind (state :ink c :background c) ...)
...), and :contrast (:text r :non-text r).
:ELEMENTS is a non-empty proper list of element clauses with distinct ids. Each :ORDER group
names an axis, :LEFT-TO-RIGHT or :TOP-TO-BOTTOM, and at least two distinct declared ids.
:STATES declares, per kind, the states an element may be in and how each looks; every state an
element clause names must be declared for its kind. :CONTRAST sets positive real thresholds for
either key. The order of the keys in FORM does not matter.
Signal MALFORMED-SPEC naming the offending clause when FORM is not well formed: the whole form
for a top-level key that is absent, unknown, or repeated, or a :VIEW that is not a keyword; the
:ELEMENTS value when it is not a proper list; the :ORDER value when it is not a proper list,
and the group at fault otherwise; the :STATES value, kind entry, or state entry at fault; the
:CONTRAST value when it is malformed; the element clause at fault otherwise, the later one for a
repeated id or the one naming an undeclared state. A circular list signals
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
        (setf parsed (nreverse parsed))
        (let ((states (parse-states-clause (clause-value form :states))))
          (loop for element in parsed
                for clause in elements
                do (check-element-states element clause states))
          (make-spec :view view
                     :elements parsed
                     :order (parse-order-clause (clause-value form :order)
                                                (mapcar #'spec-element-id parsed))
                     :states states
                     :contrast (parse-contrast-clause (clause-value form :contrast))))))))
