;;;; tests/spec-test.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; A spec states a view's elements as data; every malformed clause is named by the condition.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((pkg (find-package '#:ekphrasis/tests/spec-test)))
    (when pkg
      (sb-ext:without-package-locks (delete-package pkg)))))

(defpackage #:ekphrasis/tests/spec-test
  (:use #:cl #:zebra)
  (:import-from #:ekphrasis
                #:malformed-spec
                #:malformed-spec-clause
                #:malformed-spec-reason
                #:parse-rgb
                #:parse-spec
                #:spec-contrast
                #:spec-elements
                #:spec-order
                #:spec-states
                #:spec-element-id
                #:spec-element-kind
                #:spec-element-label
                #:spec-element-state
                #:spec-element-states
                #:spec-view))

(in-package #:ekphrasis/tests/spec-test)

(defun title-clause ()
  "Return a fresh, well-formed element clause for the header's title."
  (list :title :kind :text :label "EKPHRASIS"))

(defun header-spec (&rest clauses)
  "Return a fresh spec form for the header view whose elements are CLAUSES."
  (list :view :header :elements clauses))

(defun signalled (form)
  "Return the MALFORMED-SPEC condition PARSE-SPEC signals for FORM, or NIL when it signals none."
  (handler-case (progn (parse-spec form) nil)
    (malformed-spec (condition) condition)))

(defun names-clause (form clause what)
  "Assert that parsing FORM signals MALFORMED-SPEC whose clause is CLAUSE itself.
WHAT describes the case for the failure text."
  (fail (parse-spec form) malformed-spec "~A: no malformed-spec" what)
  (let ((condition (signalled form)))
    (true (and condition (eq clause (malformed-spec-clause condition)))
          "~A: clause named ~S" what (and condition (malformed-spec-clause condition)))))

(defun circular (&rest items)
  "Return a circular list cycling through ITEMS."
  (let ((cycle (copy-list items)))
    (setf (cdr (last cycle)) cycle)
    cycle))

(define-test valid-spec-keeps-elements-in-order
  "A well-formed spec keeps its view and its elements in order; :LABEL is optional."
  (let* ((form '(:view :header
                 :elements ((:title :kind :text :label "EKPHRASIS")
                            (:alpha :kind :session-pill :label "alpha")
                            (:bravo :kind :session-pill :label "bravo")
                            (:charlie :kind :session-pill :label "charlie"))))
         (spec (parse-spec form))
         (elements (spec-elements spec)))
    (is eq :header (spec-view spec))
    (is equal '(:title :alpha :bravo :charlie) (mapcar #'spec-element-id elements))
    (is equal '(:text :session-pill :session-pill :session-pill)
        (mapcar #'spec-element-kind elements))
    (is equal '("EKPHRASIS" "alpha" "bravo" "charlie") (mapcar #'spec-element-label elements))
    (is eq spec (parse-spec spec)))
  (let ((unlabelled (first (spec-elements (parse-spec (header-spec (list :box :kind :outline)))))))
    (is eq :box (spec-element-id unlabelled))
    (is eq :outline (spec-element-kind unlabelled))
    (false (spec-element-label unlabelled))))

(define-test malformed-clauses-are-named
  "Each malformed element clause is named by the condition, not the spec around it."
  (dolist (entry (list (list "id a string" (list "alpha" :kind :session-pill :label "alpha"))
                      (list "id missing" (list))
                      (list ":kind missing" (list :alpha :label "alpha"))
                      (list ":kind a string" (list :alpha :kind "session-pill" :label "alpha"))
                      (list ":label a symbol" (list :alpha :kind :session-pill :label 'alpha))
                      (list "unknown key :colour" (list :alpha :kind :session-pill :colour "#fff"))
                      (list "odd plist" (list :alpha :kind :session-pill :label))))
    (destructuring-bind (what clause) entry
      (names-clause (header-spec (title-clause) clause) clause what))))

(define-test duplicate-id-is-named
  "Two clauses with one id name the later clause."
  (let ((earlier (list :alpha :kind :session-pill :label "alpha"))
        (later (list :alpha :kind :session-pill :label "bravo")))
    (names-clause (header-spec (title-clause) earlier later) later "duplicate :alpha")))

(define-test unknown-top-level-key-is-named
  "An unknown top-level key names the whole spec form."
  (let ((form (list :view :header :theme :dark :elements (list (title-clause)))))
    (names-clause form form "top-level :theme")))

(define-test missing-view-is-named
  "A spec with no :VIEW, or a :VIEW that is not a keyword, names the whole spec form."
  (let ((no-view (list :elements (list (title-clause))))
        (string-view (list :view "header" :elements (list (title-clause)))))
    (names-clause no-view no-view "no :view")
    (names-clause string-view string-view "string :view")))

(define-test circular-spec-is-rejected
  "A circular element list, element clause, or spec form signals instead of looping."
  (fail (parse-spec (list :view :header :elements (circular (title-clause)))) malformed-spec)
  (fail (parse-spec (header-spec (circular :alpha :kind :session-pill))) malformed-spec)
  (fail (parse-spec (circular :view :header)) malformed-spec))

(define-test malformed-report-names-clause
  "The condition's report prints the offending clause and the reason."
  (let* ((clause (list :alpha :kind "session-pill" :label "alpha"))
         (condition (signalled (header-spec (title-clause) clause)))
         (text (princ-to-string condition)))
    (true condition)
    (true (search (prin1-to-string clause) text) "~A" text)
    (true (search (malformed-spec-reason condition) text) "~A" text)))

(define-test contrast-clause-parses
  "A :CONTRAST clause is kept as the spec's contrast; a spec without one has none."
  (is equal '(:text 3.0)
      (spec-contrast (parse-spec (list :view :header
                                       :elements (list (title-clause))
                                       :contrast (list :text 3.0)))))
  (is equal '(:text 4.0 :non-text 2.5)
      (spec-contrast (parse-spec (list :view :header
                                       :elements (list (title-clause))
                                       :contrast (list :text 4.0 :non-text 2.5)))))
  (false (spec-contrast (parse-spec (header-spec (title-clause))))))

(define-test malformed-contrast-is-named
  "Each malformed :CONTRAST value is named by the condition: an unknown key, a threshold that is
zero, negative, or not a real, an odd plist, a repeated key, and a value that is not a list."
  (dolist (entry (list (list "unknown key :large" (list :large 3.0))
                       (list "zero threshold" (list :text 0))
                       (list "negative threshold" (list :text -1))
                       (list "string threshold" (list :text "4.5"))
                       (list "key with no value" (list :text))
                       (list "repeated key" (list :text 4.5 :text 3.0))
                       (list "not a list" "4.5")))
    (destructuring-bind (what clause) entry
      (names-clause (list :view :header :elements (list (title-clause)) :contrast clause)
                    clause what))))

(defun pill-clauses ()
  "Return fresh element clauses for three session pills, alpha, bravo, and charlie."
  (list (list :alpha :kind :session-pill :label "alpha")
        (list :bravo :kind :session-pill :label "bravo")
        (list :charlie :kind :session-pill :label "charlie")))

(define-test order-clause-parses
  "An :ORDER clause keeps each group as given, whether it precedes or follows :ELEMENTS; a spec
without one has none."
  (is equal '((:left-to-right :alpha :bravo :charlie))
      (spec-order (parse-spec (list :view :header
                                    :elements (pill-clauses)
                                    :order (list (list :left-to-right :alpha :bravo :charlie))))))
  (is equal '((:top-to-bottom :charlie :alpha))
      (spec-order (parse-spec (list :order (list (list :top-to-bottom :charlie :alpha))
                                    :view :header
                                    :elements (pill-clauses)))))
  (false (spec-order (parse-spec (list :view :header :elements (pill-clauses))))))

(define-test malformed-order-is-named
  "Each malformed :ORDER group is named by the condition: an unknown axis, an undeclared id, a
repeated id, a group of one id, and a group that is not a list. An :ORDER value that is not a
list is named itself."
  (dolist (entry (list (list "axis :diagonal" (list :diagonal :alpha :bravo))
                       (list "undeclared :delta" (list :left-to-right :alpha :delta))
                       (list "repeated :alpha" (list :left-to-right :alpha :bravo :alpha))
                       (list "one id" (list :left-to-right :alpha))
                       (list "not a list" :alpha)))
    (destructuring-bind (what group) entry
      (let ((form (list :view :header
                        :elements (pill-clauses)
                        :order (list (list :left-to-right :alpha :bravo) group))))
        (names-clause form group what))))
  (let ((order "alpha bravo"))
    (names-clause (list :view :header :elements (pill-clauses) :order order)
                  order "order a string")))

(defun pill-states ()
  "Return a fresh :STATES value declaring :ATTACHED and :FENCED session pills by background."
  (list (list :session-pill
              (list :attached :background "#5C421A")
              (list :fenced :background "#802929"))))

(define-test states-clause-parses
  "A :STATES clause is kept per kind, its states in order with colours parsed to sRGB lists;
element :STATE and :STATES are kept, whether :STATES precedes or follows :ELEMENTS."
  (let* ((spec (parse-spec (list :states (pill-states)
                                 :view :header
                                 :elements (list (list :alpha :kind :session-pill
                                                       :label "alpha" :state :attached)
                                                 (list :bravo :kind :session-pill
                                                       :label "bravo"
                                                       :states (list :attached :fenced))))))
         (pill (cdr (assoc :session-pill (spec-states spec)))))
    (is equal '(:attached :fenced) (mapcar #'car pill))
    (is equal (list :background (parse-rgb "#5C421A")) (cdr (assoc :attached pill)))
    (is equal (list :background (parse-rgb "#802929")) (cdr (assoc :fenced pill)))
    (is eq :attached (spec-element-state (first (spec-elements spec))))
    (false (spec-element-states (first (spec-elements spec))))
    (false (spec-element-state (second (spec-elements spec))))
    (is equal '(:attached :fenced) (spec-element-states (second (spec-elements spec)))))
  (false (spec-states (parse-spec (list :view :header :elements (pill-clauses))))))

(define-test malformed-states-are-named
  "Each malformed :STATES entry is named by the condition: an unparsable colour, an unknown
appearance key, and a repeated state name name the state entry; a kind that is not a keyword
names the kind entry; a :STATES value that is not a list is named itself."
  (flet ((states-form (states)
           (list :view :header :elements (pill-clauses) :states states)))
    (dolist (entry (list (list "unparsable colour" (list :fenced :background "#80292"))
                         (list "unknown key :border" (list :fenced :border "#802929"))))
      (destructuring-bind (what state-entry) entry
        (names-clause (states-form (list (list :session-pill
                                                (list :attached :background "#5C421A")
                                                state-entry)))
                      state-entry what)))
    (let ((again (list :attached :background "#802929")))
      (names-clause (states-form (list (list :session-pill
                                              (list :attached :background "#5C421A")
                                              again)))
                    again "repeated :attached"))
    (let ((kind-entry (list "session-pill" (list :attached :background "#5C421A"))))
      (names-clause (states-form (list kind-entry)) kind-entry "kind a string"))
    (let ((states "attached"))
      (names-clause (states-form states) states "states a string"))))

(define-test undeclared-state-is-named
  "An element whose :STATE or :STATES names a state not declared for its kind names the element
clause, as does one whose :STATE is not a keyword."
  (dolist (entry (list (list ":state :sleeping"
                             (list :bravo :kind :session-pill :state :sleeping))
                       (list ":states with :sleeping"
                             (list :bravo :kind :session-pill :states (list :fenced :sleeping)))
                       (list ":state a string"
                             (list :bravo :kind :session-pill :state "fenced"))))
    (destructuring-bind (what clause) entry
      (names-clause (list :view :header
                          :elements (list (list :alpha :kind :session-pill) clause)
                          :states (pill-states))
                    clause what))))
