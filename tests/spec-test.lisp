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
                #:parse-spec
                #:spec-contrast
                #:spec-elements
                #:spec-element-id
                #:spec-element-kind
                #:spec-element-label
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
