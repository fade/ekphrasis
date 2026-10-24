;;;; tests/compare-test.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; Presence and labels over hand-built snapshots: missing, mislabelled, and unexpected elements.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((pkg (find-package '#:ekphrasis/tests/compare-test)))
    (when pkg
      (sb-ext:without-package-locks (delete-package pkg)))))

(defpackage #:ekphrasis/tests/compare-test
  (:use #:cl #:zebra)
  (:import-from #:ekphrasis
                #:compare-snapshot
                #:failing-findings
                #:finding-box
                #:finding-check
                #:finding-element-id
                #:finding-expected
                #:finding-measured
                #:finding-severity
                #:finding-spec-id
                #:finding-where
                #:finding-why
                #:make-element
                #:make-snapshot))

(in-package #:ekphrasis/tests/compare-test)

(defun header-spec ()
  "Return a spec naming the header's title and two session pills."
  (list :view :header
        :elements (list (list :title :kind :text :label "EKPHRASIS")
                        (list :alpha :kind :session-pill :label "alpha")
                        (list :bravo :kind :session-pill :label "bravo"))))

(defun pill (id label left)
  "Return a session pill element ID labelled LABEL at LEFT, and its label and band inside it."
  (list (make-element :id id :kind :session-pill :text label :box (list left 8 (+ left 64) 30))
        (make-element :id (+ id 1) :kind :text :text label :container id
                      :box (list (+ left 12) 15 (+ left 52) 28))
        (make-element :id (+ id 2) :kind :fill :container id
                      :box (list left 8 (+ left 64) 10))))

(defun header-snapshot (&key (bravo "bravo") extra)
  "Return a hand-built snapshot of the header: the title, alpha, a pill labelled BRAVO unless
BRAVO is NIL, a target outline, and EXTRA, a further label drawn as a pill at x 520."
  (make-snapshot
   :view :header
   :elements (append (list (make-element :id 1 :kind :text :text "EKPHRASIS"
                                         :box (list 16 14 133 36)))
                     (pill 2 "alpha" 660)
                     (when bravo (pill 5 bravo 732))
                     (list (make-element :id 8 :kind :outline :box (list 300 10 380 40)))
                     (when extra (pill 9 extra 520)))))

(defun cases ()
  "Return the findings of every defect case: removed, relabelled, and extra elements."
  (list (compare-snapshot (header-snapshot :bravo nil) (header-spec))
        (compare-snapshot (header-snapshot :bravo "brovo") (header-spec))
        (compare-snapshot (header-snapshot :extra "delta") (header-spec))))

(define-test conforming-snapshot-has-no-findings
  "A snapshot drawing every spec element with its label gives no findings."
  (is equal '() (compare-snapshot (header-snapshot) (header-spec))))

(define-test removed-element-is-missing
  "A spec element with no drawn counterpart gives exactly one :MISSING finding, with no box."
  (let ((findings (compare-snapshot (header-snapshot :bravo nil) (header-spec))))
    (is = 1 (length findings))
    (is eq :missing (finding-check (first findings)))
    (is eq :bravo (finding-spec-id (first findings)))
    (false (finding-box (first findings)))))

(define-test relabelled-element-is-a-label-finding
  "A drawn element of the right kind with other text gives one :LABEL finding located on it."
  (let* ((findings (compare-snapshot (header-snapshot :bravo "brovo") (header-spec)))
         (finding (first findings)))
    (is = 1 (length findings))
    (is eq :label (finding-check finding))
    (is eq :error (finding-severity finding))
    (is eq :bravo (finding-spec-id finding))
    (is string= "bravo" (finding-expected finding))
    (is string= "brovo" (finding-measured finding))
    (is = 5 (finding-element-id finding))
    (is equal '(732 8 796 30) (finding-box finding))))

(define-test extra-element-is-an-unexpected-warning
  "An unpaired element of a kind the spec names gives one :UNEXPECTED warning, which does not fail."
  (let* ((findings (compare-snapshot (header-snapshot :extra "delta") (header-spec)))
         (finding (first findings)))
    (is = 1 (length findings))
    (is eq :unexpected (finding-check finding))
    (is eq :warning (finding-severity finding))
    (false (finding-spec-id finding))
    (is = 9 (finding-element-id finding))
    (is string= "delta" (finding-measured finding))
    (is equal '() (failing-findings findings))))

(define-test nested-elements-are-not-judged-for-presence
  "Elements inside a container, and kinds the spec does not name, give no presence findings."
  (let ((spec (list :view :header
                    :elements (list (list :title :kind :text :label "EKPHRASIS")))))
    (is equal '() (compare-snapshot (header-snapshot :extra "delta") spec))))

(define-test every-finding-carries-where-and-why
  "Every finding says where (view, and box unless missing) and why (check, values, evidence)."
  (dolist (finding (apply #'append (cases)))
    (let ((where (finding-where finding))
          (why (finding-why finding)))
      (is eq :header (getf where :view))
      (true (member :container where))
      (true (member :element-id where))
      (if (eq :missing (finding-check finding))
          (false (getf where :box))
          (true (getf where :box) "~S has no box" (finding-check finding)))
      (true (member (getf why :check) '(:missing :label :unexpected)))
      (true (member :expected why))
      (true (member :measured why))
      (is equal '(:recorded) (getf why :evidence)))))
