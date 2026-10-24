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
                #:contrast-ratio
                #:failing-findings
                #:finding-box
                #:finding-check
                #:finding-container
                #:finding-detail
                #:finding-element-id
                #:finding-expected
                #:finding-measured
                #:finding-severity
                #:finding-spec-id
                #:finding-where
                #:finding-why
                #:make-element
                #:make-snapshot
                #:make-spec
                #:make-spec-element))

(in-package #:ekphrasis/tests/compare-test)

(defun header-spec ()
  "Return a spec naming the header's title and two session pills."
  (list :view :header
        :elements (list (list :title :kind :text :label "EKPHRASIS")
                        (list :alpha :kind :session-pill :label "alpha")
                        (list :bravo :kind :session-pill :label "bravo"))))

(defun pill (id label left)
  "Return a session pill element ID labelled LABEL at LEFT, and its label and band inside it.
The label is white on the pane's gray20."
  (list (make-element :id id :kind :session-pill :text label :box (list left 8 (+ left 64) 30))
        (make-element :id (+ id 1) :kind :text :text label :container id
                      :box (list (+ left 12) 15 (+ left 52) 28)
                      :ink (list 1 1 1) :background (list 0.2 0.2 0.2))
        (make-element :id (+ id 2) :kind :fill :container id
                      :box (list left 8 (+ left 64) 10))))

(defun header-snapshot (&key (bravo "bravo") extra)
  "Return a hand-built snapshot of the header: the title, alpha, a pill labelled BRAVO unless
BRAVO is NIL, a target outline, and EXTRA, a further label drawn as a pill at x 520.
Every text and outline mark is white on gray20, well above both contrast thresholds."
  (make-snapshot
   :view :header
   :elements (append (list (make-element :id 1 :kind :text :text "EKPHRASIS"
                                         :box (list 16 14 133 36)
                                         :ink (list 1 1 1) :background (list 0.2 0.2 0.2)))
                     (pill 2 "alpha" 660)
                     (when bravo (pill 5 bravo 732))
                     (list (make-element :id 8 :kind :outline :box (list 300 10 380 40)
                                         :ink (list 1 1 1) :background (list 0.2 0.2 0.2)))
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

(defun mark (kind ink background &key (id 12) container text (box (list 299 9 381 41)))
  "Return an element of KIND drawn in INK over BACKGROUND, both sRGB lists or NIL.
It is element ID at BOX inside CONTAINER, drawing TEXT."
  (make-element :id id :kind kind :text text :box box :container container
                :ink ink :background background))

(defun mark-snapshot (&rest elements)
  "Return a snapshot of the header view drawing ELEMENTS."
  (make-snapshot :view :header :elements elements))

(defun spec-for (kind &optional contrast)
  "Return a spec record for the header naming one unlabelled element of KIND, with the :CONTRAST
clause CONTRAST."
  (make-spec :view :header
             :elements (list (make-spec-element :id :target :kind kind))
             :contrast contrast))

(defun black-outline ()
  "Return the snapshot of one black outline, element 12, on gray20: 1.662 to 1, below 3.0."
  (mark-snapshot (mark :outline (list 0 0 0) (list 0.2 0.2 0.2))))

(define-test low-contrast-mark-is-not-perceivable
  "An outline below the non-text threshold gives one located :CONTRAST error with its ratio,
threshold, and colours."
  (let* ((findings (compare-snapshot (black-outline) (spec-for :outline)))
         (finding (first findings))
         (detail (finding-detail finding)))
    (is = 1 (length findings))
    (is eq :contrast (finding-check finding))
    (is eq :error (finding-severity finding))
    (true (< (abs (- (finding-measured finding) 1.662)) 0.005) "measured ~A"
          (finding-measured finding))
    (is = 3.0 (finding-expected finding))
    (is = 3.0 (getf detail :threshold))
    (is = (finding-measured finding) (getf detail :ratio))
    (is string= "#000000" (getf detail :ink))
    (is string= "#333333" (getf detail :background))
    (is eq :non-text (getf detail :category))
    (is = 12 (finding-element-id finding))
    (is equal '(299 9 381 41) (finding-box finding))))

(define-test threshold-is-strict
  "A ratio equal to its threshold passes; a threshold above it by 1d-6 fails it."
  (let* ((ink (list 0.53 0.53 0.53))
         (background (list 0.2 0.2 0.2))
         (ratio (contrast-ratio ink background))
         (snapshot (mark-snapshot (mark :text ink background))))
    (is equal '() (compare-snapshot snapshot (spec-for :text) :thresholds (list :text ratio)))
    (is = 1 (length (compare-snapshot snapshot (spec-for :text)
                                      :thresholds (list :text (+ ratio 1d-6)))))))

(define-test spec-threshold-overrides-default
  "A spec's :CONTRAST clause replaces the default threshold for the keys it sets."
  (is equal '() (compare-snapshot (black-outline) (spec-for :outline (list :non-text 1.5)))))

(define-test call-threshold-overrides-spec
  "A threshold given to the call replaces the spec's, and the finding records the one applied."
  (let ((findings (compare-snapshot (black-outline) (spec-for :outline (list :non-text 1.5))
                                    :thresholds (list :non-text 2.0))))
    (is = 1 (length findings))
    (is eq :contrast (finding-check (first findings)))
    (is = 2.0 (finding-expected (first findings)))))

(define-test text-and-non-text-thresholds-differ
  "One colour pair at about 3.5 to 1 fails as text, against 4.5, and passes as an outline,
against 3.0."
  (let ((ink (list 0.53 0.53 0.53))
        (background (list 0.2 0.2 0.2)))
    (let ((findings (compare-snapshot (mark-snapshot (mark :text ink background))
                                      (spec-for :text))))
      (is = 1 (length findings))
      (is eq :contrast (finding-check (first findings)))
      (is eq :text (getf (finding-detail (first findings)) :category))
      (is = 4.5 (finding-expected (first findings))))
    (is equal '() (compare-snapshot (mark-snapshot (mark :outline ink background))
                                    (spec-for :outline)))))

(define-test unresolved-ink-is-a-warning
  "A mark whose ink has no fixed colour gives one :UNRESOLVED-INK warning, which does not fail."
  (let* ((findings (compare-snapshot (mark-snapshot (mark :outline nil (list 0.2 0.2 0.2)))
                                     (spec-for :outline)))
         (finding (first findings)))
    (is = 1 (length findings))
    (is eq :unresolved-ink (finding-check finding))
    (is eq :warning (finding-severity finding))
    (is equal '(:ink) (getf (finding-detail finding) :unresolved))
    (is = 12 (finding-element-id finding))
    (is equal '() (failing-findings findings))))

(define-test fills-and-controls-are-not-judged-for-contrast
  "A fill and a session pill drawn in their background's own colour give no finding."
  (let ((grey (list 0.2 0.2 0.2)))
    (is equal '()
        (compare-snapshot (mark-snapshot (mark :session-pill grey grey :id 1)
                                         (mark :fill grey grey :id 2))
                          (spec-for :session-pill)))))

(define-test contrast-finding-names-its-spec-element
  "A faint top-level mark paired with a spec element carries its spec id; a faint mark inside a
container carries no spec id and names its container."
  (let* ((faint (list 0.1 0.1 0.1))
         (grey (list 0.2 0.2 0.2))
         (spec (list :view :header
                     :elements (list (list :title :kind :text :label "EKPHRASIS")
                                     (list :alpha :kind :session-pill :label "alpha"))))
         (findings (compare-snapshot
                    (mark-snapshot (mark :text faint grey :id 1 :text "EKPHRASIS")
                                   (mark :session-pill nil nil :id 2 :text "alpha")
                                   (mark :text faint grey :id 3 :text "alpha" :container 2))
                    spec)))
    (is equal '(:contrast :contrast) (mapcar #'finding-check findings))
    (is equal '(1 3) (mapcar #'finding-element-id findings))
    (is eq :title (finding-spec-id (first findings)))
    (false (finding-container (first findings)))
    (false (finding-spec-id (second findings)))
    (is eql 2 (finding-container (second findings)))))

(defun ordered-spec (&key (axis :left-to-right) (ids (list :alpha :bravo :charlie)))
  "Return a spec naming the session pills alpha, bravo, and charlie, with one :ORDER group
along AXIS over IDS."
  (list :view :header
        :elements (list (list :alpha :kind :session-pill :label "alpha")
                        (list :bravo :kind :session-pill :label "bravo")
                        (list :charlie :kind :session-pill :label "charlie"))
        :order (list (list* axis ids))))

(defun placed-pills (&rest placements)
  "Return a header snapshot of session pills, one per PLACEMENTS entry (label left top), in the
order given, which is their painting order. Each pill is 64 by 22."
  (make-snapshot
   :view :header
   :elements (loop for (label left top) in placements
                   for id from 1
                   collect (make-element :id id :kind :session-pill :text label
                                         :box (list left top (+ left 64) (+ top 22))))))

(define-test swapped-elements-give-one-order-finding
  "Bravo drawn left of alpha gives exactly one :ORDER error for the group, with the expected and
drawn sequences, one displaced id, and the box around all three pills."
  (let* ((findings (compare-snapshot (placed-pills (list "alpha" 650 8)
                                                   (list "bravo" 577 8)
                                                   (list "charlie" 818 8))
                                     (ordered-spec)))
         (finding (first findings))
         (detail (finding-detail finding)))
    (is = 1 (length findings))
    (is eq :order (finding-check finding))
    (is eq :error (finding-severity finding))
    (false (finding-spec-id finding))
    (false (finding-element-id finding))
    (is equal '(:alpha :bravo :charlie) (finding-expected finding))
    (is equal '(:bravo :alpha :charlie) (finding-measured finding))
    (is eq :left-to-right (getf detail :axis))
    (true (member (getf detail :displaced) '((:alpha) (:bravo)) :test #'equal)
          "displaced ~S" (getf detail :displaced))
    (is equal '(577 8 882 30) (finding-box finding))))

(define-test order-is-judged-on-position-not-drawing-order
  "Pills painted charlie, bravo, alpha but placed alpha, bravo, charlie from the left are in order."
  (is equal '() (compare-snapshot (placed-pills (list "charlie" 818 8)
                                                (list "bravo" 734 8)
                                                (list "alpha" 650 8))
                                  (ordered-spec))))

(define-test order-ignores-missing-members
  "With bravo not drawn, alpha left of charlie gives the :MISSING finding and no :ORDER finding."
  (let ((findings (compare-snapshot (placed-pills (list "alpha" 650 8)
                                                  (list "charlie" 818 8))
                                    (ordered-spec))))
    (is equal '(:missing) (mapcar #'finding-check findings))
    (is eq :bravo (finding-spec-id (first findings)))))

(define-test top-to-bottom-groups-use-the-vertical-axis
  "A :TOP-TO-BOTTOM group is judged on box top: a column drawn alpha, bravo, charlie downward is
in order whatever the left edges, and one with charlie on top is not."
  (is equal '() (compare-snapshot (placed-pills (list "alpha" 900 10)
                                                (list "bravo" 100 40)
                                                (list "charlie" 500 70))
                                  (ordered-spec :axis :top-to-bottom)))
  (let ((findings (compare-snapshot (placed-pills (list "alpha" 100 40)
                                                  (list "bravo" 100 70)
                                                  (list "charlie" 100 10))
                                    (ordered-spec :axis :top-to-bottom))))
    (is = 1 (length findings))
    (is eq :order (finding-check (first findings)))
    (is equal '(:charlie :alpha :bravo) (finding-measured (first findings)))
    (is eq :top-to-bottom (getf (finding-detail (first findings)) :axis))
    (is equal '(:charlie) (getf (finding-detail (first findings)) :displaced))))
