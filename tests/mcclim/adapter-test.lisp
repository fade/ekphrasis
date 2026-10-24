;;;; tests/mcclim/adapter-test.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; The walker turns every drawn label, control, and shape of the fixture header into an element
;;;; that carries the ink it is drawn in and the colour that lies beneath it.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((pkg (find-package '#:ekphrasis/tests/mcclim/adapter-test)))
    (when pkg
      (sb-ext:without-package-locks (delete-package pkg)))))

(defpackage #:ekphrasis/tests/mcclim/adapter-test
  (:use #:cl #:zebra)
  (:import-from #:ekphrasis
                #:element-background
                #:element-box
                #:element-container
                #:element-id
                #:element-ink
                #:element-kind
                #:element-state
                #:element-text
                #:snapshot-elements)
  (:import-from #:ekphrasis/mcclim)
  (:import-from #:ekphrasis/tests/mcclim/fixture
                #:render-fixture))

(in-package #:ekphrasis/tests/mcclim/adapter-test)

(defparameter *white* '(1 1 1)
  "The fixture pane's foreground, white.")

(defparameter *attached-rgb* (list (/ #x5C 255) (/ #x42 255) (/ #x1A 255))
  "The sRGB list of the fixture's attached fill.")

(defparameter *fenced-rgb* (list (/ #x80 255) (/ #x29 255) (/ #x29 255))
  "The sRGB list of the fixture's fenced fill.")

(defparameter *gray20* '(0.2 0.2 0.2)
  "The fixture pane's background, gray20.")

(defun colour-near-p (expected actual)
  "True when the sRGB lists EXPECTED and ACTUAL agree within 1/255 on every channel."
  (and (= (length expected) (length actual))
       (every (lambda (e a) (<= (abs (- e a)) 1/255)) expected actual)))

(defun rounded-box (element)
  "Return ELEMENT's box with every coordinate rounded to an integer."
  (mapcar #'round (element-box element)))

(defun clean-elements ()
  "Return the elements of the clean fixture header, in painting order."
  (snapshot-elements (render-fixture :variant :clean)))

(defun top-level (elements &optional kind)
  "Return the ELEMENTS drawn on the view itself, of KIND when KIND is given."
  (remove-if-not (lambda (element)
                   (and (null (element-container element))
                        (or (null kind) (eq kind (element-kind element)))))
                 elements))

(defun parts-of (container elements &optional kind)
  "Return the ELEMENTS whose container is CONTAINER's id, of KIND when KIND is given."
  (remove-if-not (lambda (element)
                   (and (eql (element-id container) (element-container element))
                        (or (null kind) (eq kind (element-kind element)))))
                 elements))

(defun pill-named (label elements)
  "Return the :SESSION-PILL element among ELEMENTS whose text is LABEL."
  (find-if (lambda (element)
             (and (eq :session-pill (element-kind element))
                  (equal label (element-text element))))
           elements))

(defun title-and-target (variant &optional render)
  "Return the title and the target box of the fixture header drawn as VARIANT, as two values.
RENDER is a plist of further SNAPSHOT-FRAME-PANE arguments."
  (let ((elements (snapshot-elements (render-fixture :variant variant :render render))))
    (values (first (top-level elements :text))
            (first (top-level elements :outline)))))

(define-test every-drawn-control-and-shape-is-an-element
  "The view holds the title, three pills, and the target box.
Each pill holds its eleven bands, its border, and its label."
  (let* ((elements (clean-elements))
         (titles (top-level elements :text))
         (pills (top-level elements :session-pill))
         (outlines (top-level elements :outline)))
    (is = 5 (length (top-level elements)))
    (is = 1 (length titles))
    (is string= "EKPHRASIS" (element-text (first titles)))
    (is = 3 (length pills))
    (is = 1 (length outlines))
    (is equal '(300 10 380 40) (rounded-box (first outlines)))
    (dolist (pill pills)
      (is = 13 (length (parts-of pill elements)))
      (is = 11 (length (parts-of pill elements :fill)))
      (is = 1 (length (parts-of pill elements :outline)))
      (is = 1 (length (parts-of pill elements :text))))))

(define-test presentation-kind-is-its-type
  "Each pill is an element of kind :SESSION-PILL whose text is its label."
  (let ((pills (top-level (clean-elements) :session-pill)))
    (true (every (lambda (pill) (eq :session-pill (element-kind pill))) pills))
    (is equal '("alpha" "bravo" "charlie")
        (sort (mapcar #'element-text pills) #'string<))))

(define-test nested-marks-name-their-container
  "A pill's label is a :TEXT element naming the pill as container; the pill names none."
  (let ((elements (clean-elements)))
    (dolist (label '("alpha" "bravo" "charlie"))
      (let* ((pill (pill-named label elements))
             (texts (parts-of pill elements :text)))
        (false (element-container pill))
        (is = 1 (length texts))
        (is string= label (element-text (first texts)))
        (is eql (element-id pill) (element-container (first texts)))))))

(define-test element-ids-follow-painting-order
  "Ids run 1 to n without gaps; a pill precedes its parts; charlie, drawn first, is rightmost."
  (let* ((elements (clean-elements))
         (ids (mapcar #'element-id elements))
         (pills (top-level elements :session-pill))
         (charlie (pill-named "charlie" elements)))
    (is equal (loop for id from 1 to (length elements) collect id) ids)
    (dolist (pill pills)
      (true (every (lambda (part) (< (element-id pill) (element-id part)))
                   (parts-of pill elements))))
    (is = (reduce #'min (mapcar #'element-id pills)) (element-id charlie))
    (is = (reduce #'max (mapcar (lambda (pill) (first (element-box pill))) pills))
        (first (element-box charlie)))))

(define-test indirect-ink-resolves-to-pane-colours
  "The title's indirect ink resolves to the pane's white foreground, never black.
The pill labels and the target box are white."
  (let* ((elements (clean-elements))
         (title (first (top-level elements :text)))
         (target (first (top-level elements :outline))))
    (true (colour-near-p *white* (element-ink title)) "title ink ~S" (element-ink title))
    (false (colour-near-p '(0 0 0) (element-ink title)))
    (dolist (pill (top-level elements :session-pill))
      (let ((label (first (parts-of pill elements :text))))
        (true (colour-near-p *white* (element-ink label)) "label ink ~S" (element-ink label))))
    (true (colour-near-p *white* (element-ink target)) "target ink ~S" (element-ink target))))

(define-test shape-kinds-are-classified
  "A pill's unfilled border is an :OUTLINE; its bands are :FILL elements in the drawn-state colour."
  (let ((elements (clean-elements)))
    (loop for (label rgb) in (list (list "alpha" *attached-rgb*)
                                   (list "bravo" *fenced-rgb*)
                                   (list "charlie" *attached-rgb*))
          for pill = (pill-named label elements)
          for border = (first (parts-of pill elements :outline))
          do (is equal (rounded-box pill) (rounded-box border))
             (true (colour-near-p '(0.8 0.8 0.8) (element-ink border))
                   "~A border ink ~S" label (element-ink border))
             (dolist (band (parts-of pill elements :fill))
               (true (colour-near-p rgb (element-ink band))
                     "~A band ink ~S" label (element-ink band))))))

(define-test label-background-is-the-fill-beneath-it
  "Each pill and its label sit on the pill's drawn-state fill; the title sits on gray20."
  (let ((elements (clean-elements)))
    (loop for (label rgb) in (list (list "alpha" *attached-rgb*)
                                   (list "bravo" *fenced-rgb*)
                                   (list "charlie" *attached-rgb*))
          for pill = (pill-named label elements)
          for text = (first (parts-of pill elements :text))
          do (true (colour-near-p rgb (element-background pill))
                   "~A pill background ~S" label (element-background pill))
             (true (colour-near-p rgb (element-background text))
                   "~A label background ~S" label (element-background text)))
    (let ((title (first (top-level elements :text))))
      (true (colour-near-p *gray20* (element-background title))
            "title background ~S" (element-background title)))))

(define-test outline-background-is-the-worst-edge-sample
  "The white target box sits on gray20.
Each gray80 pill border takes the pill's fill: gray80 on the fill is a lower ratio than on gray20."
  (let* ((elements (clean-elements))
         (target (first (top-level elements :outline))))
    (true (colour-near-p *gray20* (element-background target))
          "target background ~S" (element-background target))
    (true (colour-near-p *white* (element-ink target)) "target ink ~S" (element-ink target))
    (loop for (label rgb) in (list (list "alpha" *attached-rgb*)
                                   (list "bravo" *fenced-rgb*)
                                   (list "charlie" *attached-rgb*))
          for pill = (pill-named label elements)
          for border = (first (parts-of pill elements :outline))
          do (true (colour-near-p '(0.8 0.8 0.8) (element-ink border))
                   "~A border ink ~S" label (element-ink border))
             (true (colour-near-p rgb (element-background border))
                   "~A border background ~S" label (element-background border)))))

(define-test translucent-ink-composites-over-background
  "White at half opacity over gray20 is stored as 0.6 grey."
  (let ((title (title-and-target :translucent-title)))
    (true (colour-near-p '(0.6 0.6 0.6) (element-ink title)) "title ink ~S" (element-ink title))
    (true (colour-near-p *gray20* (element-background title))
          "title background ~S" (element-background title))))

(define-test flipping-ink-has-no-colour
  "A target box drawn in the flipping ink has no ink colour and still sits on gray20."
  (let ((target (nth-value 1 (title-and-target :flipping-outline))))
    (false (element-ink target))
    (true (colour-near-p *gray20* (element-background target))
          "target background ~S" (element-background target))))

(define-test ink-resolver-is-a-parameter
  "A caller-supplied resolver that makes every ink black gives the title a black ink."
  (let ((title (title-and-target :clean
                                 (list :ink-resolver
                                       (lambda (ink pane)
                                         (declare (ignore ink pane))
                                         (values '(0 0 0) 1))))))
    (is equal '(0 0 0) (element-ink title))))

(define-test low-contrast-variants-carry-their-colours
  "The 10% grey title and the black target box keep their inks, each on gray20."
  (let ((title (title-and-target :low-contrast-text)))
    (true (colour-near-p '(0.1 0.1 0.1) (element-ink title)) "title ink ~S" (element-ink title))
    (true (colour-near-p *gray20* (element-background title))
          "title background ~S" (element-background title)))
  (let ((target (nth-value 1 (title-and-target :low-contrast-outline))))
    (true (colour-near-p '(0 0 0) (element-ink target)) "target ink ~S" (element-ink target))
    (true (colour-near-p *gray20* (element-background target))
          "target background ~S" (element-background target))))

(define-test fill-elements-sample-earlier-layers-only
  "Every pill band sits on gray20: the bands do not overlap, and a band never lies beneath itself."
  (let ((elements (clean-elements)))
    (dolist (pill (top-level elements :session-pill))
      (dolist (band (parts-of pill elements :fill))
        (true (colour-near-p *gray20* (element-background band))
              "~A band ~A background ~S"
              (element-text pill) (element-id band) (element-background band))))))

(define-test client-reported-state-is-recorded
  "With the fixture's state function each pill, taken from the left, carries its session's state,
and no other element carries one; with the state function NIL no element does."
  (flet ((states (render)
           (mapcar (lambda (element) (list (element-text element) (element-state element)))
                   (sort (top-level (snapshot-elements
                                     (render-fixture :variant :clean :render render))
                                    :session-pill)
                         #'< :key (lambda (element) (first (element-box element)))))))
    (is equal '(("alpha" :attached) ("bravo" :fenced) ("charlie" :attached)) (states nil))
    (is equal '(("alpha" nil) ("bravo" nil) ("charlie" nil))
        (states (list :state-function nil))))
  (is equal '(:session-pill :session-pill :session-pill)
      (mapcar #'element-kind (remove nil (clean-elements) :key #'element-state)))
  (is equal '() (remove nil (snapshot-elements (render-fixture
                                                :variant :clean
                                                :render (list :state-function nil)))
                        :key #'element-state)))

(define-test state-function-must-return-a-keyword
  "A state function returning a string signals an error naming the presentation type and the
value."
  (let ((render (list :state-function (lambda (session)
                                        (declare (ignore session))
                                        "attached"))))
    (fail (render-fixture :variant :clean :render render) error)
    (let ((text (handler-case (progn (render-fixture :variant :clean :render render) nil)
                  (error (condition) (princ-to-string condition)))))
      (true (and text (search "SESSION-PILL" text)) "~A" text)
      (true (and text (search "\"attached\"" text)) "~A" text))))
