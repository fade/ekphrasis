;;;; tests/contrast-test.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; WCAG 2.2 contrast against reference values, colour parsing from spec data, and thresholds.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((pkg (find-package '#:ekphrasis/tests/contrast-test)))
    (when pkg
      (sb-ext:without-package-locks (delete-package pkg)))))

(defpackage #:ekphrasis/tests/contrast-test
  (:use #:cl #:zebra)
  (:import-from #:ekphrasis
                #:below-threshold-p
                #:composite-over
                #:contrast-ratio
                #:parse-rgb
                #:resolve-thresholds
                #:rgb-match-p
                #:rgb-to-hex))

(in-package #:ekphrasis/tests/contrast-test)

(defun near-p (expected actual tolerance)
  "True when the reals EXPECTED and ACTUAL differ by at most TOLERANCE."
  (<= (abs (- expected actual)) tolerance))

(defun rgb-near-p (expected actual tolerance)
  "True when the sRGB lists EXPECTED and ACTUAL agree within TOLERANCE on every channel."
  (and (= 3 (length expected) (length actual))
       (every (lambda (e a) (near-p e a tolerance)) expected actual)))

(defun circular-list ()
  "Return a circular list whose three conses repeat forever."
  (let ((list (list 0 0 0)))
    (setf (cdr (last list)) list)
    list))

(define-test wcag-reference-vectors
  "Contrast ratios match the WCAG 2.2 reference values within 0.001."
  (loop for (a b expected) in (list (list '(1 1 1) '(0 0 0) 21.000)
                                    (list '(0 0 0) (parse-rgb "#14151D") 1.155)
                                    (list '(0 0 0) '(0.2 0.2 0.2) 1.662)
                                    (list '(0 0 0) '(0.498 0.498 0.498) 5.244)
                                    (list '(0.1 0.1 0.1) (parse-rgb "#14151D") 1.040))
        for ratio = (contrast-ratio a b)
        do (true (near-p expected ratio 0.001) "~S on ~S: ~S, expected ~S" a b ratio expected)))

(define-test ratio-is-symmetric
  "Swapping the two colours leaves the ratio unchanged."
  (let ((a '(0.1 0.1 0.1))
        (b (parse-rgb "#5C421A")))
    (is = (contrast-ratio a b) (contrast-ratio b a))))

(define-test threshold-comparison-is-strict
  "A ratio fails only when strictly below the threshold, with no rounding."
  (true (below-threshold-p 4.499 4.5))
  (false (below-threshold-p 4.5 4.5)))

(define-test parse-rgb-accepts-hex-and-triples
  "Hex strings parse in either case; a triple of reals in [0,1] is returned as three reals."
  (let ((expected (list (/ 92 255) (/ 66 255) (/ 26 255))))
    (true (rgb-near-p expected (parse-rgb "#5C421A") 1d-9))
    (true (rgb-near-p expected (parse-rgb "#5c421a") 1d-9)))
  (let ((triple (parse-rgb '(0.2 0.2 0.2))))
    (is = 3 (length triple))
    (true (every #'realp triple))
    (true (rgb-near-p '(0.2 0.2 0.2) triple 1d-9))))

(define-test parse-rgb-rejects-malformed
  "Malformed designators give NIL.
They are a short string, a non-hex digit, a missing #, an out-of-range channel, two channels, a
symbol, and a circular list."
  (dolist (designator (list "#5C42" "#GG0000" "5C421A" '(1.2 0 0) '(0.1 0.2) 'gray20
                            (circular-list)))
    (false (parse-rgb designator))))

(define-test hex-round-trip
  "A parsed hex colour prints back unchanged; 0.2 grey prints as #333333."
  (is string= "#5C421A" (rgb-to-hex (parse-rgb "#5C421A")))
  (is string= "#333333" (rgb-to-hex '(0.2 0.2 0.2))))

(define-test composite-over-mixes-channels
  "Half-opaque white over 0.2 grey is 0.6 grey."
  (true (rgb-near-p '(0.6 0.6 0.6) (composite-over '(1 1 1) 0.5 '(0.2 0.2 0.2)) 1d-9)))

(define-test rgb-match-tolerates-eight-bit-rounding
  "Colours one 8-bit step apart match; three steps apart do not."
  (let ((base (list (/ 92 255) (/ 66 255) (/ 26 255))))
    (true (rgb-match-p base (list (/ 93 255) (/ 66 255) (/ 25 255))))
    (false (rgb-match-p base (list (/ 95 255) (/ 66 255) (/ 26 255))))))

(define-test thresholds-resolve-call-then-spec-then-default
  "Each threshold comes from the call, else the spec, else the defaults."
  (is equal '(:text 4.5 :non-text 3.0) (resolve-thresholds))
  (let ((resolved (resolve-thresholds nil '(:text 3.0))))
    (is = 3.0 (getf resolved :text))
    (is = 3.0 (getf resolved :non-text)))
  (let ((resolved (resolve-thresholds '(:non-text 1.5) '(:text 3.0))))
    (is = 3.0 (getf resolved :text))
    (is = 1.5 (getf resolved :non-text))))
