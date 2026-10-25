;;;; src/rgb.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; sRGB colours as lists of three reals in [0,1]: parsing from spec data, printing as hex,
;;;; matching at 8-bit precision, and compositing a translucent ink over a background.

(defpackage #:ekphrasis/src/rgb
  (:use #:cl)
  (:export #:parse-rgb
           #:rgb-to-hex
           #:rgb-match-p
           #:composite-over))

(in-package #:ekphrasis/src/rgb)

(defun three-element-list-p (object)
  "True when OBJECT is a proper list of exactly three elements.
Dotted and circular lists return NIL."
  (and (consp object)
       (consp (cdr object))
       (consp (cddr object))
       (null (cdddr object))))

(defun unit-real-p (object)
  "True when OBJECT is a real in [0,1]."
  (and (realp object) (<= 0 object 1)))

(defun parse-hex-rgb (string)
  "Return the sRGB list a \"#RRGGBB\" STRING names, either case, or NIL when STRING is malformed.
Each digit is validated before parsing; the Lisp reader is never used."
  (when (and (= 7 (length string))
             (char= #\# (char string 0))
             (every (lambda (char) (digit-char-p char 16)) (subseq string 1)))
    (loop for start from 1 below 7 by 2
          collect (/ (parse-integer string :start start :end (+ start 2) :radix 16) 255))))

(defun parse-rgb (designator)
  "Return DESIGNATOR as an sRGB list of three reals in [0,1], or NIL when it names no colour.
DESIGNATOR is a \"#RRGGBB\" string, either case, or a proper list of three reals in [0,1].
Anything else returns NIL, including dotted and circular lists."
  (typecase designator
    (string (parse-hex-rgb designator))
    (cons (when (and (three-element-list-p designator)
                     (every #'unit-real-p designator))
            (copy-list designator)))
    (t nil)))

(defun eight-bit (channel)
  "Return CHANNEL, a real in [0,1], as the nearest 8-bit value."
  (round (* channel 255)))

(defun rgb-to-hex (rgb)
  "Return the sRGB list RGB as an uppercase \"#RRGGBB\" string."
  (format nil "#~:@(~{~2,'0X~}~)" (mapcar #'eight-bit rgb)))

(defun rgb-match-p (a b)
  "True when the sRGB lists A and B differ by at most one 8-bit step on every channel."
  (every (lambda (x y) (<= (abs (- (eight-bit x) (eight-bit y))) 1)) a b))

(defun composite-over (rgb alpha background)
  "Return the sRGB list of ink RGB at opacity ALPHA painted over the sRGB list BACKGROUND.
Each channel is ALPHA * ink + (1 - ALPHA) * background."
  (mapcar (lambda (c b) (+ (* alpha c) (* (- 1 alpha) b))) rgb background))
