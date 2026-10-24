;;;; src/contrast.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; Contrast between two sRGB colours as WCAG 2.2 defines it, and the thresholds it is judged by.

(defpackage #:ekphrasis/src/contrast
  (:use #:cl)
  (:export #:relative-luminance
           #:contrast-ratio
           #:below-threshold-p
           #:*default-contrast-thresholds*
           #:resolve-thresholds))

(in-package #:ekphrasis/src/contrast)

(defparameter *default-contrast-thresholds* '(:text 4.5 :non-text 3.0)
  "The minimum contrast ratios: 4.5 for text and 3.0 for outlines and other non-text marks.")

(defun linearize (channel)
  "Return the linear-light value of the sRGB CHANNEL, a real in [0,1], as a double float."
  (let ((c (float channel 1d0)))
    (if (<= c 0.04045d0)
        (/ c 12.92d0)
        (expt (/ (+ c 0.055d0) 1.055d0) 2.4d0))))

(defun relative-luminance (rgb)
  "Return the WCAG 2.2 relative luminance of the sRGB list RGB as a double float in [0,1]."
  (destructuring-bind (r g b) rgb
    (+ (* 0.2126d0 (linearize r))
       (* 0.7152d0 (linearize g))
       (* 0.0722d0 (linearize b)))))

(defun contrast-ratio (rgb-1 rgb-2)
  "Return the WCAG 2.2 contrast ratio of the sRGB lists RGB-1 and RGB-2, from 1 to 21.
The ratio is symmetric: the lighter colour is always the numerator."
  (let ((l1 (relative-luminance rgb-1))
        (l2 (relative-luminance rgb-2)))
    (/ (+ (max l1 l2) 0.05d0)
       (+ (min l1 l2) 0.05d0))))

(defun below-threshold-p (ratio threshold)
  "True when RATIO is strictly below THRESHOLD. Neither is rounded: 4.499 is below 4.5."
  (< ratio threshold))

(defun resolve-thresholds (&optional call spec)
  "Return a plist (:TEXT text :NON-TEXT non-text) of contrast thresholds.
Each key is taken from the plist CALL when it sets the key, else from the plist SPEC, else from
*DEFAULT-CONTRAST-THRESHOLDS*."
  (loop for key in '(:text :non-text)
        collect key
        collect (or (getf call key)
                    (getf spec key)
                    (getf *default-contrast-thresholds* key))))
