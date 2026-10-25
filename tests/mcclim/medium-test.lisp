;;;; tests/mcclim/medium-test.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; Text metrics on the fixed-width medium: known cell sizes, widths that grow with the text, and
;;;; recorded text boxes that follow the cell rule.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((pkg (find-package '#:ekphrasis/tests/mcclim/medium-test)))
    (when pkg
      (sb-ext:without-package-locks (delete-package pkg)))))

(defpackage #:ekphrasis/tests/mcclim/medium-test
  (:use #:cl #:zebra)
  (:import-from #:clim
                #:destroy-port
                #:make-text-style
                #:text-size
                #:text-style-ascent
                #:text-style-descent
                #:text-style-height
                #:text-style-width)
  (:import-from #:ekphrasis
                #:element-box
                #:element-kind
                #:element-text
                #:snapshot-elements)
  (:import-from #:ekphrasis/mcclim
                #:make-fixed-width-port)
  (:import-from #:ekphrasis/tests/mcclim/fixture
                #:render-fixture))

(in-package #:ekphrasis/tests/mcclim/medium-test)

(defun call-with-fixed-width-medium (function)
  "Call FUNCTION with a medium of a fresh fixed-width port, destroying the port afterwards."
  (let ((port (make-fixed-width-port)))
    (unwind-protect
         (funcall function (climb:make-medium port nil))
      (destroy-port port))))

(defun text-width (medium string text-style)
  "Return the width of STRING drawn in TEXT-STYLE on MEDIUM."
  (values (text-size medium string :text-style text-style)))

(defun text-elements (snapshot)
  "Return the :TEXT elements of SNAPSHOT."
  (remove :text (snapshot-elements snapshot) :key #'element-kind :test-not #'eq))

(define-test known-cell-metrics
  "Cells are 0.6 px wide, ascents 0.8 px, and descents 0.2 px, each rounded."
  (call-with-fixed-width-medium
   (lambda (medium)
     (let ((body (make-text-style :sans-serif :roman 13))
           (title (make-text-style :sans-serif :roman 22))
           (normal (make-text-style :sans-serif :roman :normal)))
       (is = 88 (text-width medium "hello world" body))
       (is = 13 (text-style-width title medium))
       (is = 18 (text-style-ascent title medium))
       (is = 4 (text-style-descent title medium))
       (is = 22 (text-style-height title medium))
       (is = 7 (text-style-width normal medium))
       (is = 10 (text-style-ascent normal medium))
       (is = 2 (text-style-descent normal medium))))))

(define-test text-width-grows-with-length
  "A longer string in the same style is wider, and neither is zero wide."
  (call-with-fixed-width-medium
   (lambda (medium)
     (let* ((style (make-text-style :sans-serif :roman 13))
            (short (text-width medium "status" style))
            (long (text-width medium "reconnecting to host" style)))
       (true (plusp short))
       (true (plusp long))
       (true (> long short) "~D is not wider than ~D" long short)))))

(define-test text-box-matches-cell-rule
  "The clean title, bold 22 at (16 32), records the box (16 14 133 36)."
  (let ((title (find "EKPHRASIS" (text-elements (render-fixture :variant :clean))
                     :key #'element-text :test #'equal)))
    (true title)
    (is equal '(16 14 133 36) (mapcar #'round (element-box title)))))

(define-test text-boxes-are-non-zero
  "Every text element of the clean render has positive width and height."
  (let ((texts (text-elements (render-fixture :variant :clean))))
    (true texts)
    (dolist (element texts)
      (destructuring-bind (x1 y1 x2 y2) (element-box element)
        (true (> x2 x1) "~S has no width: ~S" (element-text element) (element-box element))
        (true (> y2 y1) "~S has no height: ~S" (element-text element) (element-box element))))))
