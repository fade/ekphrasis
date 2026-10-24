;;;; src/mcclim/medium.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; A headless port whose medium measures text on a fixed-width cell grid, so recorded text boxes
;;;; are deterministic and non-zero. The plain null medium measures every string as zero wide.

(defpackage #:ekphrasis/src/mcclim/medium
  (:use #:cl)
  (:import-from #:clim
                #:find-port
                #:medium-merged-text-style
                #:text-size
                #:text-style-ascent
                #:text-style-descent
                #:text-style-height
                #:text-style-size
                #:text-style-width)
  (:import-from #:mcclim-null
                #:null-medium
                #:null-port)
  (:export #:fixed-width-port
           #:fixed-width-medium
           #:make-fixed-width-port
           #:text-style-pixels
           #:*text-pixels-function*))

(in-package #:ekphrasis/src/mcclim/medium)

(defclass fixed-width-port (null-port)
  ()
  (:documentation "A headless null port whose media are FIXED-WIDTH-MEDIUM instances."))

(defclass fixed-width-medium (null-medium)
  ()
  (:documentation "A headless medium measuring text on a fixed-width cell grid.
Cell width is 0.6 px, ascent 0.8 px, and descent 0.2 px, each rounded, where px is the size
*TEXT-PIXELS-FUNCTION* gives for the text style."))

(defmethod climb:find-port-type ((type (eql :ekphrasis-fixed-width)))
  (values 'fixed-width-port 'identity))

(defmethod climb:make-medium ((port fixed-width-port) sheet)
  (make-instance 'fixed-width-medium :port port :sheet sheet))

(defparameter *text-size-pixels*
  '(:tiny 8 :very-small 9 :small 10 :normal 12 :large 16 :very-large 20 :huge 24)
  "Pixel sizes for the keyword text-style sizes.")

(defun text-style-pixels (text-style)
  "Return the size of TEXT-STYLE in pixels.
A real size is returned as given; a keyword size is looked up in the table
:tiny 8, :very-small 9, :small 10, :normal 12, :large 16, :very-large 20, :huge 24;
anything else, a NIL style included, gives 12."
  (let ((size (and text-style (text-style-size text-style))))
    (cond ((realp size) size)
          ((and (keywordp size) (getf *text-size-pixels* size)))
          (t 12))))

(defvar *text-pixels-function* 'text-style-pixels
  "The function of one text style that the fixed-width metrics call for its size in pixels.")

(defun style-pixels (text-style medium)
  "Return the pixel size of TEXT-STYLE on MEDIUM, defaulting to MEDIUM's merged text style."
  (funcall *text-pixels-function* (or text-style (medium-merged-text-style medium))))

(defun cell-width (text-style medium)
  "Return the width of one character cell of TEXT-STYLE on MEDIUM."
  (round (* 0.6 (style-pixels text-style medium))))

(defun cell-ascent (text-style medium)
  "Return the ascent of TEXT-STYLE on MEDIUM."
  (round (* 0.8 (style-pixels text-style medium))))

(defun cell-descent (text-style medium)
  "Return the descent of TEXT-STYLE on MEDIUM."
  (round (* 0.2 (style-pixels text-style medium))))

(defmethod text-style-ascent (text-style (medium fixed-width-medium))
  (cell-ascent text-style medium))

(defmethod text-style-descent (text-style (medium fixed-width-medium))
  (cell-descent text-style medium))

(defmethod text-style-height (text-style (medium fixed-width-medium))
  (+ (cell-ascent text-style medium) (cell-descent text-style medium)))

(defmethod climb:text-style-character-width (text-style (medium fixed-width-medium) char)
  (declare (ignore char))
  (cell-width text-style medium))

(defmethod text-style-width (text-style (medium fixed-width-medium))
  (cell-width text-style medium))

(defmethod text-size ((medium fixed-width-medium) string &key text-style (start 0) end)
  (let* ((string (if (characterp string) (string string) string))
         (count (- (or end (length string)) start))
         (width (* count (cell-width text-style medium)))
         (ascent (cell-ascent text-style medium))
         (height (+ ascent (cell-descent text-style medium))))
    (values width height width 0 ascent)))

(defmethod climb:text-bounding-rectangle*
    ((medium fixed-width-medium) string &key text-style (start 0) end)
  (multiple-value-bind (width height dx dy ascent)
      (text-size medium string :text-style text-style :start start :end end)
    (values 0 (- ascent) width (- height ascent) dx dy)))

(defun make-fixed-width-port ()
  "Return a fresh FIXED-WIDTH-PORT under a server path no other port shares.
The caller destroys it with CLIM:DESTROY-PORT when done."
  (find-port :server-path (list :ekphrasis-fixed-width :id (gensym "EKPHRASIS-PORT-"))))
