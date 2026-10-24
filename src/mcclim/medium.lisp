;;;; src/mcclim/medium.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; A headless port whose medium measures text on a fixed-width cell grid, so recorded text boxes
;;;; are deterministic and non-zero. The plain null medium measures every string as zero wide.

(defpackage #:ekphrasis/src/mcclim/medium
  (:use #:cl)
  (:import-from #:clim
                #:bounding-rectangle*
                #:find-port
                #:make-ellipse*
                #:make-elliptical-arc*
                #:medium-draw-ellipse*
                #:medium-draw-line*
                #:medium-draw-lines*
                #:medium-draw-point*
                #:medium-draw-points*
                #:medium-draw-polygon*
                #:medium-draw-rectangle*
                #:medium-draw-rectangles*
                #:medium-draw-text*
                #:medium-ink
                #:medium-merged-text-style
                #:medium-transformation
                #:text-size
                #:text-style-ascent
                #:text-style-descent
                #:text-style-height
                #:text-style-size
                #:text-style-width
                #:transform-position)
  (:import-from #:mcclim-null
                #:null-medium
                #:null-port)
  (:export #:fixed-width-port
           #:fixed-width-medium
           #:make-fixed-width-port
           #:text-style-pixels
           #:*text-pixels-function*
           #:*drawing-log*))

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

(defvar *drawing-log* nil
  "The collector of drawing primitives a FIXED-WIDTH-MEDIUM records, or NIL when nothing is
captured. When non-NIL, an adjustable vector with a fill pointer; each drawing call on the
medium pushes one primitive, a plist (:SHAPE shape :BOX box :FILLED filled :INK ink :TEXT text).
SHAPE names the call: :POINT, :POINTS, :LINE, :LINES, :POLYGON, :BEZIGON, :RECTANGLE,
:RECTANGLES, :ELLIPSE, or :TEXT. BOX is (x1 y1 x2 y2) in stream coordinates, rounded to
integers. FILLED is T or NIL. INK is the medium's ink at the call, unresolved. TEXT is the string
drawn, NIL for a shape.")

(defun log-primitive (medium shape box filled &optional text)
  "Push the primitive SHAPE drawn on MEDIUM with BOX (x1 y1 x2 y2) onto *DRAWING-LOG*.
BOX is rounded to integers; FILLED is stored as T or NIL; TEXT is the string drawn, or NIL."
  (vector-push-extend (list :shape shape
                            :box (mapcar #'round box)
                            :filled (and filled t)
                            :ink (medium-ink medium)
                            :text text)
                      *drawing-log*))

(defun coordinates-box (coord-seq)
  "Return the box (x1 y1 x2 y2) bounding the points of COORD-SEQ, a sequence x1 y1 x2 y2 ...
Return NIL for an empty sequence."
  (let ((coordinates (coerce coord-seq 'list)))
    (when coordinates
      (loop for (x y) on coordinates by #'cddr
            minimize x into x1
            minimize y into y1
            maximize x into x2
            maximize y into y2
            finally (return (list x1 y1 x2 y2))))))

(defun ellipse-box (cx cy rdx1 rdy1 rdx2 rdy2 start-angle end-angle filled)
  "Return the box (x1 y1 x2 y2) of the ellipse, or the arc when FILLED is NIL, that
MEDIUM-DRAW-ELLIPSE* draws for these arguments, honouring START-ANGLE and END-ANGLE.
A degenerate ellipse, which CLIM cannot make as a region, gives the box of its full ellipse."
  (handler-case
      (multiple-value-list
       (bounding-rectangle* (funcall (if filled #'make-ellipse* #'make-elliptical-arc*)
                                     cx cy rdx1 rdy1 rdx2 rdy2
                                     :start-angle start-angle :end-angle end-angle)))
    (error ()
      (let ((rx (sqrt (+ (* rdx1 rdx1) (* rdx2 rdx2))))
            (ry (sqrt (+ (* rdy1 rdy1) (* rdy2 rdy2)))))
        (list (- cx rx) (- cy ry) (+ cx rx) (+ cy ry))))))

(defun text-box (medium string x y start end align-x align-y)
  "Return the box (x1 y1 x2 y2) MEDIUM-DRAW-TEXT* covers on MEDIUM for these arguments.
The anchor (X, Y) is mapped through MEDIUM's transformation, since text drawing leaves it in user
coordinates. Width, ascent, and descent come from the fixed-width metrics at MEDIUM's merged text
style. ALIGN-X is :LEFT, :CENTER, or :RIGHT; ALIGN-Y is :BASELINE, :TOP, :CENTER, or :BOTTOM;
any other value aligns as :LEFT or :BASELINE."
  (multiple-value-bind (ax ay) (transform-position (medium-transformation medium) x y)
    (let* ((style (medium-merged-text-style medium))
           (width (text-size medium string :text-style style :start (or start 0) :end end))
           (ascent (cell-ascent style medium))
           (height (+ ascent (cell-descent style medium)))
           (left (case align-x
                   (:center (- ax (/ width 2)))
                   (:right (- ax width))
                   (t ax)))
           (top (case align-y
                  (:top ay)
                  (:center (- ay (/ height 2)))
                  (:bottom (- ay height))
                  (t (- ay ascent)))))
      (list left top (+ left width) (+ top height)))))

(defmethod medium-draw-point* :before ((medium fixed-width-medium) x y)
  (when *drawing-log*
    (log-primitive medium :point (list x y x y) nil)))

(defmethod medium-draw-points* :before ((medium fixed-width-medium) coord-seq)
  (when (and *drawing-log* (coordinates-box coord-seq))
    (log-primitive medium :points (coordinates-box coord-seq) nil)))

(defmethod medium-draw-line* :before ((medium fixed-width-medium) x1 y1 x2 y2)
  (when *drawing-log*
    (log-primitive medium :line (list (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2)) nil)))

(defmethod medium-draw-lines* :before ((medium fixed-width-medium) coord-seq)
  (when (and *drawing-log* (coordinates-box coord-seq))
    (log-primitive medium :lines (coordinates-box coord-seq) nil)))

(defmethod medium-draw-polygon* :before ((medium fixed-width-medium) coord-seq closed filled)
  (declare (ignore closed))
  (when (and *drawing-log* (coordinates-box coord-seq))
    (log-primitive medium :polygon (coordinates-box coord-seq) filled)))

(defmethod clime:medium-draw-bezigon* :before ((medium fixed-width-medium) coord-seq closed filled)
  (declare (ignore closed))
  (when (and *drawing-log* (coordinates-box coord-seq))
    (log-primitive medium :bezigon (coordinates-box coord-seq) filled)))

(defmethod medium-draw-rectangle* :before ((medium fixed-width-medium)
                                           left top right bottom filled)
  (when *drawing-log*
    (log-primitive medium :rectangle
                   (list (min left right) (min top bottom) (max left right) (max top bottom))
                   filled)))

(defmethod medium-draw-rectangles* :before ((medium fixed-width-medium) coord-seq filled)
  (when (and *drawing-log* (coordinates-box coord-seq))
    (log-primitive medium :rectangles (coordinates-box coord-seq) filled)))

(defmethod medium-draw-text* :before ((medium fixed-width-medium) string x y start end
                                      align-x align-y toward-x toward-y transform-glyphs)
  (declare (ignore toward-x toward-y transform-glyphs))
  (when *drawing-log*
    (let ((string (if (characterp string) (string string) string)))
      (log-primitive medium :text (text-box medium string x y start end align-x align-y) nil
                     (subseq string (or start 0) end)))))

(defmethod medium-draw-ellipse* :before ((medium fixed-width-medium)
                                         cx cy rdx1 rdy1 rdx2 rdy2 start-angle end-angle filled)
  (when *drawing-log*
    (log-primitive medium :ellipse
                   (ellipse-box cx cy rdx1 rdy1 rdx2 rdy2 start-angle end-angle filled)
                   filled)))
