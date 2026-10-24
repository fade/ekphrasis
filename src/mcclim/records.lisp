;;;; src/mcclim/records.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; The only file that names McCLIM internals. Each accessor here reads a class or slot that
;;;; McCLIM does not export; when the fork moves, this is the one place to repair.

(defpackage #:ekphrasis/src/mcclim/records
  (:use #:cl)
  (:import-from #:clim
                #:output-record)
  (:export #:*mcclim-fork-commit*
           #:text-record-p
           #:text-record-string
           #:shape-record-kind
           #:shape-record-filled-p
           #:polygon-record-coordinates
           #:uniform-compositum-p
           #:*record-internals*
           #:missing-record-internals))

(in-package #:ekphrasis/src/mcclim/records)

(defparameter *mcclim-fork-commit* "537f213e6"
  "The McCLIM fork commit whose internal record classes and slots this file was read from.")

(defparameter *record-internals*
  '((climi::draw-text-output-record climi::string)
    (climi::draw-rectangle-output-record climi::filled)
    (climi::draw-rectangles-output-record climi::filled climi::coord-seq)
    (climi::draw-polygon-output-record climi::filled climi::coord-seq)
    (climi::draw-bezigon-output-record climi::filled climi::coord-seq)
    (climi::draw-ellipse-output-record climi::filled)
    (climi::draw-line-output-record)
    (climi::draw-lines-output-record climi::coord-seq)
    (climi::draw-point-output-record)
    (climi::draw-points-output-record climi::coord-seq)
    (climi::draw-pattern-output-record)
    (climi::uniform-compositum))
  "The McCLIM internal classes the adapter reads, each followed by the slots it reads.")

(defun text-record-p (record)
  "True when RECORD is an output record of one DRAW-TEXT* call."
  (typep record 'climi::draw-text-output-record))

(defun text-record-string (record)
  "Return the string a text output record drew. RECORD satisfies TEXT-RECORD-P."
  (check-type record output-record)
  (slot-value record 'climi::string))

(defun shape-record-kind (record)
  "Return the keyword naming the drawing call RECORD records, or NIL when it records no shape.
The keywords are :RECTANGLE, :RECTANGLES, :POLYGON, :BEZIGON, :ELLIPSE, :LINE, :LINES, :POINT,
:POINTS, and :PATTERN."
  (typecase record
    (climi::draw-rectangle-output-record :rectangle)
    (climi::draw-rectangles-output-record :rectangles)
    (climi::draw-polygon-output-record :polygon)
    (climi::draw-bezigon-output-record :bezigon)
    (climi::draw-ellipse-output-record :ellipse)
    (climi::draw-line-output-record :line)
    (climi::draw-lines-output-record :lines)
    (climi::draw-point-output-record :point)
    (climi::draw-points-output-record :points)
    (climi::draw-pattern-output-record :pattern)
    (t nil)))

(defun shape-record-filled-p (record)
  "True when RECORD records a filled rectangle, rectangle set, polygon, bezigon, or ellipse.
Return NIL for every other record."
  (and (member (shape-record-kind record) '(:rectangle :rectangles :polygon :bezigon :ellipse))
       (slot-value record 'climi::filled)
       t))

(defun polygon-record-coordinates (record)
  "Return the coordinates RECORD was drawn through as a list (x1 y1 x2 y2 ...).
RECORD records rectangles, a polygon, a bezigon, lines, or points; return NIL for any other."
  (when (member (shape-record-kind record) '(:rectangles :polygon :bezigon :lines :points))
    (coerce (slot-value record 'climi::coord-seq) 'list)))

(defun uniform-compositum-p (design)
  "True when DESIGN is a uniform compositum: one colour at one opacity, as COMPOSE-IN of a colour
and an opacity returns. McCLIM exports no predicate for the class."
  (typep design 'climi::uniform-compositum))

(defun missing-record-internals (&optional (internals *record-internals*))
  "Return a list of strings, one per internal class or slot in INTERNALS that this image lacks.
INTERNALS is a list of entries (class-name slot-name ...), by default *RECORD-INTERNALS*.
Return NIL when every class exists and names every slot. Finalizes each class it inspects."
  (loop for (class-name . slot-names) in internals
        for class = (find-class class-name nil)
        if (null class)
          collect (format nil "class ~S" class-name)
        else
          append (progn
                   (unless (sb-mop:class-finalized-p class)
                     (sb-mop:finalize-inheritance class))
                   (let ((present (mapcar #'sb-mop:slot-definition-name
                                          (sb-mop:class-slots class))))
                     (loop for slot-name in slot-names
                           unless (member slot-name present :test #'eq)
                             collect (format nil "slot ~S of class ~S"
                                             slot-name class-name))))))
