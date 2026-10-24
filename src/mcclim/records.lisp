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
           #:*record-internals*
           #:missing-record-internals))

(in-package #:ekphrasis/src/mcclim/records)

(defparameter *mcclim-fork-commit* "537f213e6"
  "The McCLIM fork commit whose internal record classes and slots this file was read from.")

(defparameter *record-internals*
  '((climi::draw-text-output-record climi::string))
  "The McCLIM internal classes the walker reads, each followed by the slots it reads.")

(defun text-record-p (record)
  "True when RECORD is an output record of one DRAW-TEXT* call."
  (typep record 'climi::draw-text-output-record))

(defun text-record-string (record)
  "Return the string a text output record drew. RECORD satisfies TEXT-RECORD-P."
  (check-type record output-record)
  (slot-value record 'climi::string))

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
