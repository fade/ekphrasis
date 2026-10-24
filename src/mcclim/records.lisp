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
           #:text-record-string))

(in-package #:ekphrasis/src/mcclim/records)

(defparameter *mcclim-fork-commit* "537f213e6"
  "The McCLIM fork commit whose internal record classes and slots this file was read from.")

(defun text-record-p (record)
  "True when RECORD is an output record of one DRAW-TEXT* call."
  (typep record 'climi::draw-text-output-record))

(defun text-record-string (record)
  "Return the string a text output record drew. RECORD satisfies TEXT-RECORD-P."
  (check-type record output-record)
  (slot-value record 'climi::string))
