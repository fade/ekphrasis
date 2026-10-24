;;;; src/ekphrasis.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; The public interface of the core: records, spec parsing, comparison, report, and run.

(uiop:define-package #:ekphrasis
  (:use #:cl)
  (:use-reexport #:ekphrasis/src/model
                 #:ekphrasis/src/spec
                 #:ekphrasis/src/compare
                 #:ekphrasis/src/report
                 #:ekphrasis/src/run)
  (:documentation "Check what a view draws against a spec held as data.
Toolkit-neutral: a snapshot of drawn elements is judged with no toolkit loaded."))
