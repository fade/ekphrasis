;;;; src/mcclim/ekphrasis-mcclim.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; The public interface of the McCLIM adapter.

(uiop:define-package #:ekphrasis/mcclim
  (:use #:cl)
  (:use-reexport #:ekphrasis/src/mcclim/adapter
                 #:ekphrasis/src/mcclim/medium)
  (:documentation "Render McCLIM frame panes headlessly into ekphrasis snapshots and judge them."))
