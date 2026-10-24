;;;; tests/mcclim/fixture.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; An ekphrasis fixture frame with a header pane, drawn in variants that each carry one defect,
;;;; plus a clean variant every check must pass.

(defpackage #:ekphrasis/tests/mcclim/fixture
  (:use #:cl)
  (:import-from #:clim
                #:+gray20+
                #:+white+
                #:define-application-frame
                #:draw-text*
                #:make-text-style
                #:with-text-style)
  (:import-from #:ekphrasis/mcclim
                #:check-frame-pane
                #:snapshot-frame-pane)
  (:export #:*fixture-variant*
           #:fixture-view
           #:display-fixture-view
           #:title-spec
           #:render-fixture
           #:check-fixture))

(in-package #:ekphrasis/tests/mcclim/fixture)

(defvar *fixture-variant* :clean
  "The variant the fixture draws: :CLEAN, or :UNTITLED, which omits the title.")

(define-application-frame fixture-view ()
  ()
  (:menu-bar nil)
  (:panes
   (header :application
           :display-function 'display-fixture-view
           :scroll-bars nil
           :background +gray20+
           :foreground +white+
           :text-style (make-text-style :sans-serif :roman 13)))
  (:layouts
   (default header)))

(defun display-fixture-view (frame pane)
  "Draw the header pane of FRAME onto PANE as *FIXTURE-VARIANT* says."
  (declare (ignore frame))
  (unless (eq *fixture-variant* :untitled)
    (with-text-style (pane (make-text-style :sans-serif :bold 22))
      (draw-text* pane "EKPHRASIS" 16 32))))

(defun title-spec ()
  "Return the spec form naming the header's title."
  (list :view :header
        :elements (list (list :title :kind :text :label "EKPHRASIS"))))

(defun render-fixture (&key (variant :clean) (frame 'fixture-view) render)
  "Return the snapshot of FRAME's header pane drawn as VARIANT.
RENDER is a plist of further SNAPSHOT-FRAME-PANE keyword arguments."
  (let ((*fixture-variant* variant))
    (apply #'snapshot-frame-pane frame 'header :width 900 :height 60 render)))

(defun check-fixture (spec &key (variant :clean) (frame 'fixture-view) observers render)
  "Judge FRAME's header pane drawn as VARIANT against SPEC and return what CHECK-FRAME-PANE returns.
OBSERVERS are passed to the run; RENDER is a plist of further SNAPSHOT-FRAME-PANE arguments."
  (let ((*fixture-variant* variant))
    (check-frame-pane spec frame 'header
                      :observers observers
                      :render (append render (list :width 900 :height 60)))))
