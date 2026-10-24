;;;; tests/mcclim/fixture.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; An ekphrasis fixture frame with a header pane, drawn in variants that each carry one defect,
;;;; plus a clean variant every check must pass.

(defpackage #:ekphrasis/tests/mcclim/fixture
  (:use #:cl)
  (:import-from #:clim
                #:+gray20+
                #:+gray80+
                #:+white+
                #:bounding-rectangle-width
                #:define-application-frame
                #:define-presentation-type
                #:draw-polygon*
                #:draw-rectangle*
                #:draw-text*
                #:make-rgb-color
                #:make-text-style
                #:text-size
                #:with-output-as-presentation
                #:with-text-style)
  (:import-from #:ekphrasis/mcclim
                #:check-frame-pane
                #:snapshot-frame-pane)
  (:export #:*fixture-variant*
           #:fixture-view
           #:display-fixture-view
           #:session-pill
           #:fixture-session
           #:make-fixture-session
           #:fixture-session-name
           #:fixture-session-state
           #:fixture-session-drawn-state
           #:*attached-fill*
           #:*fenced-fill*
           #:fixture-sessions
           #:title-spec
           #:fixture-spec
           #:render-fixture
           #:check-fixture))

(in-package #:ekphrasis/tests/mcclim/fixture)

(defvar *fixture-variant* :clean
  "The variant the fixture draws: :CLEAN; :UNTITLED, which omits the title; :MISSING, which omits
the bravo pill; or :RELABELLED, which labels the bravo pill \"brovo\".")

(define-presentation-type session-pill ())

(defstruct (fixture-session
            (:constructor make-fixture-session (name state &optional (drawn-state state))))
  "One session the header shows as a pill.
NAME is its label; STATE is the state it is in; DRAWN-STATE is the state its pill is drawn as.
The struct is named apart from the SESSION-PILL presentation type: both would define a class."
  name
  state
  drawn-state)

(defparameter *attached-fill* (make-rgb-color (/ #x5C 255) (/ #x42 255) (/ #x1A 255))
  "The fill of a pill drawn as attached.")

(defparameter *fenced-fill* (make-rgb-color (/ #x80 255) (/ #x29 255) (/ #x29 255))
  "The fill of a pill drawn as fenced.")

(defun fixture-sessions (variant)
  "Return the sessions the header shows for VARIANT, in visual order from left to right.
:CLEAN and :UNTITLED show alpha attached, bravo fenced, and charlie attached; :MISSING drops
bravo; :RELABELLED names bravo \"brovo\"."
  (ecase variant
    ((:clean :untitled)
     (list (make-fixture-session "alpha" :attached)
           (make-fixture-session "bravo" :fenced)
           (make-fixture-session "charlie" :attached)))
    (:missing
     (list (make-fixture-session "alpha" :attached)
           (make-fixture-session "charlie" :attached)))
    (:relabelled
     (list (make-fixture-session "alpha" :attached)
           (make-fixture-session "brovo" :fenced)
           (make-fixture-session "charlie" :attached)))))

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

(defun state-fill (state)
  "Return the fill a pill drawn in STATE takes."
  (ecase state
    (:attached *attached-fill*)
    (:fenced *fenced-fill*)))

(defun draw-session-pill (pane session right)
  "Draw SESSION's pill on PANE with its right edge at RIGHT, as a SESSION-PILL presentation.
The pill is eleven filled bands 2 px tall from y 8 to 30 in its drawn-state fill, an unfilled
gray80 border through its corners, and its white label. Return the pill's left edge."
  (let* ((label (fixture-session-name session))
         (left (- right (+ (text-size pane label) 24))))
    (with-output-as-presentation (pane session 'session-pill)
      (let ((fill (state-fill (fixture-session-drawn-state session))))
        (loop for top from 8 below 30 by 2
              do (draw-rectangle* pane left top right (+ top 2) :filled t :ink fill)))
      (draw-polygon* pane (list left 8 right 8 right 30 left 30)
                     :closed t :filled nil :ink +gray80+)
      (draw-text* pane label (+ left 12) 25 :ink +white+))
    left))

(defun display-fixture-view (frame pane)
  "Draw the header pane of FRAME onto PANE as *FIXTURE-VARIANT* says.
Draw the title, then the session pills from the right edge leftward in reverse visual order,
8 px apart, then an unfilled white target box."
  (declare (ignore frame))
  (unless (eq *fixture-variant* :untitled)
    (with-text-style (pane (make-text-style :sans-serif :bold 22))
      (draw-text* pane "EKPHRASIS" 16 32)))
  (let ((right (- (bounding-rectangle-width pane) 16)))
    (dolist (session (reverse (fixture-sessions *fixture-variant*)))
      (setf right (- (draw-session-pill pane session right) 8))))
  (draw-rectangle* pane 300 10 380 40 :filled nil :ink +white+))

(defun title-spec ()
  "Return the spec form naming the header's title."
  (list :view :header
        :elements (list (list :title :kind :text :label "EKPHRASIS"))))

(defun fixture-spec ()
  "Return the spec form the clean header conforms to: the title and the three session pills."
  (list :view :header
        :elements (list (list :title :kind :text :label "EKPHRASIS")
                        (list :alpha :kind :session-pill :label "alpha")
                        (list :bravo :kind :session-pill :label "bravo")
                        (list :charlie :kind :session-pill :label "charlie"))))

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
