;;;; tests/mcclim/fixture.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; An ekphrasis fixture frame with a header pane, drawn in variants that each carry one defect,
;;;; plus a clean variant every check must pass.

(defpackage #:ekphrasis/tests/mcclim/fixture
  (:use #:cl)
  (:import-from #:clim
                #:+black+
                #:+flipping-ink+
                #:+foreground-ink+
                #:+gray20+
                #:+gray80+
                #:+white+
                #:bounding-rectangle*
                #:bounding-rectangle-min-x
                #:bounding-rectangle-width
                #:compose-in
                #:define-application-frame
                #:define-presentation-method
                #:define-presentation-type
                #:draw-pattern*
                #:draw-polygon*
                #:draw-rectangle*
                #:draw-text*
                #:highlight-presentation
                #:make-gray-color
                #:make-opacity
                #:make-pattern
                #:make-rgb-color
                #:make-text-style
                #:map-over-output-records
                #:presentation-object
                #:presentationp
                #:stream-output-history
                #:surrounding-output-with-border
                #:text-size
                #:with-drawing-options
                #:with-output-as-presentation
                #:with-text-style)
  (:import-from #:ekphrasis/mcclim
                #:call-with-rendered-pane
                #:check-frame-pane
                #:snapshot-frame-pane)
  (:export #:*fixture-variant*
           #:fixture-view
           #:dark-ink-fixture-view
           #:display-fixture-view
           #:session-pill
           #:fixture-session
           #:make-fixture-session
           #:fixture-session-name
           #:fixture-session-state
           #:fixture-session-drawn-state
           #:*attached-fill*
           #:*fenced-fill*
           #:*attached-highlight-fill*
           #:*fenced-highlight-fill*
           #:fixture-sessions
           #:title-spec
           #:fixture-spec
           #:render-fixture
           #:call-with-fixture-pane
           #:fixture-presentations
           #:check-fixture
           #:fixture-battery))

(in-package #:ekphrasis/tests/mcclim/fixture)

(defvar *fixture-variant* :clean
  "The variant the fixture draws. Each one differs from :CLEAN in one mark:
:UNTITLED omits the title.
:MISSING omits the bravo pill.
:RELABELLED labels the bravo pill \"brovo\".
:LOW-CONTRAST-TEXT draws the title in 10% grey.
:LOW-CONTRAST-OUTLINE draws the target box in black.
:TRANSLUCENT-TITLE draws the title in white at half opacity.
:FLIPPING-OUTLINE draws the target box in the flipping ink.
:FLIPPING-FILL fills a rectangle beneath the title in the flipping ink.
:PATTERN-FILL draws a black and white pattern beneath the title.
:HIDDEN-FRAME-ROUND-PATTERN draws a black and white pattern inside the target box, and the box in
gray20, the page's own colour.
:FRAME-ROUND-PATTERN draws a black and white pattern inside the white target box.
:SWAPPED draws bravo left of alpha.
:WRONG-STATE has bravo attached, and draws it so.
:STATE-DRAWN-WRONGLY has bravo fenced but draws its pill attached.
:SINGLE-BOX-PILLS presents each pill as single-box, so its highlight is one thin frame.
:BORDERED-PILLS draws each pill as a label in a rounded border with a highlight background.
:BADGE-PILLS draws each pill as a label on a badge in its drawn-state fill, inside a rounded
black border whose highlight background is white.
:SOLID-PILLS draws each pill on one filled rectangle instead of eleven bands.
:REDRAWN-LABEL-PILLS presents each pill as single-box; its highlight fills the pill in white and
redraws the label on it in the pill's drawn-state fill.
:FLIPPING-HIGHLIGHT-PILLS presents each pill as single-box; its highlight fills the pill in the
flipping ink and redraws the label on it in white.
:OVERLAY-PILLS presents each pill as single-box; its highlight veils the pill in white at 15%
opacity and draws nothing else.
:PATTERNED-PILLS presents each pill as single-box, with a small black and white pattern swatch
inside it clear of its label and its frame.
:STICKY-HIGHLIGHT-PILLS presents each pill as single-box; unhighlighting leaves a white 1 px mark
at the pill's top-left corner, so its drawing is never restored. It exists to prove a real restore
failure is reported.
:COMBINED carries four defects at once: no charlie, bravo left of alpha, alpha fenced and drawn
so, and the target box in black.")

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

(defparameter *attached-highlight-fill* (make-rgb-color (/ #x7A 255) (/ #x5A 255) (/ #x26 255))
  "The fill of a bordered pill drawn as attached, while highlighted.")

(defparameter *fenced-highlight-fill* (make-rgb-color (/ #xA0 255) (/ #x3A 255) (/ #x3A 255))
  "The fill of a bordered pill drawn as fenced, while highlighted.")

(defun fixture-sessions (variant)
  "Return the sessions the header shows for VARIANT, in visual order from left to right.
:MISSING drops bravo; :RELABELLED names bravo \"brovo\"; :SWAPPED shows bravo first;
:WRONG-STATE has bravo attached and drawn so; :STATE-DRAWN-WRONGLY has bravo fenced but drawn
attached; :COMBINED shows bravo then alpha, both fenced, and no charlie. Every other variant
shows alpha attached, bravo fenced, and charlie attached."
  (ecase variant
    ((:clean :untitled :low-contrast-text :low-contrast-outline :translucent-title
      :flipping-outline :single-box-pills :bordered-pills :flipping-fill :pattern-fill
      :redrawn-label-pills :flipping-highlight-pills :solid-pills :overlay-pills :badge-pills
      :hidden-frame-round-pattern :frame-round-pattern :patterned-pills :sticky-highlight-pills)
     (list (make-fixture-session "alpha" :attached)
           (make-fixture-session "bravo" :fenced)
           (make-fixture-session "charlie" :attached)))
    (:missing
     (list (make-fixture-session "alpha" :attached)
           (make-fixture-session "charlie" :attached)))
    (:relabelled
     (list (make-fixture-session "alpha" :attached)
           (make-fixture-session "brovo" :fenced)
           (make-fixture-session "charlie" :attached)))
    (:swapped
     (list (make-fixture-session "bravo" :fenced)
           (make-fixture-session "alpha" :attached)
           (make-fixture-session "charlie" :attached)))
    (:wrong-state
     (list (make-fixture-session "alpha" :attached)
           (make-fixture-session "bravo" :attached)
           (make-fixture-session "charlie" :attached)))
    (:state-drawn-wrongly
     (list (make-fixture-session "alpha" :attached)
           (make-fixture-session "bravo" :fenced :attached)
           (make-fixture-session "charlie" :attached)))
    (:combined
     (list (make-fixture-session "bravo" :fenced)
           (make-fixture-session "alpha" :fenced)))))

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

(define-application-frame dark-ink-fixture-view ()
  ()
  (:documentation "The fixture header with a black foreground on a near-black page, #14151D.
Every resting mark is drawn in an explicit light ink, so only the toolkit's own highlight
feedback is drawn in the foreground.")
  (:menu-bar nil)
  (:panes
   (header :application
           :display-function 'display-fixture-view
           :scroll-bars nil
           :background (make-rgb-color 20/255 21/255 29/255)
           :foreground +black+
           :text-style (make-text-style :sans-serif :roman 13)))
  (:layouts
   (default header)))

(defun state-fill (state)
  "Return the fill a pill drawn in STATE takes."
  (ecase state
    (:attached *attached-fill*)
    (:fenced *fenced-fill*)))

(defun state-highlight-fill (state)
  "Return the fill a bordered pill drawn in STATE takes while highlighted."
  (ecase state
    (:attached *attached-highlight-fill*)
    (:fenced *fenced-highlight-fill*)))

(defun draw-session-pill (pane session right)
  "Draw SESSION's pill on PANE with its right edge at RIGHT, as a SESSION-PILL presentation.
The pill is eleven filled bands 2 px tall from y 8 to 30 in its drawn-state fill, an unfilled
gray80 border through its corners, and its white label; under :SOLID-PILLS one filled rectangle
from y 8 to 30 replaces the bands. Under :SINGLE-BOX-PILLS, :REDRAWN-LABEL-PILLS,
:FLIPPING-HIGHLIGHT-PILLS, :OVERLAY-PILLS, :PATTERNED-PILLS, and :STICKY-HIGHLIGHT-PILLS the
presentation is single-box. Under :PATTERNED-PILLS a 6 by 4 black and white pattern swatch is
drawn after the bands and before the border, from (left + 3, 9) to (left + 9, 13). Under
:BORDERED-PILLS the pill is instead its white label inside a rounded border whose background is
the drawn-state fill and whose highlight background is that state's highlight fill. Under
:BADGE-PILLS the border's background is black and its highlight background white, and inside it
the white label sits on a filled badge from (left + 8, 12) to (right - 8, 31) in the drawn-state
fill. Return the pill's left edge."
  (let* ((label (fixture-session-name session))
         (left (- right (+ (text-size pane label) 24)))
         (state (fixture-session-drawn-state session))
         (single-box (and (member *fixture-variant* '(:single-box-pills :redrawn-label-pills
                                                       :flipping-highlight-pills :overlay-pills
                                                       :patterned-pills :sticky-highlight-pills))
                          t)))
    (if (member *fixture-variant* '(:bordered-pills :badge-pills))
        (let* ((badge (eq *fixture-variant* :badge-pills))
               (record (with-output-as-presentation (pane session 'session-pill)
                         (surrounding-output-with-border
                             (pane :shape :rounded :move-cursor nil
                                   :background (if badge +black+ (state-fill state))
                                   :highlight-background (if badge
                                                             +white+
                                                             (state-highlight-fill state)))
                           (when badge
                             (draw-rectangle* pane (+ left 8) 12 (- right 8) 31
                                              :filled t :ink (state-fill state)))
                           (draw-text* pane label (+ left 12) 25 :ink +white+)))))
          (bounding-rectangle-min-x record))
        (progn
          (with-output-as-presentation (pane session 'session-pill :single-box single-box)
            (let ((fill (state-fill state)))
              (if (eq *fixture-variant* :solid-pills)
                  (draw-rectangle* pane left 8 right 30 :filled t :ink fill)
                  (loop for top from 8 below 30 by 2
                        do (draw-rectangle* pane left top right (+ top 2)
                                            :filled t :ink fill))))
            (when (eq *fixture-variant* :patterned-pills)
              (let ((indices (make-array '(4 6))))
                (dotimes (row 4)
                  (dotimes (column 6)
                    (setf (aref indices row column) (mod (+ row column) 2))))
                (draw-pattern* pane (make-pattern indices (list +black+ +white+)) (+ left 3) 9)))
            (draw-polygon* pane (list left 8 right 8 right 30 left 30)
                           :closed t :filled nil :ink +gray80+)
            (draw-text* pane label (+ left 12) 25 :ink +white+))
          left))))

;;; Highlight a session pill as *FIXTURE-VARIANT* says. Under :OVERLAY-PILLS, fill the pill's
;;; box in white at 15% opacity and draw nothing else; under :REDRAWN-LABEL-PILLS, fill the box
;;; in white and redraw its label on it, centred, in the drawn-state fill; under
;;; :FLIPPING-HIGHLIGHT-PILLS, fill the box in the flipping ink and redraw the label in white.
;;; Under :STICKY-HIGHLIGHT-PILLS, on :UNHIGHLIGHT, fill a 1 px white square at the box's top-left
;;; corner, so a real restore failure is there to be reported. Otherwise draw what the next method
;;; draws. A pill is its label's width plus 12 px each side, so the centred label lands on the
;;; resting label's box.

(define-presentation-method highlight-presentation ((type session-pill) record stream state)
  (let ((variant *fixture-variant*))
    (cond
      ((and (eq state :highlight) (eq variant :overlay-pills))
       (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* record)
         (draw-rectangle* stream x1 y1 x2 y2
                          :filled t :ink (compose-in +white+ (make-opacity 0.15)))))
      ((and (eq state :highlight)
            (member variant '(:redrawn-label-pills :flipping-highlight-pills)))
       (let* ((session (presentation-object record))
              (redrawn (eq variant :redrawn-label-pills))
              (state-ink (state-fill (fixture-session-drawn-state session))))
         (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* record)
           (draw-rectangle* stream x1 y1 x2 y2
                            :filled t :ink (if redrawn +white+ +flipping-ink+))
           (draw-text* stream (fixture-session-name session) (/ (+ x1 x2) 2) 25
                       :align-x :center :ink (if redrawn state-ink +white+)))))
      ((and (eq state :unhighlight) (eq variant :sticky-highlight-pills))
       (multiple-value-bind (x1 y1) (bounding-rectangle* record)
         (draw-rectangle* stream x1 y1 (1+ x1) (1+ y1) :filled t :ink +white+)))
      (t (call-next-method)))))

(defun display-fixture-view (frame pane)
  "Draw the header pane of FRAME onto PANE as *FIXTURE-VARIANT* says.
Draw the title, then the session pills from the right edge leftward in reverse visual order,
8 px apart, then an unfilled target box. The title is in the pane's foreground, white in a
DARK-INK-FIXTURE-VIEW, and the box in white, except where the variant changes one of those inks.
Under :FLIPPING-FILL and :PATTERN-FILL a fill from (8, 6) to (200, 40) is drawn first, beneath
the title. Under :HIDDEN-FRAME-ROUND-PATTERN and :FRAME-ROUND-PATTERN a black and white pattern
from (301, 11) to (379, 39) is drawn after the pills, filling the target box's interior; the
first draws the box in gray20, the page's own colour."
  (case *fixture-variant*
    (:flipping-fill
     (draw-rectangle* pane 8 6 200 40 :filled t :ink +flipping-ink+))
    (:pattern-fill
     (let ((indices (make-array '(34 192))))
       (dotimes (row 34)
         (dotimes (column 192)
           (setf (aref indices row column) (mod (+ row column) 2))))
       (draw-pattern* pane (make-pattern indices (list +black+ +white+)) 8 6))))
  (unless (eq *fixture-variant* :untitled)
    (with-text-style (pane (make-text-style :sans-serif :bold 22))
      (with-drawing-options (pane :ink (case *fixture-variant*
                                         (:low-contrast-text (make-gray-color 0.1))
                                         (:translucent-title
                                          (compose-in +white+ (make-opacity 0.5)))
                                         (t (if (typep frame 'dark-ink-fixture-view)
                                                +white+
                                                +foreground-ink+))))
        (draw-text* pane "EKPHRASIS" 16 32))))
  (let ((right (- (bounding-rectangle-width pane) 16)))
    (dolist (session (reverse (fixture-sessions *fixture-variant*)))
      (setf right (- (draw-session-pill pane session right) 8))))
  (when (member *fixture-variant* '(:hidden-frame-round-pattern :frame-round-pattern))
    (let ((indices (make-array '(28 78))))
      (dotimes (row 28)
        (dotimes (column 78)
          (setf (aref indices row column) (mod (+ row column) 2))))
      (draw-pattern* pane (make-pattern indices (list +black+ +white+)) 301 11)))
  (draw-rectangle* pane 300 10 380 40
                   :filled nil
                   :ink (case *fixture-variant*
                          ((:low-contrast-outline :combined) +black+)
                          (:hidden-frame-round-pattern +gray20+)
                          (:flipping-outline +flipping-ink+)
                          (t +white+))))

(defun title-spec ()
  "Return the spec form naming the header's title."
  (list :view :header
        :elements (list (list :title :kind :text :label "EKPHRASIS"))))

(defun fixture-spec ()
  "Return the spec form the clean header conforms to: the title and the three session pills,
alpha and charlie attached and bravo fenced, in that order from the left, each state known by
its pill's fill."
  (list :view :header
        :elements (list (list :title :kind :text :label "EKPHRASIS")
                        (list :alpha :kind :session-pill :label "alpha" :state :attached)
                        (list :bravo :kind :session-pill :label "bravo" :state :fenced)
                        (list :charlie :kind :session-pill :label "charlie" :state :attached))
        :order (list (list :left-to-right :alpha :bravo :charlie))
        :states (list (list :session-pill
                            (list :attached :background "#5C421A")
                            (list :fenced :background "#802929")))))

(defun call-with-fixture-pane (function &key (variant :clean) (frame 'fixture-view))
  "Render FRAME's header pane drawn as VARIANT, 900 by 60, and call FUNCTION with the pane.
Return FUNCTION's values; the frame and its port are gone afterwards."
  (let ((*fixture-variant* variant))
    (call-with-rendered-pane frame 'header
                             (lambda (made pane)
                               (declare (ignore made))
                               (funcall function pane))
                             :width 900 :height 60)))

(defun fixture-presentations (pane)
  "Return the presentations in PANE's output history, in painting order."
  (let ((presentations '()))
    (labels ((visit (record)
               (if (presentationp record)
                   (push record presentations)
                   (map-over-output-records #'visit record))))
      (map-over-output-records #'visit (stream-output-history pane)))
    (nreverse presentations)))

(defun render-fixture (&key (variant :clean) (frame 'fixture-view) render)
  "Return the snapshot of FRAME's header pane drawn as VARIANT.
RENDER is a plist of further SNAPSHOT-FRAME-PANE keyword arguments. Each pill reports its
session's state through FIXTURE-SESSION-STATE unless RENDER gives its own :STATE-FUNCTION,
NIL included."
  (let ((*fixture-variant* variant))
    (apply #'snapshot-frame-pane frame 'header :width 900 :height 60
           (append render (list :state-function #'fixture-session-state)))))

(defun check-fixture (spec &key (variant :clean) (frame 'fixture-view) observers render thresholds)
  "Judge FRAME's header pane drawn as VARIANT against SPEC and return what CHECK-FRAME-PANE returns.
OBSERVERS and THRESHOLDS are passed to the run; RENDER is a plist of further SNAPSHOT-FRAME-PANE
arguments. Each pill reports its session's state through FIXTURE-SESSION-STATE unless RENDER
gives its own :STATE-FUNCTION, NIL included."
  (let ((*fixture-variant* variant))
    (check-frame-pane spec frame 'header
                      :observers observers
                      :thresholds thresholds
                      :render (append render
                                      (list :width 900 :height 60
                                            :state-function #'fixture-session-state)))))

(defun fixture-battery ()
  "Return every defect class's control and clean twins, each a plist judged against FIXTURE-SPEC.
Each entry is (:CLASS class :CHECK check :MARK mark :CONTROL case :TWINS cases). CHECK is the
finding check the control must yield as an error. MARK is NIL, or (kind state) naming the kind
and state of the element that finding must name. A case is a plist (:FRAME frame :VARIANT variant
:RENDER render) of CHECK-FIXTURE arguments. Resting classes are rendered with highlight feedback
off; the interaction-only classes exercise it. The label-hidden class's twins are the highlight
idioms that leave the label legible: one thin frame, a redrawn bordered background, solid pills
framed round the label, a label redrawn on the highlight, a translucent veil, a label on a badge
inside a redrawn bordered background, and the band pills under a black highlight."
  (let ((resting (list :exercise-feedback nil)))
    (flet ((at-rest (variant)
             (list :frame 'fixture-view :variant variant :render resting))
           (exercised (variant &optional (frame 'fixture-view))
             (list :frame frame :variant variant :render '())))
      (list (list :class :missing :check :missing
                  :control (at-rest :missing) :twins (list (at-rest :clean)))
            (list :class :label :check :label
                  :control (at-rest :relabelled) :twins (list (at-rest :clean)))
            (list :class :order :check :order
                  :control (at-rest :swapped) :twins (list (at-rest :clean)))
            (list :class :state :check :state
                  :control (at-rest :wrong-state) :twins (list (at-rest :clean)))
            (list :class :state-appearance :check :state-appearance
                  :control (at-rest :state-drawn-wrongly) :twins (list (at-rest :clean)))
            (list :class :not-perceivable-text :check :contrast :mark '(:text nil)
                  :control (at-rest :low-contrast-text) :twins (list (at-rest :clean)))
            (list :class :not-perceivable-outline :check :contrast :mark '(:outline nil)
                  :control (at-rest :low-contrast-outline) :twins (list (at-rest :clean)))
            (list :class :interaction-only-not-perceivable :check :contrast
                  :mark '(:outline :highlighted)
                  :control (exercised :single-box-pills 'dark-ink-fixture-view)
                  :twins (list (exercised :single-box-pills)))
            (list :class :highlight-hides-label :check :contrast :mark '(:text :highlighted)
                  :control (exercised :clean)
                  :twins (list (exercised :single-box-pills) (exercised :bordered-pills)
                               (exercised :solid-pills) (exercised :redrawn-label-pills)
                               (exercised :overlay-pills) (exercised :badge-pills)
                               (exercised :clean 'dark-ink-fixture-view)))))))
