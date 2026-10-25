;;;; src/mcclim/adapter.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; Render a McCLIM frame pane headlessly and walk its output records, in painting order, into a
;;;; snapshot of drawn elements.

(defpackage #:ekphrasis/src/mcclim/adapter
  (:use #:cl)
  (:import-from #:clim
                #:*application-frame*
                #:+background-ink+
                #:+foreground-ink+
                #:bounding-rectangle*
                #:destroy-port
                #:disown-frame
                #:displayed-output-record-ink
                #:find-frame-manager
                #:find-pane-named
                #:frame-manager
                #:highlight-output-record
                #:make-application-frame
                #:make-polygon*
                #:make-rectangle*
                #:map-over-output-records
                #:opacity-value
                #:opacityp
                #:pane-background
                #:pane-foreground
                #:presentation-object
                #:presentation-type
                #:presentation-type-name
                #:presentationp
                #:redisplay-frame-pane
                #:region-contains-position-p
                #:replay
                #:stream-output-history
                #:with-output-recording-options)
  (:import-from #:ekphrasis
                #:composite-over
                #:contrast-ratio
                #:element-background
                #:element-box
                #:element-container
                #:element-id
                #:element-ink
                #:element-kind
                #:element-text
                #:make-element
                #:make-snapshot
                #:run-check)
  (:import-from #:ekphrasis/src/mcclim/medium
                #:*drawing-log*
                #:make-fixed-width-port)
  (:import-from #:ekphrasis/src/mcclim/records
                #:polygon-record-coordinates
                #:shape-record-filled-p
                #:shape-record-kind
                #:text-record-p
                #:text-record-string
                #:uniform-compositum-p)
  (:export #:snapshot-frame-pane
           #:check-frame-pane
           #:resolve-ink
           #:call-with-rendered-pane
           #:capture-replay
           #:exercise-feedback-state
           #:feedback-state-not-restored
           #:feedback-state-presentation-type
           #:feedback-state-box))

(in-package #:ekphrasis/src/mcclim/adapter)

(defun resolve-ink (ink pane)
  "Return the fixed colour INK paints on PANE as two values: an sRGB list (r g b) and its alpha.
+FOREGROUND-INK+ and +BACKGROUND-INK+ resolve through PANE's own foreground and background, not
its medium's, which keep their defaults on a headless port; other indirect inks resolve through
the ink they stand for. An opacity paints PANE's foreground at that opacity. Colours and uniform
composita give their own colour and alpha. Return NIL when INK has no fixed colour: a flipping
ink paints by XOR, and patterns and images paint more than one colour."
  (cond ((eq ink +foreground-ink+)
         (resolve-ink (pane-foreground pane) pane))
        ((eq ink +background-ink+)
         (resolve-ink (pane-background pane) pane))
        ((clime:indirect-ink-p ink)
         (resolve-ink (clime:indirect-ink-ink ink) pane))
        ((clime:flipping-ink-p ink)
         nil)
        ((and (opacityp ink) (not (typep ink 'clim:color)))
         (multiple-value-bind (rgb alpha) (resolve-ink (pane-foreground pane) pane)
           (when rgb
             (values rgb (* alpha (opacity-value ink))))))
        ((or (typep ink 'clim:color) (uniform-compositum-p ink))
         (multiple-value-bind (r g b alpha) (clime:color-rgba ink)
           (values (list r g b) alpha)))
        (t nil)))

(defun presentation-kind (presentation)
  "Return PRESENTATION's type name as a keyword: :SESSION-PILL for type SESSION-PILL."
  (intern (symbol-name (presentation-type-name (presentation-type presentation))) :keyword))

(defun sample-points (kind box)
  "Return the points (x . y) an element of KIND with BOX (x1 y1 x2 y2) is sampled at.
An :OUTLINE is sampled 2 px outside and 2 px inside the midpoint of each edge, eight points in
all; any other element at its box centre."
  (destructuring-bind (x1 y1 x2 y2) box
    (let ((mx (/ (+ x1 x2) 2))
          (my (/ (+ y1 y2) 2)))
      (if (eq kind :outline)
          (list (cons mx (- y1 2)) (cons mx (+ y1 2))
                (cons mx (+ y2 2)) (cons mx (- y2 2))
                (cons (- x1 2) my) (cons (+ x1 2) my)
                (cons (+ x2 2) my) (cons (- x2 2) my))
          (list (cons mx my))))))

(defun rgb-beneath (x y layers background)
  "Return the sRGB list that lies beneath the point (X, Y), or BACKGROUND when no layer covers it.
LAYERS is a list of (region rgb alpha), most recently drawn first; a layer with RGB NIL is opaque
and of no fixed colour. The first layer whose region contains the point decides:
- with no colour, return NIL, as nothing known lies beneath the point;
- with ALPHA below 1, return its RGB composited over what lies beneath that layer, or NIL when
  that is not known;
- otherwise return its RGB."
  (loop for (layer . below) on layers
        for (region rgb alpha) = layer
        when (region-contains-position-p region x y)
          return (cond ((null rgb) nil)
                       ((< alpha 1)
                        (let ((under (rgb-beneath x y below background)))
                          (and under (composite-over rgb alpha under))))
                       (t rgb))
        finally (return background)))

(defun ink-over (rgb alpha background)
  "Return the sRGB list ink RGB at ALPHA shows over BACKGROUND.
An opaque ink is RGB itself; a translucent one over no known background is NIL."
  (cond ((>= alpha 1) rgb)
        (background (composite-over rgb alpha background))
        (t nil)))

(defun sample-element (kind box rgb alpha layers background)
  "Return the ink, background, and partly-unresolved flag of an element of KIND and BOX drawn in
RGB at ALPHA, as three values.
Each sample point takes what lies beneath it under LAYERS over BACKGROUND, and the ink as it shows
there. Only points with something known beneath them are judged: of those, the point with the
lowest contrast ratio wins, the first one when no ratio is known. The third value is true when
some other sample point had nothing known beneath it.
When no sample point has anything known beneath it, the background is NIL, the ink is what
INK-OVER gives over no background (RGB when opaque, NIL when translucent), and the third value is
NIL. With RGB NIL, return NIL, what lies beneath the first sample point, and NIL."
  (let ((points (sample-points kind box)))
    (flet ((beneath (point)
             (rgb-beneath (car point) (cdr point) layers background)))
      (if (null rgb)
          (values nil (beneath (first points)) nil)
          (let* ((unders (mapcar #'beneath points))
                 (known (remove nil unders)))
            (if (null known)
                (values (ink-over rgb alpha nil) nil nil)
                (let (best-ink best-background best-ratio)
                  (loop for under in known
                        for firstp = t then nil
                        for ink = (ink-over rgb alpha under)
                        for ratio = (and ink (contrast-ratio ink under))
                        when (or firstp
                                 (and ratio (or (null best-ratio) (< ratio best-ratio))))
                          do (setf best-ink ink
                                   best-background under
                                   best-ratio ratio))
                  (values best-ink best-background
                          (and (member nil unders) t)))))))))

(defun fill-region (record kind)
  "Return the region a filled shape RECORD of KIND covers.
A polygon or bezigon covers the polygon through its coordinates; a rectangle, rectangle set, or
ellipse covers its bounding rectangle."
  (if (member kind '(:polygon :bezigon))
      (make-polygon* (polygon-record-coordinates record))
      (multiple-value-call #'make-rectangle* (bounding-rectangle* record))))

(defun reported-state (state-function presentation)
  "Return the state STATE-FUNCTION reports for PRESENTATION's object, or NIL.
STATE-FUNCTION is NIL, which reports nothing, or a function of one argument returning a keyword
or NIL. Signal an error naming the presentation type and the value when it returns anything
else."
  (when state-function
    (let ((state (funcall state-function (presentation-object presentation))))
      (unless (or (null state) (keywordp state))
        (error "The state function returned ~S for a ~S presentation; ~
                a state is a keyword or NIL."
               state (presentation-type presentation)))
      state)))

(defun walk-records (history pane &optional (ink-resolver #'resolve-ink) state-function)
  "Return elements for the records under HISTORY, depth first, numbered from 1 in painting order.
Return as further values the fill layers, most recently drawn first, as RGB-BENEATH takes them,
one for every :FILL and with RGB NIL for a fill of no fixed colour or a pattern;
PANE's resolved background; and an alist (presentation . element) in painting order.
A presentation becomes an element of its type, numbered before its descendants, which name it as
their container; its text joins its text descendants' strings with one space, its ink and
background are its first text descendant's, and its state is what REPORTED-STATE gives for
STATE-FUNCTION, NIL when STATE-FUNCTION is NIL. A text record becomes a :TEXT element. A shape
record becomes a :FILL element when filled, otherwise an :OUTLINE element; a pattern is a :FILL
with no ink. Other records are descended without an element.
INK-RESOLVER is called as RESOLVE-INK is, on every record ink and on PANE's background. Each
element's background is what lies beneath its sample points: the topmost earlier :FILL whose
region contains the point, else PANE's background. A :FILL of no fixed colour hides what lies
beneath it. An element is judged on its sample points with something known beneath them, as
SAMPLE-ELEMENT gives: it is partly unresolved when some other point lies on such a fill, and has
background NIL when every point does. A translucent ink is stored composited over that
background; an :OUTLINE keeps its lowest-contrast known sample."
  (let ((elements '())
        (presentations '())
        (layers '())
        (next-id 0)
        (background (values (funcall ink-resolver +background-ink+ pane))))
    (labels ((record-box (record)
               (multiple-value-list (bounding-rectangle* record)))
             (record-ink (record)
               (multiple-value-bind (rgb alpha)
                   (funcall ink-resolver (displayed-output-record-ink record) pane)
                 (values rgb (or alpha 1))))
             (emit (record kind container &rest initargs)
               (let ((element (apply #'make-element
                                     :id (incf next-id)
                                     :kind kind
                                     :box (record-box record)
                                     :container container
                                     initargs)))
                 (push element elements)
                 element))
             (emit-drawn (record kind container &rest initargs)
               (multiple-value-bind (rgb alpha) (record-ink record)
                 (multiple-value-bind (ink under partly-unresolved)
                     (sample-element kind (record-box record) rgb alpha layers background)
                   (apply #'emit record kind container
                          :ink ink :background under :partly-unresolved partly-unresolved
                          initargs))))
             (visit-presentation (record container)
               (let* ((element (emit record (presentation-kind record) container
                                     :state (reported-state state-function record)))
                      (before elements)
                      (id (element-id element)))
                 (push (cons record element) presentations)
                 (map-over-output-records (lambda (child) (visit child id)) record)
                 (let ((texts (remove :text (reverse (ldiff elements before))
                                      :key #'element-kind :test-not #'eq)))
                   (setf (element-text element)
                         (when texts
                           (format nil "~{~A~^ ~}" (mapcar #'element-text texts)))
                         (element-ink element) (and texts (element-ink (first texts)))
                         (element-background element)
                         (and texts (element-background (first texts)))))))
             (visit-fill (record kind container)
               (emit-drawn record :fill container)
               (multiple-value-bind (rgb alpha) (record-ink record)
                 (push (list (fill-region record kind) rgb (if rgb alpha 1)) layers)))
             (visit-shape (record kind container)
               (cond ((eq kind :pattern)
                      (emit record :fill container :ink nil
                            :background (nth-value 1 (sample-element :fill (record-box record)
                                                                     nil 1 layers background)))
                      (push (list (fill-region record kind) nil 1) layers))
                     ((shape-record-filled-p record)
                      (visit-fill record kind container))
                     (t
                      (emit-drawn record :outline container))))
             (visit (record container)
               (let ((shape (shape-record-kind record)))
                 (cond ((presentationp record)
                        (visit-presentation record container))
                       ((text-record-p record)
                        (emit-drawn record :text container
                                    :text (text-record-string record)))
                       (shape
                        (visit-shape record shape container))
                       (t
                        (map-over-output-records (lambda (child) (visit child container))
                                                 record))))))
      (map-over-output-records (lambda (record) (visit record nil)) history))
    (values (nreverse elements) layers background (nreverse presentations))))

(define-condition feedback-state-not-restored (error)
  ((presentation-type :initarg :presentation-type :reader feedback-state-presentation-type)
   (box :initarg :box :reader feedback-state-box))
  (:report (lambda (condition stream)
             (format stream "Unhighlighting the ~S presentation at ~S did not restore the drawing ~
                             it had at rest."
                     (feedback-state-presentation-type condition)
                     (feedback-state-box condition))))
  (:documentation "Signalled when a presentation, highlighted and then unhighlighted, no longer
draws what it drew at rest. PRESENTATION-TYPE is its presentation type; BOX its bounding box
(x1 y1 x2 y2)."))

(defun call-with-rendered-pane (frame-class pane-name function
                                &key (width 900) (height 60) frame-initargs)
  "Render the pane PANE-NAME of a fresh FRAME-CLASS frame headlessly and call FUNCTION on it.
The frame is made WIDTH by HEIGHT on a fresh fixed-width port, with FRAME-INITARGS added; its
display function runs once through a forced redisplay. FUNCTION is called with the frame and the
pane, with *APPLICATION-FRAME* bound to the frame, and its values are returned. The frame is
disowned and the port destroyed before returning, whether or not FUNCTION returns normally."
  (let* ((port (make-fixed-width-port))
         (frame nil))
    (unwind-protect
         (let* ((manager (find-frame-manager :port port))
                (made (apply #'make-application-frame frame-class
                             :frame-manager manager :width width :height height
                             frame-initargs))
                (pane (progn (setf frame made)
                             (find-pane-named made pane-name))))
           (let ((*application-frame* made))
             (redisplay-frame-pane made pane :force-p t)
             (funcall function made pane)))
      (when frame
        (disown-frame (frame-manager frame) frame))
      (destroy-port port))))

(defun capture-replay (pane)
  "Replay PANE's output history onto its medium and return the drawing primitives, in drawing
order. Each is a plist as *DRAWING-LOG* describes. Nothing is recorded during the replay, so the
history is left as it was. PANE draws on a FIXED-WIDTH-MEDIUM."
  (let ((*drawing-log* (make-array 64 :adjustable t :fill-pointer 0)))
    (with-output-recording-options (pane :record nil :draw t)
      (replay (stream-output-history pane) pane))
    (coerce *drawing-log* 'list)))

(defun same-ink-p (ink other)
  "True when INK and OTHER draw the same: they are EQUALP, or both are CLIME:TRANSFORMED-DESIGN
inks whose source designs are the same by this rule and whose transformations are
CLIM:TRANSFORMATION-EQUAL. A replayed pattern or image arrives as a fresh transformed design round
the record's own design, so two replays of one drawing hold distinct ink objects."
  (or (equalp ink other)
      (and (typep ink 'clime:transformed-design)
           (typep other 'clime:transformed-design)
           (same-ink-p (clime:transformed-design-design ink)
                       (clime:transformed-design-design other))
           (clim:transformation-equal (clime:transformed-design-transformation ink)
                                      (clime:transformed-design-transformation other)))))

(defun same-drawing-p (primitive other)
  "True when the drawing primitives PRIMITIVE and OTHER, plists as *DRAWING-LOG* describes, draw
the same: they hold the same keys in the same order, every value but the ink is EQUALP, and the
inks are the same by SAME-INK-P."
  (and (= (length primitive) (length other))
       (loop for (key value) on primitive by #'cddr
             for (other-key other-value) on other by #'cddr
             always (and (eq key other-key)
                         (if (eq key :ink)
                             (same-ink-p value other-value)
                             (equalp value other-value))))))

(defun added-primitives (exercised resting)
  "Return the primitives of EXERCISED that RESTING does not account for, by multiset difference
under SAME-DRAWING-P, in EXERCISED order. Each is returned as (:POSITION n . primitive), N its
zero-based position in EXERCISED."
  (let ((unmatched (copy-list resting)))
    (loop for primitive in exercised
          for position from 0
          for match = (member primitive unmatched :test #'same-drawing-p)
          if match
            do (setf unmatched (remove (first match) unmatched :count 1 :test #'eq))
          else
            collect (list* :position position primitive))))

(defun exercise-feedback-state (pane presentation)
  "Put PRESENTATION on PANE into its highlighted state, capture what is then drawn, and restore it.
Return two values: the primitives the highlighted replay draws beyond the resting one, each as
(:POSITION n . primitive) in highlighted-replay order; and the whole highlighted replay. A
primitive is a plist as *DRAWING-LOG* describes. The presentation is unhighlighted even on a
non-local exit. Signal FEEDBACK-STATE-NOT-RESTORED unless the replay after unhighlighting has
as many primitives as the resting one and each is the same drawing as its resting counterpart by
SAME-DRAWING-P. Highlighting goes through CLIM:HIGHLIGHT-OUTPUT-RECORD: pointer motion and
sheet repaints never reach a medium on a headless port."
  (let ((resting (capture-replay pane))
        (exercised nil))
    (unwind-protect
         (progn
           (highlight-output-record presentation pane :highlight)
           (setf exercised (capture-replay pane)))
      (highlight-output-record presentation pane :unhighlight))
    (let ((restored (capture-replay pane)))
      (unless (and (= (length resting) (length restored))
                   (every #'same-drawing-p resting restored))
        (error 'feedback-state-not-restored
               :presentation-type (presentation-type presentation)
               :box (multiple-value-list (bounding-rectangle* presentation)))))
    (values (added-primitives exercised resting) exercised)))

(defun union-box (boxes)
  "Return the box (x1 y1 x2 y2) bounding BOXES, a non-empty list of boxes."
  (loop for (x1 y1 x2 y2) in boxes
        minimize x1 into left
        minimize y1 into top
        maximize x2 into right
        maximize y2 into bottom
        finally (return (list left top right bottom))))

(defun filled-coverage (primitives box)
  "Return the fraction of the integer pixel grid of BOX (x1 y1 x2 y2) the filled PRIMITIVES cover.
A pixel counts once however many primitives cover it. A filled primitive covers at least one
pixel across each dimension of its box, so a 1 px stroke drawn as a thin rectangle counts."
  (destructuring-bind (x1 y1 x2 y2) box
    (let* ((width (max 1 (- x2 x1)))
           (height (max 1 (- y2 y1)))
           (grid (make-array (* width height) :element-type 'bit :initial-element 0)))
      (dolist (primitive primitives)
        (when (getf primitive :filled)
          (destructuring-bind (px1 py1 px2 py2) (getf primitive :box)
            (loop for y from (max py1 y1) below (min (max py2 (1+ py1)) (+ y1 height))
                  do (loop for x from (max px1 x1) below (min (max px2 (1+ px1)) (+ x1 width))
                           do (setf (sbit grid (+ (* (- y y1) width) (- x x1))) 1))))))
      (/ (count 1 grid) (* width height)))))

(defun box-within-p (inner outer)
  "True when the box INNER lies within the box OUTER, allowing 1 px on every side."
  (destructuring-bind (ix1 iy1 ix2 iy2) inner
    (destructuring-bind (ox1 oy1 ox2 oy2) outer
      (and (>= ix1 (1- ox1)) (>= iy1 (1- oy1)) (<= ix2 (1+ ox2)) (<= iy2 (1+ oy2))))))

(defun group-by-ink (primitives pane ink-resolver)
  "Return PRIMITIVES grouped by the colour their ink resolves to, in order of first appearance.
Each group is a list (rgb alpha primitives); RGB is NIL for an ink with no fixed colour.
INK-RESOLVER is called as RESOLVE-INK is."
  (let ((groups '()))
    (dolist (primitive primitives)
      (multiple-value-bind (rgb alpha) (funcall ink-resolver (getf primitive :ink) pane)
        (let* ((alpha (or alpha 1))
               (group (find-if (lambda (group)
                                 (and (equal rgb (first group)) (= alpha (second group))))
                               groups)))
          (if group
              (push primitive (third group))
              (push (list rgb alpha (list primitive)) groups)))))
    (nreverse (mapcar (lambda (group)
                        (list (first group) (second group) (reverse (third group))))
                      groups))))

(defun last-drawing-position (label exercised)
  "Return the position in EXERCISED of the last :TEXT primitive drawing the :TEXT element LABEL:
LABEL's text over LABEL's box rounded to integers. Return NIL when no primitive draws it."
  (let ((box (mapcar #'round (element-box label))))
    (position-if
     (lambda (primitive)
       (and (eq :text (getf primitive :shape))
            (equal (element-text label) (getf primitive :text))
            (equal box (getf primitive :box))))
     exercised :from-end t)))

(defun label-on-fill (label fill added exercised pane ink-resolver layers-before background)
  "Return the ink and background, as two values, the resting :TEXT element LABEL shows on the
highlighted :FILL element FILL. ADDED are the added primitives EXERCISE-FEEDBACK-STATE returns;
EXERCISED is the whole highlighted replay. LAYERS-BEFORE is a function of a position in
EXERCISED returning the filled primitives EXERCISED draws before it, resting and added alike, as
layers RGB-BENEATH takes over BACKGROUND, in that replay's own order.
LABEL is judged by its last drawing in EXERCISED, as LAST-DRAWING-POSITION finds it: that text
primitive, in its ink as INK-RESOLVER gives it, is sampled over LABEL's box against the layers
drawn before it. A cover is an added filled primitive drawn after that drawing whose box
contains the centre of LABEL's box, edges included; a primitive that only overlaps LABEL's box
covers nothing. Each cover, in drawing order, applies by its ink as INK-RESOLVER gives it:
- an opaque colour sets both ink and background to that colour;
- a translucent colour replaces each of ink and background by that colour composited over it at
  its alpha, a NIL staying NIL;
- an ink of no fixed colour sets both to NIL.
With no text primitive, LABEL's own ink shows on the fill's ink."
  (let* ((centre (first (sample-points :text (element-box label))))
         (text-position (last-drawing-position label exercised))
         (text (and text-position (nth text-position exercised))))
    (flet ((covers-label-p (primitive)
             (and (getf primitive :filled)
                  (> (getf primitive :position) text-position)
                  (destructuring-bind (x1 y1 x2 y2) (getf primitive :box)
                    (and (<= x1 (car centre) x2) (<= y1 (cdr centre) y2))))))
      (if (null text)
          (values (element-ink label) (element-ink fill))
          (multiple-value-bind (ink under)
              (multiple-value-bind (rgb alpha) (funcall ink-resolver (getf text :ink) pane)
                (sample-element :text (element-box label) rgb (or alpha 1)
                                (funcall layers-before text-position) background))
            (dolist (primitive added (values ink under))
              (when (covers-label-p primitive)
                (multiple-value-bind (rgb alpha)
                    (funcall ink-resolver (getf primitive :ink) pane)
                  (if (and rgb alpha (< alpha 1))
                      (setf ink (and ink (composite-over rgb alpha ink))
                            under (and under (composite-over rgb alpha under)))
                      (setf ink rgb
                            under rgb))))))))))

(defun feedback-elements (pane presentations resting background ink-resolver)
  "Exercise the highlight of each presentation on PANE and return the elements it adds.
PRESENTATIONS is an alist (presentation . element) in painting order; RESTING the resting
elements; BACKGROUND the pane background WALK-RECORDS returned with them. Every element returned
has state :HIGHLIGHTED, evidence :RECORDED, the presentation's element as container, and the next
id after RESTING's. For each presentation, in order:
- each added text primitive is a :TEXT element;
- the other added primitives, grouped by resolved ink, are each one element over the group's
  union box: a :FILL when the group's filled primitives cover at least 90% of that box, otherwise
  an :OUTLINE;
- each resting :TEXT element of the presentation lying within a highlighted :FILL is judged on
  it again as a :TEXT element, its colours from LABEL-ON-FILL, unless its last drawing is an
  added text primitive, which already judges it.
Inks resolve through INK-RESOLVER. A mark at position P of the highlighted replay is sampled, as
WALK-RECORDS samples, against the layers drawn before it in that replay: every filled primitive
of the replay, resting or added, whose position is below P, most recently drawn first, over
BACKGROUND. Each layer covers its primitive's box, a filled polygon included, and is of no fixed
colour when its ink has none. A group element is sampled at its earliest primitive's position,
so no group lies beneath itself. Each added text element and each group element is partly
unresolved as SAMPLE-ELEMENT reports it; a re-judged label never is."
  (let ((next-id (length resting))
        (made '()))
    (flet ((emit (&rest initargs)
             (let ((element (apply #'make-element :id (incf next-id)
                                                  :state :highlighted
                                                  :evidence :recorded
                                                  initargs)))
               (push element made)
               element))
           (replay-layers (exercised)
             (loop for primitive in exercised
                   for position from 0
                   when (getf primitive :filled)
                     collect (multiple-value-bind (rgb alpha)
                                 (funcall ink-resolver (getf primitive :ink) pane)
                               (list position
                                     (apply #'make-rectangle* (getf primitive :box))
                                     rgb
                                     (if rgb (or alpha 1) 1))))))
      (loop for (presentation . owner) in presentations
            for container = (element-id owner)
            do (multiple-value-bind (added exercised) (exercise-feedback-state pane presentation)
                 (let ((fills '())
                       (drawn (replay-layers exercised)))
                   (labels ((layers-before (position)
                              (let ((before '()))
                                (loop for (at . layer) in drawn
                                      while (< at position)
                                      do (push layer before))
                                before))
                            (sampled (kind box rgb alpha position)
                              (multiple-value-list
                               (sample-element kind box rgb alpha
                                               (layers-before position) background))))
                     (dolist (primitive added)
                       (when (eq :text (getf primitive :shape))
                         (multiple-value-bind (rgb alpha)
                             (funcall ink-resolver (getf primitive :ink) pane)
                           (destructuring-bind (ink under partly-unresolved)
                               (sampled :text (getf primitive :box) rgb (or alpha 1)
                                        (getf primitive :position))
                             (emit :kind :text :text (getf primitive :text)
                                   :box (getf primitive :box) :container container
                                   :ink ink :background under
                                   :partly-unresolved partly-unresolved)))))
                     (loop for (rgb alpha primitives)
                             in (group-by-ink (remove :text added
                                                      :key (lambda (primitive)
                                                             (getf primitive :shape)))
                                              pane ink-resolver)
                           for box = (union-box (mapcar (lambda (primitive)
                                                          (getf primitive :box))
                                                        primitives))
                           for kind = (if (>= (filled-coverage primitives box) 9/10)
                                          :fill
                                          :outline)
                           for earliest = (reduce #'min primitives
                                                  :key (lambda (primitive)
                                                         (getf primitive :position)))
                           do (destructuring-bind (ink under partly-unresolved)
                                  (sampled kind box rgb alpha earliest)
                                (let ((element (emit :kind kind :box box :container container
                                                     :ink ink :background under
                                                     :partly-unresolved partly-unresolved)))
                                  (when (eq kind :fill)
                                    (push (cons element primitives) fills)))))
                     (dolist (label resting)
                       (when (and (eq :text (element-kind label))
                                  (eql container (element-container label)))
                         (let ((cover (find-if (lambda (fill)
                                                 (box-within-p (element-box label)
                                                               (element-box (car fill))))
                                               fills)))
                           (when (and cover
                                     (not (member (last-drawing-position label exercised)
                                                  added
                                                  :key (lambda (primitive)
                                                         (getf primitive :position)))))
                             (multiple-value-bind (ink under)
                                 (label-on-fill label (car cover) added exercised
                                                pane ink-resolver #'layers-before background)
                               (emit :kind :text :text (element-text label)
                                     :box (element-box label) :container container
                                     :ink ink :background under))))))))))
      (nreverse made))))

(defun snapshot-frame-pane (frame-class pane-name
                            &key (width 900) (height 60) frame-initargs
                              ((:ink-resolver ink-resolver) #'resolve-ink)
                              ((:state-function state-function) nil)
                              ((:exercise-feedback exercise-feedback) t))
  "Render the pane PANE-NAME of a fresh FRAME-CLASS frame headlessly and return its snapshot.
The frame is made WIDTH by HEIGHT on a fresh fixed-width port, with FRAME-INITARGS added; its
display function runs once through a forced redisplay. INK-RESOLVER, called as RESOLVE-INK is,
gives the colour of every ink the walk meets and of the pane's background. STATE-FUNCTION, when
non-NIL, is called with the object of each presentation; its keyword result becomes that
element's reported state, and any other non-NIL result signals an error. When EXERCISE-FEEDBACK
is true, each presentation's highlight is exercised after the resting walk and the elements
FEEDBACK-ELEMENTS makes of it follow the resting ones; NIL gives the resting drawing alone. The
frame is disowned and the port destroyed before returning, whether or not the render succeeds."
  (call-with-rendered-pane
   frame-class pane-name
   (lambda (frame pane)
     (declare (ignore frame))
     (multiple-value-bind (elements layers background presentations)
         (walk-records (stream-output-history pane) pane ink-resolver state-function)
       (declare (ignore layers))
       (make-snapshot :view (intern (string pane-name) :keyword)
                      :elements (if exercise-feedback
                                    (append elements
                                            (feedback-elements pane presentations elements
                                                               background ink-resolver))
                                    elements))))
   :width width :height height :frame-initargs frame-initargs))

(defun check-frame-pane (spec frame-class pane-name &key observers render thresholds)
  "Judge the pane PANE-NAME of a fresh FRAME-CLASS frame against SPEC, rendered headlessly.
RENDER is a plist of SNAPSHOT-FRAME-PANE keyword arguments; OBSERVERS and THRESHOLDS are passed
to RUN-CHECK.
Return what RUN-CHECK returns: the findings, the report string, and the snapshot."
  (run-check spec
             (lambda () (apply #'snapshot-frame-pane frame-class pane-name render))
             :observers observers
             :thresholds thresholds))
