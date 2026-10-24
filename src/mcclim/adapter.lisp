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
                #:stream-output-history)
  (:import-from #:ekphrasis
                #:composite-over
                #:contrast-ratio
                #:element-background
                #:element-id
                #:element-ink
                #:element-kind
                #:element-text
                #:make-element
                #:make-snapshot
                #:run-check)
  (:import-from #:ekphrasis/src/mcclim/medium
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
           #:resolve-ink))

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
LAYERS is a list of (region rgb alpha), most recently drawn first. The first layer whose region
contains the point gives its RGB, composited over what lies beneath that layer when ALPHA is
below 1."
  (loop for (layer . below) on layers
        for (region rgb alpha) = layer
        when (region-contains-position-p region x y)
          return (if (< alpha 1)
                     (composite-over rgb alpha (rgb-beneath x y below background))
                     rgb)
        finally (return background)))

(defun ink-over (rgb alpha background)
  "Return the sRGB list ink RGB at ALPHA shows over BACKGROUND.
An opaque ink is RGB itself; a translucent one over no known background is NIL."
  (cond ((>= alpha 1) rgb)
        (background (composite-over rgb alpha background))
        (t nil)))

(defun sample-element (kind box rgb alpha layers background)
  "Return the ink and background of an element of KIND and BOX drawn in RGB at ALPHA, as two values.
Each sample point takes what lies beneath it under LAYERS over BACKGROUND, and the ink as it shows
there; the point with the lowest contrast ratio wins, the first point when no ratio is known.
With RGB NIL, return NIL and what lies beneath the first sample point."
  (let ((points (sample-points kind box)))
    (flet ((beneath (point)
             (rgb-beneath (car point) (cdr point) layers background)))
      (if (null rgb)
          (values nil (beneath (first points)))
          (let (best-ink best-background best-ratio)
            (loop for point in points
                  for firstp = t then nil
                  for under = (beneath point)
                  for ink = (ink-over rgb alpha under)
                  for ratio = (and ink under (contrast-ratio ink under))
                  when (or firstp
                           (and ratio (or (null best-ratio) (< ratio best-ratio))))
                    do (setf best-ink ink
                             best-background under
                             best-ratio ratio))
            (values best-ink best-background))))))

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
A presentation becomes an element of its type, numbered before its descendants, which name it as
their container; its text joins its text descendants' strings with one space, its ink and
background are its first text descendant's, and its state is what REPORTED-STATE gives for
STATE-FUNCTION, NIL when STATE-FUNCTION is NIL. A text record becomes a :TEXT element. A shape
record becomes a :FILL element when filled, otherwise an :OUTLINE element; a pattern is a :FILL
with no ink. Other records are descended without an element.
INK-RESOLVER is called as RESOLVE-INK is, on every record ink and on PANE's background. Each
element's background is what lies beneath its sample points: the topmost earlier :FILL with a
resolved ink whose region contains the point, else PANE's background. A translucent ink is
stored composited over that background; an :OUTLINE keeps its lowest-contrast sample."
  (let ((elements '())
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
                 (multiple-value-bind (ink under)
                     (sample-element kind (record-box record) rgb alpha layers background)
                   (apply #'emit record kind container :ink ink :background under initargs))))
             (visit-presentation (record container)
               (let* ((element (emit record (presentation-kind record) container
                                     :state (reported-state state-function record)))
                      (before elements)
                      (id (element-id element)))
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
                 (when rgb
                   (push (list (fill-region record kind) rgb alpha) layers))))
             (visit-shape (record kind container)
               (cond ((eq kind :pattern)
                      (emit record :fill container :ink nil
                            :background (nth-value 1 (sample-element :fill (record-box record)
                                                                     nil 1 layers background))))
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
    (nreverse elements)))

(defun snapshot-frame-pane (frame-class pane-name
                            &key (width 900) (height 60) frame-initargs
                              ((:ink-resolver ink-resolver) #'resolve-ink)
                              ((:state-function state-function) nil))
  "Render the pane PANE-NAME of a fresh FRAME-CLASS frame headlessly and return its snapshot.
The frame is made WIDTH by HEIGHT on a fresh fixed-width port, with FRAME-INITARGS added; its
display function runs once through a forced redisplay. INK-RESOLVER, called as RESOLVE-INK is,
gives the colour of every ink the walk meets and of the pane's background. STATE-FUNCTION, when
non-NIL, is called with the object of each presentation; its keyword result becomes that
element's reported state, and any other non-NIL result signals an error. The frame is disowned
and the port destroyed before returning, whether or not the render succeeds."
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
             (make-snapshot :view (intern (string pane-name) :keyword)
                            :elements (walk-records (stream-output-history pane) pane
                                                    ink-resolver state-function))))
      (when frame
        (disown-frame (frame-manager frame) frame))
      (destroy-port port))))

(defun check-frame-pane (spec frame-class pane-name &key observers render thresholds)
  "Judge the pane PANE-NAME of a fresh FRAME-CLASS frame against SPEC, rendered headlessly.
RENDER is a plist of SNAPSHOT-FRAME-PANE keyword arguments; OBSERVERS and THRESHOLDS are passed
to RUN-CHECK.
Return what RUN-CHECK returns: the findings, the report string, and the snapshot."
  (run-check spec
             (lambda () (apply #'snapshot-frame-pane frame-class pane-name render))
             :observers observers
             :thresholds thresholds))
