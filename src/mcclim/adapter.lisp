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
                #:map-over-output-records
                #:pane-background
                #:pane-foreground
                #:redisplay-frame-pane
                #:stream-output-history)
  (:import-from #:ekphrasis
                #:make-element
                #:make-snapshot
                #:run-check)
  (:import-from #:ekphrasis/src/mcclim/medium
                #:make-fixed-width-port)
  (:import-from #:ekphrasis/src/mcclim/records
                #:text-record-p
                #:text-record-string)
  (:export #:snapshot-frame-pane
           #:check-frame-pane
           #:resolve-ink))

(in-package #:ekphrasis/src/mcclim/adapter)

(defun resolve-ink (ink pane)
  "Return the fixed colour INK paints on PANE as two values: an sRGB list (r g b) and its alpha.
+FOREGROUND-INK+ and +BACKGROUND-INK+ resolve through PANE's own foreground and background, not
its medium's, which keep their defaults on a headless port. Return NIL when INK has no fixed
colour (a flipping ink, a pattern, or an image)."
  (cond ((eq ink +foreground-ink+)
         (resolve-ink (pane-foreground pane) pane))
        ((eq ink +background-ink+)
         (resolve-ink (pane-background pane) pane))
        ((clime:indirect-ink-p ink)
         (resolve-ink (clime:indirect-ink-ink ink) pane))
        ((typep ink 'clim:color)
         (multiple-value-bind (r g b alpha) (clime:color-rgba ink)
           (values (list r g b) alpha)))
        (t nil)))

(defun walk-records (history pane)
  "Return elements for the records under HISTORY, depth first, numbered from 1 in painting order."
  (let ((elements '())
        (next-id 0)
        (background (resolve-ink +background-ink+ pane)))
    (labels ((visit (record)
               (if (text-record-p record)
                   (push (make-element :id (incf next-id)
                                       :kind :text
                                       :text (text-record-string record)
                                       :box (multiple-value-list (bounding-rectangle* record))
                                       :container nil
                                       :ink (values (resolve-ink
                                                     (displayed-output-record-ink record) pane))
                                       :background background)
                         elements)
                   (map-over-output-records #'visit record))))
      (map-over-output-records #'visit history))
    (nreverse elements)))

(defun snapshot-frame-pane (frame-class pane-name &key (width 900) (height 60) frame-initargs)
  "Render the pane PANE-NAME of a fresh FRAME-CLASS frame headlessly and return its snapshot.
The frame is made WIDTH by HEIGHT on a fresh fixed-width port, with FRAME-INITARGS added; its
display function runs once through a forced redisplay. The frame is disowned and the port
destroyed before returning, whether or not the render succeeds."
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
                            :elements (walk-records (stream-output-history pane) pane))))
      (when frame
        (disown-frame (frame-manager frame) frame))
      (destroy-port port))))

(defun check-frame-pane (spec frame-class pane-name &key observers render)
  "Judge the pane PANE-NAME of a fresh FRAME-CLASS frame against SPEC, rendered headlessly.
RENDER is a plist of SNAPSHOT-FRAME-PANE keyword arguments; OBSERVERS are passed to RUN-CHECK.
Return what RUN-CHECK returns: the findings, the report string, and the snapshot."
  (run-check spec
             (lambda () (apply #'snapshot-frame-pane frame-class pane-name render))
             :observers observers))
