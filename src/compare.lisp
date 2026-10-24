;;;; src/compare.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; Judge a snapshot against a spec. Pure over its arguments apart from the optional callbacks.

(defpackage #:ekphrasis/src/compare
  (:use #:cl)
  (:import-from #:ekphrasis/src/model
                #:element-background
                #:element-box
                #:element-container
                #:element-id
                #:element-ink
                #:element-kind
                #:element-state
                #:element-text
                #:make-finding
                #:snapshot-elements
                #:snapshot-view)
  (:import-from #:ekphrasis/src/spec
                #:parse-spec
                #:spec-contrast
                #:spec-elements
                #:spec-order
                #:spec-states
                #:spec-element-id
                #:spec-element-kind
                #:spec-element-label
                #:spec-element-state
                #:spec-element-states)
  (:import-from #:ekphrasis/src/contrast
                #:below-threshold-p
                #:contrast-ratio
                #:resolve-thresholds)
  (:import-from #:ekphrasis/src/rgb
                #:rgb-match-p
                #:rgb-to-hex)
  (:export #:compare-snapshot))

(in-package #:ekphrasis/src/compare)

(defun label-matches-p (label text)
  "True when drawn TEXT is LABEL. A NIL label matches NIL or empty text."
  (if label
      (and text (string= label text))
      (or (null text) (string= text ""))))

(defun reading-order-p (a b)
  "True when element A comes before element B in reading order: by box top, then by box left.
An element with no box reads as if at the origin."
  (let ((top-a (or (second (element-box a)) 0))
        (top-b (or (second (element-box b)) 0)))
    (or (< top-a top-b)
        (and (= top-a top-b)
             (< (or (first (element-box a)) 0) (or (first (element-box b)) 0))))))

(defun pair-elements (spec-elements elements)
  "Pair each of SPEC-ELEMENTS with the drawn element among ELEMENTS that stands for it.
ELEMENTS are the drawn elements in scope, in painting order.
First, each spec element in spec order takes the first unpaired drawn element of its kind whose
text matches its label. Then, per kind, the spec elements still unpaired take the unpaired drawn
elements of that kind position by position, in reading order.
Return two values: a list of conses (spec-element . element) in spec order, with element NIL
when nothing stands for it; and the drawn elements left unpaired, in painting order."
  (let ((pairs (mapcar (lambda (spec-element) (cons spec-element nil)) spec-elements))
        (taken '()))
    (flet ((free-p (element)
             (not (member element taken :test #'eq)))
           (of-kind-p (kind element)
             (eq kind (element-kind element))))
      (dolist (pair pairs)
        (let* ((spec-element (car pair))
               (match (find-if (lambda (element)
                                 (and (free-p element)
                                      (of-kind-p (spec-element-kind spec-element) element)
                                      (label-matches-p (spec-element-label spec-element)
                                                       (element-text element))))
                               elements)))
          (when match
            (setf (cdr pair) match)
            (push match taken))))
      (dolist (kind (remove-duplicates (mapcar #'spec-element-kind spec-elements)
                                       :from-end t))
        (let ((unpaired (remove-if-not (lambda (pair)
                                         (and (null (cdr pair))
                                              (eq kind (spec-element-kind (car pair)))))
                                       pairs))
              (free (stable-sort (remove-if-not (lambda (element)
                                                  (and (free-p element)
                                                       (of-kind-p kind element)))
                                                (copy-list elements))
                                 #'reading-order-p)))
          (loop for pair in unpaired
                for element in free
                do (setf (cdr pair) element)
                   (push element taken))))
      (values pairs (remove-if-not #'free-p elements)))))

(defun missing-finding (spec-element view)
  "Return the :MISSING finding for SPEC-ELEMENT, which no drawn element in VIEW stands for."
  (make-finding :check :missing
                :severity :error
                :spec-id (spec-element-id spec-element)
                :view view
                :container nil
                :element-id nil
                :box nil
                :expected (spec-element-label spec-element)
                :measured nil
                :evidence (list :recorded)
                :detail (list :kind (spec-element-kind spec-element))))

(defun label-finding (spec-element element view)
  "Return the :LABEL finding for SPEC-ELEMENT, which ELEMENT in VIEW stands for with other text."
  (make-finding :check :label
                :severity :error
                :spec-id (spec-element-id spec-element)
                :view view
                :container (element-container element)
                :element-id (element-id element)
                :box (element-box element)
                :expected (spec-element-label spec-element)
                :measured (element-text element)
                :evidence (list :recorded)
                :detail (list :kind (spec-element-kind spec-element))))

(defun unexpected-finding (element view)
  "Return the :UNEXPECTED warning for ELEMENT, drawn in VIEW with no spec element it stands for."
  (make-finding :check :unexpected
                :severity :warning
                :spec-id nil
                :view view
                :container (element-container element)
                :element-id (element-id element)
                :box (element-box element)
                :expected nil
                :measured (element-text element)
                :evidence (list :recorded)
                :detail (list :kind (element-kind element))))

(defun looks-like-p (element appearance)
  "True when ELEMENT's colours match APPEARANCE, a plist (:INK rgb :BACKGROUND rgb) holding
either key or both: each colour given matches ELEMENT's by RGB-MATCH-P. An element with no
fixed colour where one is given does not match."
  (loop for (key rgb) on appearance by #'cddr
        for colour = (if (eq key :ink) (element-ink element) (element-background element))
        always (and colour (rgb-match-p colour rgb))))

(defun drawn-state (element kind states)
  "Return the state ELEMENT's drawing shows, judged against the appearances STATES declares for
KIND. STATES is a parsed :STATES alist.
Return the first declared state with an appearance that ELEMENT looks like; :UNKNOWN when KIND
declares appearances and none matches; NIL when KIND declares no appearance."
  (let ((shown (remove-if-not #'cdr (cdr (assoc kind states)))))
    (when shown
      (or (car (find-if (lambda (state) (looks-like-p element (cdr state))) shown))
          :unknown))))

(defun hex-or-nil (rgb)
  "Return the sRGB list RGB as \"#RRGGBB\", or NIL when RGB is NIL."
  (and rgb (rgb-to-hex rgb)))

(defun state-findings (spec-element element states view)
  "Return the findings ELEMENT in VIEW earns for state as SPEC-ELEMENT's counterpart, in order:
a :STATE error, then a :STATE-APPEARANCE error, either or both absent.
STATES is a parsed :STATES alist. Return NIL when SPEC-ELEMENT names neither :STATE nor :STATES.
The state judged is the one the client reports, or, with no report, the one the drawing
shows. A :STATE error is made when that state is not the expected :STATE, or, with
only :STATES given, not among them. A :STATE-APPEARANCE error is made when the client reports
one state and the drawing shows another."
  (let ((expected (spec-element-state spec-element))
        (allowed (spec-element-states spec-element)))
    (when (or expected allowed)
      (let* ((reported (element-state element))
             (drawn (drawn-state element (spec-element-kind spec-element) states))
             (actual (or reported drawn))
             (detail (list :reported reported
                           :drawn drawn
                           :ink (hex-or-nil (element-ink element))
                           :background (hex-or-nil (element-background element))
                           :kind (spec-element-kind spec-element))))
        (flet ((state-finding (check expected measured)
                 (make-finding :check check
                               :severity :error
                               :spec-id (spec-element-id spec-element)
                               :view view
                               :container (element-container element)
                               :element-id (element-id element)
                               :box (element-box element)
                               :expected expected
                               :measured measured
                               :evidence (list :recorded)
                               :detail detail)))
          (append
           (cond (expected
                  (unless (eq actual expected)
                    (list (state-finding :state expected actual))))
                 ((not (member actual allowed :test #'eq))
                  (list (state-finding :state allowed actual))))
           (when (and reported drawn (not (eq reported drawn)))
             (list (state-finding :state-appearance reported drawn)))))))))

(defun kept-in-order (positions)
  "Return the indices into POSITIONS, a list of distinct integers, of one longest increasing
subsequence, in ascending order.
Among predecessors of equal length, the one with the lowest position is kept, and among
subsequences of equal length, the one ending at the lowest position, so the choice is fixed."
  (let* ((vector (coerce positions 'vector))
         (count (length vector))
         (lengths (make-array count :initial-element 1))
         (previous (make-array count :initial-element nil)))
    (dotimes (i count)
      (dotimes (j i)
        (when (and (< (aref vector j) (aref vector i))
                   (or (> (1+ (aref lengths j)) (aref lengths i))
                       (and (= (1+ (aref lengths j)) (aref lengths i))
                            (< (aref vector j) (aref vector (aref previous i))))))
          (setf (aref lengths i) (1+ (aref lengths j))
                (aref previous i) j))))
    (let ((end nil))
      (dotimes (i count)
        (when (or (null end)
                  (> (aref lengths i) (aref lengths end))
                  (and (= (aref lengths i) (aref lengths end))
                       (< (aref vector i) (aref vector end))))
          (setf end i)))
      (loop with kept = '()
            for i = end then (aref previous i)
            while i
            do (push i kept)
            finally (return kept)))))

(defun box-union (boxes)
  "Return the smallest box (x1 y1 x2 y2) holding every box in BOXES, or NIL when BOXES holds
none. NIL entries are skipped."
  (let ((boxes (remove nil boxes)))
    (when boxes
      (list (reduce #'min boxes :key #'first)
            (reduce #'min boxes :key #'second)
            (reduce #'max boxes :key #'third)
            (reduce #'max boxes :key #'fourth)))))

(defun order-key (axis element)
  "Return where ELEMENT is drawn along AXIS: its box left for :LEFT-TO-RIGHT, its box top for
:TOP-TO-BOTTOM. An element with no box sits at 0."
  (or (if (eq axis :top-to-bottom)
          (second (element-box element))
          (first (element-box element)))
      0))

(defun order-finding (group pairs view)
  "Return the :ORDER finding for GROUP, an :ORDER group (axis id ...), or NIL when it earns none.
PAIRS are the conses (spec-element . element) PAIR-ELEMENTS returns. Only members stood for by
a drawn element are judged, and fewer than two are not judged. Members are placed by where they
are drawn along the axis, never by painting order; ties keep the expected order. The detail's
:DISPLACED lists the members outside one longest run already in expected order."
  (destructuring-bind (axis &rest ids) group
    (let* ((members (loop for id in ids
                          for element = (cdr (find id pairs
                                                   :key (lambda (pair)
                                                          (spec-element-id (car pair)))))
                          when element
                            collect (cons id element)))
           (drawn (stable-sort (copy-list members) #'<
                               :key (lambda (member) (order-key axis (cdr member)))))
           (expected-ids (mapcar #'car members))
           (drawn-ids (mapcar #'car drawn)))
      (when (and (>= (length members) 2)
                 (not (equal expected-ids drawn-ids)))
        (let* ((positions (mapcar (lambda (id) (position id expected-ids)) drawn-ids))
               (kept (kept-in-order positions)))
          (make-finding :check :order
                        :severity :error
                        :spec-id nil
                        :view view
                        :container nil
                        :element-id nil
                        :box (box-union (mapcar (lambda (member) (element-box (cdr member)))
                                                members))
                        :expected expected-ids
                        :measured drawn-ids
                        :evidence (list :recorded)
                        :detail (list :axis axis
                                      :displaced (loop for id in drawn-ids
                                                       for index from 0
                                                       unless (member index kept)
                                                         collect id))))))))

(defun contrast-category (element)
  "Return the contrast category ELEMENT is judged in: :TEXT for a :TEXT mark, :NON-TEXT for an
:OUTLINE mark, or NIL for an element that is not judged for contrast.
Fills and toolkit kinds return NIL: a presentation's colours repeat those of its label."
  (case (element-kind element)
    (:text :text)
    (:outline :non-text)))

(defun mark-facts (element container-spec-id)
  "Return the plist of facts about ELEMENT a perceivability finding's detail carries, each key only
when it has a value, in order:
- :LABEL, ELEMENT's text, for a :TEXT mark with a container;
- :CONTAINER-SPEC-ID, CONTAINER-SPEC-ID, the id of the spec element ELEMENT's container stands for;
- :STATE :HIGHLIGHTED, when ELEMENT's state is :HIGHLIGHTED.
Any other state is not carried."
  (append (when (and (eq :text (element-kind element))
                     (element-container element)
                     (element-text element))
            (list :label (element-text element)))
          (when container-spec-id
            (list :container-spec-id container-spec-id))
          (when (eq :highlighted (element-state element))
            (list :state :highlighted))))

(defun contrast-finding (element spec-id view ratio threshold category &optional container-spec-id)
  "Return the :CONTRAST error for ELEMENT in VIEW, whose ink against its background has RATIO,
below THRESHOLD for CATEGORY. SPEC-ID is the id of the spec element ELEMENT stands for, or NIL.
CONTAINER-SPEC-ID is the id of the spec element ELEMENT's container stands for, or NIL. The
detail ends with the facts MARK-FACTS returns."
  (make-finding :check :contrast
                :severity :error
                :spec-id spec-id
                :view view
                :container (element-container element)
                :element-id (element-id element)
                :box (element-box element)
                :expected threshold
                :measured ratio
                :evidence (list :recorded)
                :detail (append (list :ratio ratio
                                      :threshold threshold
                                      :ink (rgb-to-hex (element-ink element))
                                      :background (rgb-to-hex (element-background element))
                                      :category category
                                      :kind (element-kind element))
                                (mark-facts element container-spec-id))))

(defun unresolved-ink-finding (element spec-id view category &optional container-spec-id)
  "Return the :UNRESOLVED-INK warning for ELEMENT in VIEW, whose ink or background has no fixed
colour. SPEC-ID is the id of the spec element ELEMENT stands for, or NIL. CONTAINER-SPEC-ID is
the id of the spec element ELEMENT's container stands for, or NIL.
The detail's :UNRESOLVED lists which of :INK and :BACKGROUND is missing, and the detail ends
with the facts MARK-FACTS returns."
  (make-finding :check :unresolved-ink
                :severity :warning
                :spec-id spec-id
                :view view
                :container (element-container element)
                :element-id (element-id element)
                :box (element-box element)
                :expected nil
                :measured nil
                :evidence (list :recorded)
                :detail (append (list :unresolved (append (unless (element-ink element)
                                                            (list :ink))
                                                          (unless (element-background element)
                                                            (list :background)))
                                      :category category
                                      :kind (element-kind element))
                                (mark-facts element container-spec-id))))

(defun perceivability-finding (element spec-id view thresholds &optional container-spec-id)
  "Return the finding ELEMENT in VIEW earns for contrast, or NIL when it earns none.
THRESHOLDS is a resolved plist (:TEXT text :NON-TEXT non-text). A :TEXT or :OUTLINE mark whose
ink against its background is strictly below its category's threshold earns a :CONTRAST error;
one with no fixed ink or background earns an :UNRESOLVED-INK warning. The ratio is not rounded.
SPEC-ID is the id of the spec element ELEMENT stands for, or NIL. CONTAINER-SPEC-ID is the id of
the spec element ELEMENT's container stands for, or NIL."
  (let ((category (contrast-category element)))
    (when category
      (if (and (element-ink element) (element-background element))
          (let ((ratio (contrast-ratio (element-ink element) (element-background element)))
                (threshold (getf thresholds category)))
            (when (below-threshold-p ratio threshold)
              (contrast-finding element spec-id view ratio threshold category
                                container-spec-id)))
          (unresolved-ink-finding element spec-id view category container-spec-id)))))

(defun compare-snapshot (snapshot spec &key thresholds on-examine on-finding)
  "Judge SNAPSHOT against SPEC and return the findings in the order made.
SPEC is a spec record or spec data; it is parsed with PARSE-SPEC, which may signal MALFORMED-SPEC.
Presence is judged over the elements drawn on the view itself, outside any container: a spec
element nothing stands for is :MISSING; one stood for by an element with other text is a :LABEL
error; an unpaired element of a kind the spec names is an :UNEXPECTED warning. Kinds the spec
does not name are not judged for presence. A paired element whose spec element names :STATE or
:STATES is judged for state with it: a :STATE error when it is in the wrong state, and a
:STATE-APPEARANCE error when the state the client reports is not the one its drawing shows.
Order is judged next: each :ORDER group whose drawn members sit out of the expected sequence
along its axis gives one :ORDER error, judged on drawn position and never on painting order.
Perceivability is judged over every element, in painting order: a :TEXT or :OUTLINE mark below
its contrast threshold is a :CONTRAST error, and one with no fixed ink or background is an
:UNRESOLVED-INK warning. Fills and toolkit kinds are not judged for contrast. The detail of such
a finding also carries the mark's text as :LABEL for a :TEXT mark inside a control, the spec id
the control stands for as :CONTAINER-SPEC-ID, and :STATE :HIGHLIGHTED for a highlight-only mark.
THRESHOLDS is a plist (:TEXT r :NON-TEXT r); each key it sets overrides the spec's :CONTRAST
clause, which overrides *DEFAULT-CONTRAST-THRESHOLDS*.
ON-EXAMINE, when non-NIL, is called with each element of SNAPSHOT in painting order, before its
perceivability is judged.
ON-FINDING, when non-NIL, is called with each finding as it is made."
  (let* ((spec (parse-spec spec))
         (view (snapshot-view snapshot))
         (elements (snapshot-elements snapshot))
         (spec-elements (spec-elements spec))
         (named-kinds (mapcar #'spec-element-kind spec-elements))
         (thresholds (resolve-thresholds thresholds (spec-contrast spec)))
         (stood-for '())
         (findings '()))
    (flet ((emit (finding)
             (when on-finding
               (funcall on-finding finding))
             (push finding findings))
           (container-spec-id (element)
             (let ((container (element-container element)))
               (and container
                    (cdr (find container stood-for
                               :key (lambda (entry) (element-id (car entry)))))))))
      (multiple-value-bind (pairs unpaired)
          (pair-elements spec-elements (remove-if #'element-container elements))
        (loop for (spec-element . element) in pairs
              do (cond ((null element)
                        (emit (missing-finding spec-element view)))
                       (t
                        (push (cons element (spec-element-id spec-element)) stood-for)
                        (unless (label-matches-p (spec-element-label spec-element)
                                                 (element-text element))
                          (emit (label-finding spec-element element view)))
                        (dolist (finding (state-findings spec-element element
                                                         (spec-states spec) view))
                          (emit finding)))))
        (dolist (element unpaired)
          (when (member (element-kind element) named-kinds :test #'eq)
            (emit (unexpected-finding element view))))
        (dolist (group (spec-order spec))
          (let ((finding (order-finding group pairs view)))
            (when finding
              (emit finding)))))
      (dolist (element elements)
        (when on-examine
          (funcall on-examine element))
        (let ((finding (perceivability-finding element
                                               (cdr (assoc element stood-for :test #'eq))
                                               view
                                               thresholds
                                               (container-spec-id element))))
          (when finding
            (emit finding)))))
    (nreverse findings)))
