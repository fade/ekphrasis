;;;; src/model.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; Records shared by every layer: drawn elements, the snapshot of a view, and findings.

(defpackage #:ekphrasis/src/model
  (:use #:cl)
  (:export #:element
           #:make-element
           #:element-p
           #:element-id
           #:element-kind
           #:element-text
           #:element-box
           #:element-container
           #:element-state
           #:element-ink
           #:element-background
           #:element-partly-unresolved
           #:element-evidence
           #:snapshot
           #:make-snapshot
           #:snapshot-p
           #:snapshot-view
           #:snapshot-elements
           #:finding
           #:make-finding
           #:finding-p
           #:finding-check
           #:finding-severity
           #:finding-spec-id
           #:finding-view
           #:finding-container
           #:finding-element-id
           #:finding-box
           #:finding-expected
           #:finding-measured
           #:finding-evidence
           #:finding-detail
           #:failing-findings
           #:element-to-plist
           #:plist-to-element
           #:finding-to-plist
           #:plist-to-finding
           #:finding-where
           #:finding-why))

(in-package #:ekphrasis/src/model)

(defstruct (element (:copier nil))
  "One mark a view draws, as plain data.
ID is an integer giving painting order, starting at 1.
KIND is a keyword: the toolkit's own type, or :TEXT, :OUTLINE or :FILL.
TEXT is the drawn string, or NIL.
BOX is the list (x1 y1 x2 y2) as recorded.
CONTAINER is the id of the enclosing element, or NIL for the view itself.
STATE is the state the client reports, or NIL.
INK and BACKGROUND are sRGB lists (r g b) in [0,1], or NIL when the colour is not fixed.
PARTLY-UNRESOLVED is true when BACKGROUND was taken from some sample points while others had
nothing known beneath them, otherwise NIL.
EVIDENCE is :RECORDED (read from the toolkit's records) or :SHOWN (read from an image)."
  (id 0 :type integer)
  (kind :text :type keyword)
  (text nil :type (or null string))
  (box nil :type list)
  (container nil :type (or null integer))
  (state nil)
  (ink nil :type list)
  (background nil :type list)
  (partly-unresolved nil)
  (evidence :recorded :type keyword))

(defstruct (snapshot (:copier nil))
  "What one view drew.
VIEW is a keyword naming the view; ELEMENTS is a list of elements in painting order."
  (view nil :type (or null keyword))
  (elements '() :type list))

(defstruct (finding (:copier nil))
  "One disagreement between a spec and a snapshot, as plain data.
CHECK is a keyword naming the check that made it.
SEVERITY is :ERROR, which fails, or :WARNING, which reports without failing.
SPEC-ID is the spec element's id, or NIL.
VIEW, CONTAINER, ELEMENT-ID, and BOX say where.
EXPECTED and MEASURED say why.
EVIDENCE lists what the finding rests on: :RECORDED, :SHOWN, or both.
DETAIL is a plist of check-specific facts."
  (check nil :type (or null keyword))
  (severity :error :type (member :error :warning))
  (spec-id nil)
  (view nil :type (or null keyword))
  (container nil :type (or null integer))
  (element-id nil :type (or null integer))
  (box nil :type list)
  (expected nil)
  (measured nil)
  (evidence (list :recorded) :type list)
  (detail '() :type list))

(defun failing-findings (findings)
  "Return the findings in FINDINGS whose severity is :ERROR, in their original order."
  (remove-if-not (lambda (finding) (eq (finding-severity finding) :error)) findings))

(defun element-to-plist (element)
  "Return ELEMENT as a plist with one keyword key per slot, values as stored.
Keys, in order: :ID :KIND :TEXT :BOX :CONTAINER :STATE :INK :BACKGROUND :PARTLY-UNRESOLVED
:EVIDENCE."
  (list :id (element-id element)
        :kind (element-kind element)
        :text (element-text element)
        :box (element-box element)
        :container (element-container element)
        :state (element-state element)
        :ink (element-ink element)
        :background (element-background element)
        :partly-unresolved (element-partly-unresolved element)
        :evidence (element-evidence element)))

(defun plist-to-element (plist)
  "Return the element PLIST describes, as ELEMENT-TO-PLIST writes it.
An absent key takes the slot's default; an unknown key signals an error."
  (apply #'make-element plist))

(defun finding-to-plist (finding)
  "Return FINDING as a plist with one keyword key per slot, values as stored.
Keys, in order: :CHECK :SEVERITY :SPEC-ID :VIEW :CONTAINER :ELEMENT-ID :BOX :EXPECTED :MEASURED
:EVIDENCE :DETAIL."
  (list :check (finding-check finding)
        :severity (finding-severity finding)
        :spec-id (finding-spec-id finding)
        :view (finding-view finding)
        :container (finding-container finding)
        :element-id (finding-element-id finding)
        :box (finding-box finding)
        :expected (finding-expected finding)
        :measured (finding-measured finding)
        :evidence (finding-evidence finding)
        :detail (finding-detail finding)))

(defun plist-to-finding (plist)
  "Return the finding PLIST describes, as FINDING-TO-PLIST writes it.
An absent key takes the slot's default; an unknown key signals an error."
  (apply #'make-finding plist))

(defun finding-where (finding)
  "Return where FINDING is, as the plist (:VIEW view :CONTAINER id :ELEMENT-ID id :BOX box)."
  (list :view (finding-view finding)
        :container (finding-container finding)
        :element-id (finding-element-id finding)
        :box (finding-box finding)))

(defun finding-why (finding)
  "Return why FINDING was made, as the plist
(:CHECK check :EXPECTED expected :MEASURED measured :EVIDENCE evidence)."
  (list :check (finding-check finding)
        :expected (finding-expected finding)
        :measured (finding-measured finding)
        :evidence (finding-evidence finding)))
