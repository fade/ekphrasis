;;;; src/report.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; Findings as a short text report: one header line, then one line per finding.
;;;; Labels and drawn text are always format arguments, never part of a control string.

(defpackage #:ekphrasis/src/report
  (:use #:cl)
  (:import-from #:ekphrasis/src/model
                #:finding-box
                #:finding-check
                #:finding-container
                #:finding-detail
                #:finding-element-id
                #:finding-expected
                #:finding-measured
                #:finding-severity
                #:finding-spec-id
                #:finding-view)
  (:export #:format-report
           #:format-finding))

(in-package #:ekphrasis/src/report)

(defun name-text (designator)
  "Return DESIGNATOR's name in lower case, or \"view\" when it is NIL."
  (if designator
      (string-downcase (string designator))
      "view"))

(defun keyword-text (keyword)
  "Return KEYWORD as it is written, in lower case: :title for :TITLE."
  (format nil ":~A" (name-text keyword)))

(defun call-with-report-stream (stream function)
  "Call FUNCTION with an output stream following FORMAT's destination rules for STREAM.
Return the string written when STREAM is NIL, otherwise NIL."
  (cond ((null stream)
         (with-output-to-string (out)
           (funcall function out)))
        ((eq stream t)
         (funcall function *standard-output*)
         nil)
        (t
         (funcall function stream)
         nil)))

(defun quoted-text (text)
  "Return TEXT printed readably in double quotes, or \"no text\" when TEXT is NIL."
  (if text
      (prin1-to-string text)
      "no text"))

(defun box-text (box)
  "Return \" at (x1 y1 x2 y2)\" for BOX with each coordinate rounded, or \"\" when BOX is NIL."
  (if box
      (format nil " at (~{~D~^ ~})" (mapcar #'round box))
      ""))

(defun where-text (finding)
  "Return the view FINDING is in, followed by \" #id\" of its container when it has one."
  (if (finding-container finding)
      (format nil "~A #~D" (name-text (finding-view finding)) (finding-container finding))
      (name-text (finding-view finding))))

(defun element-reference (finding)
  "Return FINDING's spec id and drawn element id as \":bravo #7\", omitting whichever is NIL."
  (format nil "~{~A~^ ~}"
          (remove nil (list (and (finding-spec-id finding)
                                 (keyword-text (finding-spec-id finding)))
                            (and (finding-element-id finding)
                                 (format nil "#~D" (finding-element-id finding)))))))

(defun names-text (designators)
  "Return DESIGNATORS as a parenthesised list of lower-case names: \"(alpha bravo)\"."
  (format nil "(~{~A~^ ~})" (mapcar #'name-text designators)))

(defun state-text (state)
  "Return STATE in lower case: \"fenced\" for :FENCED, \"none\" for NIL, and \"one of (attached
fenced)\" for a list of allowed states."
  (cond ((null state) "none")
        ((listp state) (format nil "one of ~A" (names-text state)))
        (t (name-text state))))

(defun unresolved-text (unresolved)
  "Return what UNRESOLVED, a list of :INK and :BACKGROUND, says has no fixed colour:
\"ink has no fixed colour\", or \"ink and background have no fixed colour\"."
  (format nil "~{~A~^ and ~} ~:[has~;have~] no fixed colour"
          (mapcar #'name-text unresolved)
          (rest unresolved)))

(defun mark-clauses (detail)
  "Return the clauses a perceivability line ends with for DETAIL, a finding's detail plist:
\", label \" and the :LABEL text quoted, then \" of \" and the :CONTAINER-SPEC-ID when present;
then \", only when highlighted\" when :STATE is the highlight feedback state :HIGHLIGHTED.
Return \"\" when neither clause applies. No other state prints a clause."
  (let ((label (getf detail :label))
        (container-spec-id (getf detail :container-spec-id)))
    (concatenate 'string
                 (if label
                     (format nil ", label ~A~@[ of ~A~]"
                             (quoted-text label)
                             (and container-spec-id (keyword-text container-spec-id)))
                     "")
                 (if (eq :highlighted (getf detail :state))
                     ", only when highlighted"
                     ""))))

(defun write-finding-text (finding out)
  "Write the check-specific text of FINDING to OUT."
  (let ((*print-pretty* nil)
        (*print-escape* t)
        (kind (name-text (getf (finding-detail finding) :kind))))
    (case (finding-check finding)
      (:missing
       (format out "~A (~A) in ~A: expected ~A, none drawn"
               (keyword-text (finding-spec-id finding))
               kind
               (name-text (finding-view finding))
               (quoted-text (finding-expected finding))))
      (:label
       (format out "~A (~A)~A in ~A: expected ~A, drawn ~A"
               (element-reference finding)
               kind
               (box-text (finding-box finding))
               (where-text finding)
               (quoted-text (finding-expected finding))
               (quoted-text (finding-measured finding))))
      (:unexpected
       (format out "~A (~A)~A in ~A: drawn ~A, not in the spec"
               (element-reference finding)
               kind
               (box-text (finding-box finding))
               (where-text finding)
               (quoted-text (finding-measured finding))))
      (:order
       (format out "~A~A in ~A: expected ~A, drawn ~A, displaced ~A"
               (name-text (getf (finding-detail finding) :axis))
               (box-text (finding-box finding))
               (where-text finding)
               (names-text (finding-expected finding))
               (names-text (finding-measured finding))
               (names-text (getf (finding-detail finding) :displaced))))
      (:state
       (format out "~A (~A)~A in ~A: expected ~A, measured ~A (reported ~A, drawn ~A)"
               (element-reference finding)
               kind
               (box-text (finding-box finding))
               (where-text finding)
               (state-text (finding-expected finding))
               (state-text (finding-measured finding))
               (state-text (getf (finding-detail finding) :reported))
               (state-text (getf (finding-detail finding) :drawn))))
      (:state-appearance
       (format out "~A (~A)~A in ~A: reported ~A, drawn as ~A"
               (element-reference finding)
               kind
               (box-text (finding-box finding))
               (where-text finding)
               (state-text (getf (finding-detail finding) :reported))
               (state-text (getf (finding-detail finding) :drawn))))
      (:contrast
       (format out "~A (~A)~A in ~A: ~,2F below ~,2F (ink ~A on ~A)~A"
               (element-reference finding)
               kind
               (box-text (finding-box finding))
               (where-text finding)
               (getf (finding-detail finding) :ratio)
               (getf (finding-detail finding) :threshold)
               (getf (finding-detail finding) :ink)
               (getf (finding-detail finding) :background)
               (mark-clauses (finding-detail finding))))
      (:unresolved-ink
       (format out "~A (~A)~A in ~A: ~A~A"
               (element-reference finding)
               kind
               (box-text (finding-box finding))
               (where-text finding)
               (unresolved-text (getf (finding-detail finding) :unresolved '(:ink)))
               (mark-clauses (finding-detail finding))))
      (t
       (format out "~A in ~A: expected ~S, measured ~S"
               (if (finding-spec-id finding) (keyword-text (finding-spec-id finding)) "element")
               (name-text (finding-view finding))
               (finding-expected finding)
               (finding-measured finding))))))

(defun format-finding (finding &optional stream)
  "Write FINDING as one report line: two spaces, severity, check, then what and where.
STREAM follows FORMAT's destination rules; with NIL, return the line as a string."
  (call-with-report-stream
   stream
   (lambda (out)
     (format out "  ~A ~A "
             (name-text (finding-severity finding))
             (name-text (finding-check finding)))
     (write-finding-text finding out))))

(defun format-report (view findings &optional stream)
  "Write the report for FINDINGS in VIEW: a header line, then one FORMAT-FINDING line per finding.
STREAM follows FORMAT's destination rules; with NIL, return the report as a string."
  (let* ((errors (count :error findings :key #'finding-severity))
         (warnings (count :warning findings :key #'finding-severity))
         (total (length findings)))
    (call-with-report-stream
     stream
     (lambda (out)
       (if (zerop total)
           (format out "~A: no findings" (name-text view))
           (format out "~A: ~D finding~:P (~D error~:P, ~D warning~:P)"
                   (name-text view) total errors warnings))
       (dolist (finding findings)
         (terpri out)
         (format-finding finding out))))))
