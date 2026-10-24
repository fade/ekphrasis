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
