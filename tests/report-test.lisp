;;;; tests/report-test.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; The report's exact lines: the header count, the missing line, and text printed verbatim.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((pkg (find-package '#:ekphrasis/tests/report-test)))
    (when pkg
      (sb-ext:without-package-locks (delete-package pkg)))))

(defpackage #:ekphrasis/tests/report-test
  (:use #:cl #:zebra)
  (:import-from #:ekphrasis
                #:format-finding
                #:format-report
                #:make-finding))

(in-package #:ekphrasis/tests/report-test)

(defun missing (label &key (spec-id :title))
  "Return the :MISSING error finding for a text element labelled LABEL in the header view."
  (make-finding :check :missing
                :severity :error
                :spec-id spec-id
                :view :header
                :expected label
                :detail (list :kind :text)))

(defun unexpected ()
  "Return a :WARNING finding in the header view."
  (make-finding :check :unexpected
                :severity :warning
                :view :header
                :element-id 2
                :measured "offline"))

(defun header-line (report)
  "Return the first line of REPORT."
  (subseq report 0 (position #\Newline report)))

(define-test missing-finding-reports-one-line
  "One missing finding reports as the header line and the missing line, joined by a newline."
  (is string=
      (format nil "~A~%~A"
              "header: 1 finding (1 error, 0 warnings)"
              "  error missing :title (text) in header: expected \"EKPHRASIS\", none drawn")
      (format-report :header (list (missing "EKPHRASIS")))))

(define-test report-prints-tilde-verbatim
  "A label holding a tilde appears in the report exactly as written."
  (let ((report (format-report :header (list (missing "50~ off")))))
    (true (search "expected \"50~ off\", none drawn" report) "~A" report)))

(define-test report-header-counts-errors-and-warnings
  "The header counts findings, errors, and warnings, pluralised for 0, 1, and 3."
  (let ((three (format-report :header
                              (list (missing "EKPHRASIS")
                                    (missing "ONLINE" :spec-id :status)
                                    (unexpected)))))
    (is string= "header: no findings" (format-report :header '()))
    (is string= "header: 1 finding (1 error, 0 warnings)"
        (header-line (format-report :header (list (missing "EKPHRASIS")))))
    (is string= "header: 3 findings (2 errors, 1 warning)" (header-line three))
    (is = 3 (count #\Newline three))))

(define-test label-and-unexpected-lines
  "A :LABEL error and an :UNEXPECTED warning each report as one line.
The line names the element, its box, and the view, with the container's id when it has one."
  (let ((label (make-finding :check :label
                             :severity :error
                             :spec-id :bravo
                             :view :header
                             :element-id 7
                             :box (list 709.0d0 8.0d0 768.0d0 30.0d0)
                             :expected "bravo"
                             :measured "brovo"
                             :detail (list :kind :session-pill)))
        (unexpected (make-finding :check :unexpected
                                  :severity :warning
                                  :view :header
                                  :element-id 9
                                  :box (list 600 8 664 30)
                                  :measured "delta"
                                  :detail (list :kind :session-pill)))
        (nested (make-finding :check :unexpected
                              :severity :warning
                              :view :header
                              :container 3
                              :element-id 9
                              :box (list 600 8 664 30)
                              :measured "50~ off"
                              :detail (list :kind :session-pill))))
    (is string= (concatenate 'string
                             "  error label :bravo #7 (session-pill) at (709 8 768 30) in header: "
                             "expected \"bravo\", drawn \"brovo\"")
        (format-finding label nil))
    (is string= (concatenate 'string
                             "  warning unexpected #9 (session-pill) at (600 8 664 30) in header: "
                             "drawn \"delta\", not in the spec")
        (format-finding unexpected nil))
    (is string= (concatenate 'string
                             "  warning unexpected #9 (session-pill) at (600 8 664 30) "
                             "in header #3: drawn \"50~ off\", not in the spec")
        (format-finding nested nil))))
