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

(define-test contrast-and-unresolved-ink-lines
  "A :CONTRAST error reports its ratio and threshold to two decimals and both colours; an
:UNRESOLVED-INK warning says which colour is not fixed."
  (let ((contrast (make-finding :check :contrast
                                :severity :error
                                :view :header
                                :element-id 12
                                :box (list 299 9 381 41)
                                :expected 3.0
                                :measured 1.662
                                :detail (list :ratio 1.662 :threshold 3.0
                                              :ink "#000000" :background "#333333"
                                              :category :non-text :kind :outline)))
        (unresolved (make-finding :check :unresolved-ink
                                  :severity :warning
                                  :view :header
                                  :element-id 12
                                  :box (list 299 9 381 41)
                                  :detail (list :unresolved (list :ink)
                                                :category :non-text :kind :outline))))
    (is string= (concatenate 'string
                             "  error contrast #12 (outline) at (299 9 381 41) in header: "
                             "1.66 below 3.00 (ink #000000 on #333333)")
        (format-finding contrast nil))
    (is string= (concatenate 'string
                             "  warning unresolved-ink #12 (outline) at (299 9 381 41) in header: "
                             "ink has no fixed colour")
        (format-finding unresolved nil))))

(define-test order-line
  "An :ORDER error reports its axis, the box around its group, and the expected, drawn, and
displaced ids."
  (let ((order (make-finding :check :order
                             :severity :error
                             :view :header
                             :box (list 577 8 882 30)
                             :expected (list :alpha :bravo :charlie)
                             :measured (list :bravo :alpha :charlie)
                             :detail (list :axis :left-to-right :displaced (list :bravo)))))
    (is string= (concatenate 'string
                             "  error order left-to-right at (577 8 882 30) in header: "
                             "expected (alpha bravo charlie), drawn (bravo alpha charlie), "
                             "displaced (bravo)")
        (format-finding order nil))))

(define-test state-lines
  "A :STATE error reports the expected and measured states with the reported and drawn ones; a
:STATE-APPEARANCE error reports the state the client gave and the one the drawing shows.
A missing state reads \"none\"."
  (flet ((stated (check expected measured reported drawn)
           (make-finding :check check
                         :severity :error
                         :spec-id :bravo
                         :view :header
                         :element-id 16
                         :box (list 709 8 773 30)
                         :expected expected
                         :measured measured
                         :detail (list :reported reported :drawn drawn
                                       :ink "#FFFFFF" :background "#5C421A"
                                       :kind :session-pill))))
    (is string= (concatenate 'string
                             "  error state :bravo #16 (session-pill) at (709 8 773 30) "
                             "in header: expected fenced, measured attached "
                             "(reported attached, drawn attached)")
        (format-finding (stated :state :fenced :attached :attached :attached) nil))
    (is string= (concatenate 'string
                             "  error state-appearance :bravo #16 (session-pill) "
                             "at (709 8 773 30) in header: reported fenced, drawn as attached")
        (format-finding (stated :state-appearance :fenced :attached :fenced :attached) nil))
    (is string= (concatenate 'string
                             "  error state :bravo #16 (session-pill) at (709 8 773 30) "
                             "in header: expected one of (attached fenced), measured none "
                             "(reported none, drawn none)")
        (format-finding (stated :state (list :attached :fenced) nil nil nil) nil))))

(define-test highlighted-mark-lines-say-so
  "A :CONTRAST or :UNRESOLVED-INK line ends with the label clause when the detail has :LABEL,
naming :CONTAINER-SPEC-ID when present, then with \", only when highlighted\" when the detail's
:STATE is :HIGHLIGHTED. Any other :STATE prints no clause."
  (flet ((contrast (id container box detail)
           (make-finding :check :contrast
                         :severity :error
                         :view :header
                         :container container
                         :element-id id
                         :box box
                         :expected (getf detail :threshold)
                         :measured (getf detail :ratio)
                         :detail detail)))
    (let* ((white (list :ink "#FFFFFF" :background "#FFFFFF" :category :text :kind :text))
           (frame (contrast 45 2 (list 802 8 882 30)
                            (list :ratio 1.1549d0 :threshold 3.0
                                  :ink "#000000" :background "#14151D"
                                  :category :non-text :kind :outline
                                  :container-spec-id :charlie :state :highlighted)))
           (label (contrast 46 2 (list 814 15 870 28)
                            (append (list :ratio 1.0d0 :threshold 4.5) white
                                    (list :label "charlie" :container-spec-id :charlie
                                          :state :highlighted))))
           (unpaired (contrast 9 3 (list 600 15 656 28)
                               (append (list :ratio 1.0d0 :threshold 4.5) white
                                       (list :label "50~ off" :state :highlighted))))
           (unresolved (make-finding :check :unresolved-ink
                                     :severity :warning
                                     :view :header
                                     :container 2
                                     :element-id 47
                                     :box (list 802 8 882 30)
                                     :detail (list :unresolved (list :ink)
                                                   :category :non-text :kind :outline
                                                   :container-spec-id :charlie
                                                   :state :highlighted)))
           (resting-detail (list :ratio 1.38d0 :threshold 4.5
                                 :ink "#1A1A1A" :background "#333333"
                                 :category :text :kind :text
                                 :label "alpha" :container-spec-id :alpha))
           (resting (contrast 3 2 (list 672 15 712 28) resting-detail))
           (reported (contrast 3 2 (list 672 15 712 28)
                               (append resting-detail (list :state :attached))))
           (resting-line (concatenate 'string
                                      "  error contrast #3 (text) at (672 15 712 28) "
                                      "in header #2: 1.38 below 4.50 (ink #1A1A1A on #333333), "
                                      "label \"alpha\" of :alpha")))
      (is string= (concatenate 'string
                               "  error contrast #45 (outline) at (802 8 882 30) in header #2: "
                               "1.15 below 3.00 (ink #000000 on #14151D), only when highlighted")
          (format-finding frame nil))
      (is string= (concatenate 'string
                               "  error contrast #46 (text) at (814 15 870 28) in header #2: "
                               "1.00 below 4.50 (ink #FFFFFF on #FFFFFF), "
                               "label \"charlie\" of :charlie, only when highlighted")
          (format-finding label nil))
      (is string= (concatenate 'string
                               "  error contrast #9 (text) at (600 15 656 28) in header #3: "
                               "1.00 below 4.50 (ink #FFFFFF on #FFFFFF), "
                               "label \"50~ off\", only when highlighted")
          (format-finding unpaired nil))
      (is string= (concatenate 'string
                               "  warning unresolved-ink #47 (outline) at (802 8 882 30) "
                               "in header #2: ink has no fixed colour, only when highlighted")
          (format-finding unresolved nil))
      (is string= resting-line (format-finding resting nil))
      (is string= resting-line (format-finding reported nil)))))
