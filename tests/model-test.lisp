;;;; tests/model-test.lisp
;;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;;;
;;;; Elements and findings as plain data: plist round trips, WHERE and WHY, and severity filtering.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((pkg (find-package '#:ekphrasis/tests/model-test)))
    (when pkg
      (sb-ext:without-package-locks (delete-package pkg)))))

(defpackage #:ekphrasis/tests/model-test
  (:use #:cl #:zebra)
  (:import-from #:ekphrasis
                #:element-evidence
                #:element-to-plist
                #:failing-findings
                #:finding-evidence
                #:finding-to-plist
                #:finding-where
                #:finding-why
                #:make-element
                #:make-finding
                #:plist-to-element
                #:plist-to-finding))

(in-package #:ekphrasis/tests/model-test)

(defun full-element ()
  "Return an element with every slot set to a non-default value."
  (make-element :id 3
                :kind :push-button
                :text "Reconnect"
                :box (list 300 8 360 30)
                :container 2
                :state :enabled
                :ink (list 1.0 1.0 1.0)
                :background (list 0.2 0.2 0.2)
                :evidence :shown))

(defun full-finding ()
  "Return a finding with every slot set to a non-default value."
  (make-finding :check :contrast
                :severity :warning
                :spec-id :reconnect
                :view :header
                :container 2
                :element-id 3
                :box (list 300 8 360 30)
                :expected 4.5
                :measured 2.1
                :evidence (list :recorded :shown)
                :detail (list :ratio 2.1 :threshold 4.5)))

(defun plist-keys (plist)
  "Return the keys of PLIST in order."
  (loop for key in plist by #'cddr collect key))

(define-test element-plist-round-trip
  "An element written as a plist and read back is equalp to the original."
  (let* ((element (full-element))
         (plist (element-to-plist element)))
    (true (equalp element (plist-to-element plist)) "~S" plist)))

(define-test element-carries-every-field
  "An element's plist has one key per field, and a fresh element's evidence is :recorded."
  (is equal '(:id :kind :text :box :container :state :ink :background :evidence)
      (plist-keys (element-to-plist (full-element))))
  (is eq :recorded (element-evidence (make-element)))
  (is eq :recorded (getf (element-to-plist (make-element)) :evidence)))

(define-test finding-plist-round-trip
  "A finding written as a plist and read back is equalp to the original."
  (let* ((finding (full-finding))
         (plist (finding-to-plist finding)))
    (true (equalp finding (plist-to-finding plist)) "~S" plist)
    (is equal '(:check :severity :spec-id :view :container :element-id :box
                :expected :measured :evidence :detail)
        (plist-keys plist))
    (is equal '(:recorded) (finding-evidence (make-finding)))
    (true (equalp (make-finding) (plist-to-finding (finding-to-plist (make-finding)))))))

(define-test finding-names-where-and-why
  "FINDING-WHERE gives view, container, element id, and box; FINDING-WHY gives check, expected,
measured, and evidence."
  (let ((finding (full-finding)))
    (is equal '(:view :header :container 2 :element-id 3 :box (300 8 360 30))
        (finding-where finding))
    (is equal '(:check :contrast :expected 4.5 :measured 2.1 :evidence (:recorded :shown))
        (finding-why finding))))

(define-test failing-findings-keeps-errors-only
  "Of one :error and one :warning finding, only the :error one fails."
  (let ((error-finding (make-finding :check :missing :severity :error :spec-id :title))
        (warning-finding (make-finding :check :unexpected :severity :warning)))
    (is equal (list error-finding)
        (failing-findings (list error-finding warning-finding)))
    (false (failing-findings (list warning-finding)))))
