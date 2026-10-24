;;;; ekphrasis.asd
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(asdf:defsystem "ekphrasis"
  :class :package-inferred-system
  :description "Check what a user interface actually renders against a written description of what it should show."
  :author "Brian O'Reilly <fade@deepsky.com>"
  :license "AGPL-3.0-or-later"
  :version "0.1.0"
  :depends-on ("ekphrasis/src/main" "ekphrasis/src/ekphrasis")
  :in-order-to ((test-op (test-op "ekphrasis/tests"))))

(asdf:defsystem "ekphrasis/tests"
  :class :package-inferred-system
  :depends-on ("zebra"
               "ekphrasis"
               "ekphrasis/tests/main-test"
               "ekphrasis/tests/model-test"
               "ekphrasis/tests/run-test"
               "ekphrasis/tests/report-test")
  :perform (test-op (o c)
                    (declare (ignore o))
                    (let* ((test-package-names
                            (remove-if-not
                             (lambda (dep)
                               (and (stringp dep)
                                    (uiop:string-prefix-p "ekphrasis/tests/" dep)))
                             (asdf:system-depends-on c)))
                           (any-failed nil))
                      (dolist (name test-package-names)
                        (let* ((pkg (or (find-package (string-upcase name))
                                        (error "Test package ~S not loaded." name)))
                               (result (uiop:symbol-call :zebra :test pkg)))
                          (when (uiop:symbol-call :zebra :results-with-status
                                                  :failed result)
                            (setf any-failed t))))
                      (when any-failed
                        (error "ekphrasis/tests: one or more zebra tests failed.")))))

(asdf:defsystem "ekphrasis/mcclim"
  :class :package-inferred-system
  :description "McCLIM adapter for ekphrasis: render a frame pane headlessly into drawn elements."
  :author "Brian O'Reilly <fade@deepsky.com>"
  :license "AGPL-3.0-or-later"
  :version "0.1.0"
  :depends-on ("ekphrasis"
               "clim"
               "mcclim-null"
               "ekphrasis/src/mcclim/ekphrasis-mcclim")
  :in-order-to ((test-op (test-op "ekphrasis/mcclim/tests"))))

(asdf:defsystem "ekphrasis/mcclim/tests"
  :class :package-inferred-system
  :depends-on ("zebra"
               "ekphrasis/mcclim"
               "ekphrasis/tests/mcclim/acceptance-test"
               "ekphrasis/tests/mcclim/medium-test"
               "ekphrasis/tests/mcclim/records-test"
               "ekphrasis/tests/mcclim/adapter-test")
  :perform (test-op (o c)
                    (declare (ignore o))
                    (let* ((test-package-names
                            (remove-if-not
                             (lambda (dep)
                               (and (stringp dep)
                                    (uiop:string-prefix-p "ekphrasis/tests/mcclim/" dep)))
                             (asdf:system-depends-on c)))
                           (any-failed nil))
                      (dolist (name test-package-names)
                        (let* ((pkg (or (find-package (string-upcase name))
                                        (error "Test package ~S not loaded." name)))
                               (result (uiop:symbol-call :zebra :test pkg)))
                          (when (uiop:symbol-call :zebra :results-with-status
                                                  :failed result)
                            (setf any-failed t))))
                      (when any-failed
                        (error "ekphrasis/mcclim/tests: one or more zebra tests failed.")))))
