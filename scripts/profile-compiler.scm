;; mzscheme -f <this-file>
(require (lib "errortrace.ss" "errortrace"))
(instrumenting-enabled #t)
(profiling-enabled #t)
(profiling-record-enabled #t)
;;(profile-paths-enabled #t)

(require "compiler-command-line.scm")
(the-line)

(output-profile-results #f #f)
(exit)