#lang racket/base
(require racket/cmdline "xexpr.rkt" "rob.rkt")

(command-line
 #:args sources
 (for ([fn sources])
   (display-xexpr (rob->intermine (read-xexpr fn))
                  (string-append fn ".out"))))
