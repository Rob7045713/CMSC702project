#lang racket/base
(require racket/cmdline "xexpr.rkt" "rob.rkt")

(command-line
 #:args sources
 
 (for ([fn sources])
   (display-xexpr (rob->intermine (read-xexpr (open-input-file fn)))
                  (open-output-file (string-append fn ".out") #:exists 'replace))))