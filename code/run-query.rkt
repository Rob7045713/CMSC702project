#lang racket/base
(require racket/cmdline "xexpr.rkt" "load.rkt")

(command-line
 #:once-each
 [("-v" "--verbose") "Verbose mode" (verbose? #t)]
 #:args (desc.xml query.xml . data)
 (printf
  "Query result: ~a~n"
  (query (load-dtb (read-xexpr desc.xml) (map open-input-file data))
         (read-xexpr query.xml))))