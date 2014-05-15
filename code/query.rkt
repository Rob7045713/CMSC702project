#lang racket/base
(require racket/cmdline "xexpr.rkt" "load.rkt")

(command-line
 #:args (desc.xml query.xml . data)
 (printf
  "Query result: ~a~n"
  (query (load-dtb (read-xexpr (open-input-file desc.xml))
                   (map open-input-file data))
         (read-xexpr (open-input-file query.xml)))))