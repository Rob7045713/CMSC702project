#lang racket/base
(require racket/cmdline "xexpr.rkt" "load.rkt")

(command-line
 #:args (desc.xml . data)
 (load-dtb (read-xexpr (open-input-file desc.xml))
           (map open-input-file data)))