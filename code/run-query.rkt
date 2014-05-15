#lang racket/base
(require racket/cmdline racket/list racket/string
         "utils.rkt" "xexpr.rkt" "load.rkt" "query.rkt")

(command-line
 #:once-each
 [("-v" "--verbose") "Verbose mode" (verbose? #t)]
 #:args (query.xml description-dir data-dir)
 (let* ([desc-fns (map path->string (directory-list description-dir))]
        [desc-names (for/list ([name desc-fns]) (first (string-split name ".")))]
        [data-fns (map path->string (directory-list data-dir))]
        [data-fns-lists (for/list ([desc-name desc-names])
                          (for/list ([data-fn data-fns]
                                     #:when (regexp-match? (regexp (string-append ".*" desc-name ".*"))
                                                           data-fn))
                            data-fn))])
   (printf
    "Query result: ~a~n"
    (query (load-dtbs (map read-xexpr (for/list ([d desc-fns])
                                        (string-append description-dir "/" d)))
                      (for/list ([data-fns-list data-fns-lists])
                        (for/list ([d data-fns-list])
                          (open-input-file (string-append data-dir "/" d)))))
           (read-xexpr query.xml)))))