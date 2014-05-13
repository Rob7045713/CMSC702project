#lang racket
;; this module provides translation from Intermine's XML to Rob's XML
(provide (all-defined-out))
(require "xexpr.rkt" "intermine.rkt")

;;; extract data from Rob's <class>
(define rob/class->primary-id ; <class> -> String
  (match-lambda
    [(<> class _ ... (<> primaryId (@: [field s])) _ ...) s]))
(define rob/class->attributes ; <class> -> (List String String)
  (match-lambda
    [(<> class body ...) (for/list ([b (filter-tag 'attribute body)])
                           (lookup* (second b) 'name 'type))]))
(define (rob/class->links T) ; tag -> (<class> -> (List String String String?))
  (match-lambda
    [(<> class body ...) (for/list ([b (filter-tag T body)])
                           (lookup* (second b) 'name 'type))]))
(define rob/class->collections (rob/class->links 'collection))
(define rob/class->references (rob/class->links 'reference))
(define (rob/class->name c) ; <class> -> String
  (lookup (second c) 'name))

;; translate Rob's XML to Intermine's XML
(define rob->intermine
  (match-lambda
    [(<> format (@: [ext ext]) body ...)
     (model ext "FIXME:package"
            (for/list ([b (filter-tag 'class body)])
              (class (rob/class->name b)
                (map (curry apply attribute) (rob/class->attributes b))
                (map (curry apply reference) (rob/class->references b))
                (map (curry apply collection) (rob/class->collections b)))))]))

;;;;; test data
#;(define rob (read-xexpr (open-input-file "../file_formats/jnct_expr.xml")))
#;(display-xexpr (rob->intermine rob))