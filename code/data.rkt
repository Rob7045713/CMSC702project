#lang racket/base
;; this module provides internal data representation
(provide (all-defined-out))
(require racket/list racket/match racket/function)

(struct table-desc (name primary-ids attributes references collections) #:transparent)

;; (Listof (TableDesc Table)) String -> Table
(define (table-by-name dtb name)
  (for*/first ([tb dtb] #:when (equal? name (table-desc-name (first tb))))
    tb))

;; convert string to other data
(define (type->converter t)
  (match t
    [(or "float" "int" "integer" "real" "number") string->number]
    #;[(or "bool" "boolean") ; FIXME use match + insensitive regexp
       (λ (s)
         (cond [(or (string-ci=? s "yes") (string-ci=? s "true")) #t]
               [(or (string-ci=? s "no") (string-ci=? s "false")) #f]
               [else (error "Don't know how to convert to boolean" s)]))]
    [_ identity]))

;; String -> (Any * -> Any)
(define string->op ; TODO this assumes correct args
  (match-lambda
    ["=" (match-lambda*
           [(list (? string? s) ...) (apply string-ci=? s)]
           [(cons x xs) (for/and ([z xs]) (equal? x z))])]
    ["<" <] [">" >] ["<=" <=] [">=" >=]
    ["sum" +] ["prod" *]
    ["mean" (λ xs (/ (apply + xs) (length xs)))]
    ["max" max] ["min" min]
    [x (error "Unknown operation" x)]))