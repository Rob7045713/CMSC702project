#lang racket/base

;; this module provides tools for querying the database
(provide (all-defined-out))

(require racket/match racket/list racket/string "utils.rkt" "xexpr.rkt" "data.rkt")

;; String -> (List String String)
(define (string->paths s) (string-split s "."))

;; Dtb Xexpr -> Any
(define (query dtb xexpr)
  (match xexpr
    [(<> operation (@: [name op])
         (<> param _ ... (<> query (@: [view view])
                             (<> constraint (@: [path paths] [op constraint-ops] [value values]))
                             ...)))
     (match-let ([(list view-tab view-col) (string->paths view)]
                 [(list (list constraint-tabs constraint-cols) ...) (map string->paths paths)])
       (cond
         [(for/and ([tab constraint-tabs]) (equal? view-tab tab))
          (let* ([tab (table-by-name dtb view-tab)]
                 [view-col↓ (dtb-name->idx dtb view-tab view-col)]
                 [constraint-cols↓ (for/list ([constraint-tab constraint-tabs] [constraint-col constraint-cols])
                                     (dtb-name->idx dtb constraint-tab constraint-col))]
                 [op↓ (string->op op)]
                 [constraint-ops↓ (map string->op constraint-ops)]
                 [constraint-types (for/list ([constraint-col↓ constraint-cols↓])
                                     (third (list-ref (table-desc-attributes (first tab)) constraint-col↓)))]
                 [values↓ (for/list ([v values] [t constraint-types])
                            ((type->converter t) v))])
            (cond
              [(and (integer? view-col↓) (andmap integer? constraint-cols↓))
               (when (verbose?)
                 (printf "Query: ~a(~a) where " op view)
                 (printf "~a~n" (string-join (for/list ([path paths] [constraint-op constraint-ops] [value values])
                                               (format "(~a ~a ~a)" path constraint-op value))
                                             " and ")))
               (apply
                op↓
                (for/list ([obj (in-hash-values (second tab))]
                           #:when (for/and ([constraint-op↓ constraint-ops↓]
                                            [constraint-col↓ constraint-cols↓]
                                            [value↓ values↓])
                                    (constraint-op↓ (list-ref obj constraint-col↓) value↓)))
                  (list-ref obj view-col↓)))]
              [else (error "Unknown path(s), check for typos" (list* view paths))]))]
         [else (error "Query enot supported yet" xexpr)]))]
    [q (error "Query not supported yet" q)]))

;; Dtb String String -> Int
;; return column index (counting from 0) for given column name in dtb name
(define (dtb-name->idx dtb tab-name col-name)
  (let ([tab (table-by-name dtb tab-name)])
    (for/first ([attr (table-desc-attributes (first tab))]
                [i (in-naturals)]
                #:when (equal? col-name (second attr)))
      i)))