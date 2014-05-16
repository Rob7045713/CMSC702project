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
     (match-let* ([(list view-tab-name view-col-name) (string->paths view)]
                  [(list (list constraint-tab-names constraint-col-names) ...) (map string->paths paths)]
                  [view-tab (table-by-name dtb view-tab-name)]
                  [constraint-tabs (for/list ([n constraint-tab-names]) (table-by-name dtb n))]
                  [view-col↓ (dtb-name->idx dtb view-tab-name view-col-name)]
                  [op↓ (string->op op)]
                  [constraint-ops↓ (map string->op constraint-ops)]
                  [constraint-cols↓ (for/list ([tab-name constraint-tab-names]
                                               [col-name constraint-col-names])
                                      (dtb-name->idx dtb tab-name col-name))]
                  [constraint-types (for/list ([tab constraint-tabs] [col↓ constraint-cols↓])
                                      (third (list-ref (table-desc-attributes (first tab)) col↓)))]
                  [values↓ (for/list ([v values] [t constraint-types])
                             ((type->converter t) v))])
       (when (verbose?)
         (printf "Query: ~a(~a) where " op view)
         (printf "~a~n" (string-join (for/list ([path paths] [constraint-op constraint-ops] [value values])
                                       (format "(~a ~a ~a)" path constraint-op value))
                                     " and ")))
       (unless (and (integer? view-col↓) (andmap integer? constraint-cols↓))
         (error "Unknown path(s), check for typos" (list* view paths)))
       (cond
         [(for/and ([name constraint-tab-names]) (equal? view-tab-name name))
          (apply
           op↓
           (for/list ([obj (in-hash-values (second view-tab))]
                      #:when (for/and ([constraint-op↓ constraint-ops↓]
                                       [constraint-col↓ constraint-cols↓]
                                       [value↓ values↓])
                               (constraint-op↓ (list-ref obj constraint-col↓) value↓)))
             (list-ref obj view-col↓)))]
         ; adhoc special case
         [(for/and ([tab constraint-tabs])
            (list⊆ (table-desc-primary-ids (first tab))
                   (table-desc-primary-ids (first view-tab))))
          (apply
           op↓
           (for/list ([(obj-key obj) (in-hash (second view-tab))]
                      #:when
                      (for/and ([constraint-tab constraint-tabs]
                                [constraint-tab-name constraint-tab-names]
                                [constraint-col↓ constraint-cols↓]
                                [constraint-op↓ constraint-ops↓]
                                [value↓ values↓])
                        (constraint-op↓
                         (cond
                           [(equal? view-tab-name constraint-tab-name)
                            (list-ref obj constraint-col↓)]
                           [else
                            (let* ([constraint-ids 
                                    (for/list ([id (table-desc-primary-ids (first view-tab))]
                                               [v obj-key]
                                               #:when (member id (table-desc-primary-ids (first constraint-tab))))
                                      v)]
                                   [target (hash-ref (second constraint-tab) constraint-ids)])
                              (list-ref target constraint-col↓))])
                         value↓)))
             (list-ref obj view-col↓)))]
         [else (error "Query not supported yet" xexpr)]))]
    [q (error "Query not supported yet" q)]))

;; Table (Listof Int) -> Table
(define (proj tb cols)
  (match-let* ([(list desc rows) tb]
               [(table-desc name primary-ids attributes references collections) desc])
    (list
     (table-desc name
                 primary-ids
                 (take-indices attributes cols)
                 (take-indices references cols)
                 (take-indices collections cols))
     (for/hash ([(ids fields) (in-hash rows)])
       (values ids (take-indices fields cols))))))

;; Table (Listof (Op × Int × Val)) -> Table
(define (sel tb constraints↓)
  (match-let ([(list desc rows) tb])
    (list
     desc
     (for/hash ([(ids fields) (in-hash rows)]
                #:when (for/and ([constraint↓ constraints↓])
                         (match-let ([(list op↓ idx↓ val↓) constraint↓])
                           (op↓ (list-ref fields idx↓) val↓))))
       (values ids fields)))))

;; Dtb String String -> Int
;; return column index (counting from 0) for given column name in dtb name
(define (dtb-name->idx dtb tab-name col-name)
  (let ([tab (table-by-name dtb tab-name)])
    (for/first ([attr (table-desc-attributes (first tab))]
                [i (in-naturals)]
                #:when (equal? col-name (second attr)))
      i)))