#lang racket
;; this module uses Rob's XML to load data
(provide (all-defined-out))
(require "utils.rkt" "xexpr.rkt" "rob.rkt")

(struct table-desc (name primary-ids attributes references collections) #:transparent)

(define (dtb-name->idx dtb tab-name col-name)
  (let ([tab (table-by-name dtb tab-name)])
    (for/first ([attr (table-desc-attributes (first tab))]
                [i (in-naturals)]
                #:when (equal? col-name (second attr)))
      i)))

;; XExpr Input-Port -> (Listof (TableDesc × (Map (Listof Id) (Listof Val))))
;; load database from given description and input stream
(define (load-dtb format-desc ins)
  (match format-desc
    [(<> format (@: [ext name] [delimited "true"] [delimiter d]) body ...)
     (match-let*-values ([(class-descs)
                          (map xexpr->table-desc (filter-tag 'class body))]
                         [((list main-desc) aux-descs)
                          (partition (match-lambda
                                       [(table-desc n _ _ _ _) (equal? n name)])
                                     class-descs)]
                         [(main-table) (load-main-table main-desc ins (escape d))]
                         [(aux-tables)
                          (for/list ([aux-desc aux-descs])
                            (make-aux-table main-table main-desc aux-desc))])
       (resolve-references (cons main-table aux-tables) (cons main-desc aux-descs)))]
    [desc (error "BS format description" desc)]))

;; XExpr -> TableDesc
(define (xexpr->table-desc xexpr)
  ;; retrieves 'label' attribute of all given XExpr with given tag
  (define (project xexprs tag key)
    (for/list ([x (filter-tag tag xexprs)])
      (lookup (xexpr->attributes x) key)))
  
  (match xexpr
    [(<> class (@: [name name]) body ...)
     (let ([prim-ids (string-split (rob/class->primary-id xexpr) ",")]
           [attribute-labels (project body 'attribute 'label)]
           [attribute-names (project body 'attribute 'name)]
           [attribute-types (project body 'attribute 'type)]
           [reference-labels (project body 'reference 'label)]
           [reference-names (project body 'reference 'name)]
           [reference-types (project body 'reference 'type)]
           [collection-names (project body 'collection 'name)]
           [collection-types (project body 'collection 'type)])
       (table-desc name
                   prim-ids
                   (map list attribute-labels attribute-names attribute-types)
                   (map list reference-labels reference-names reference-types)
                   (map list collection-names collection-types)))]
    [desc (error "BS class description" desc)]))

;; TableDesc InputPort -> Table
;; load main-table from given input stream
(define (load-main-table desc ins [delim "\t"])
  (let* ([a-labels (map first (table-desc-attributes desc))]
         [a-names (map second (table-desc-attributes desc))]
         [a-converters (map (compose type->converter third) (table-desc-attributes desc))]
         [id-names (table-desc-primary-ids desc)])
    
    (for/hash ([l (apply sequence-append (for/list ([in ins]) (sequence-tail in 1)))])
      (let* ([fields (for/list ([s (string-split l delim)] [convert a-converters])
                       (convert s))]
             [id-fields (for/list ([v fields] [n a-names] #:when (member n id-names)) v)])
        ;; TODO do references too or just lookup?
        (values id-fields fields)))))

;; Table TableDesc TableDesc -> Table
;; make auxiliary table from main table and descriptions
(define (make-aux-table main main-desc aux-desc)
  (let* ([a-labels/aux (map first (table-desc-attributes aux-desc))]
         [a-labels/main (map second (table-desc-attributes main-desc))]
         [a-indices/aux (for/list ([l a-labels/aux]) (index-of l a-labels/main))]
         [c-names/aux (map first (table-desc-collections aux-desc))]
         [c-types/aux (map second (table-desc-collections aux-desc))]
         [name->label (λ (name)
                        (or (for/first ([attr (table-desc-attributes aux-desc)]
                                        #:when (equal? (second attr) name))
                              (first attr))
                            (error "No name for label" name)))]
         [primary-indices/aux (for/list ([n (table-desc-primary-ids aux-desc)])
                                (index-of (name->label n) a-labels/main))])
    (for/fold ([aux (hash)]) ([(main-ids main-fields) (in-hash main)])
      (let* ([aux-fields (append (take-indices main-fields a-indices/aux)
                                 (for/list ([c c-names/aux] [t c-types/aux]) (box t)))]
             [aux-ids (take-indices main-fields primary-indices/aux)])
        (match (hash-ref aux aux-ids #f)
          [#f (hash-set aux aux-ids aux-fields)]
          [_ aux])))))

;; (Listof Table) (Listof TableDesc) -> (Listof Table)
;; resolve references between objects
(define (resolve-references tabs descs)
  (map list descs tabs) ; TODO
  #;(match-let ([(cons main-tab aux-tabs) tabs]
                [(cons main-desc aux-descs) descs])
      (for ([reference (table-desc-references main-desc)])
        (match-let ([(list ref-label ref-name ref-type) reference])
          (for ([(main-ids main-fields) (in-hash main-tab)])
            )))))

;; (Listof (TableDesc Table)) String -> Table
(define (table-by-name dtb name)
  (for*/first ([tb dtb]
               [tb-name (in-value (table-desc-name (first tb)))]
               #:when (equal? name tb-name))
    tb))

;; convert string to other data
(define (type->converter t)
  (match t
    [(or "float" "int" "integer" "real" "number") string->number]
    [(or "bool" "boolean") (match-lambda ["true" #t] ["false" #f])]
    [_ identity]))

;; String -> (List String String)
(define (string->path s) (string-split s "."))

;; String -> (Any * -> Any)
(define string->op ; TODO this assumes correct args
  (match-lambda
    ["=" equal?] ["<" <] [">" >] ["<=" <=] [">=" >=]
    ["sum" +] ["prod" *]
    ["mean" (λ xs (/ (apply + xs) (length xs)))]
    ["max" max] ["min" min]
    [x (error "Unknown operation" x)]))

;; Dtb Xexpr -> Any
(define (query dtb xexpr)
  (match xexpr
    [(<> operation (@: [name op])
         (<> param _ ... (<> query (@: [view view])
                             (<> constraint (@: [path path] [op constraint-op] [value value])))))
     (match-let ([(list view-tab view-col) (string->path view)]
                 [(list constraint-tab constraint-col) (string->path path)])
       (cond
         [(equal? view-tab constraint-tab)
          (let* ([tab (table-by-name dtb view-tab)]
                 [view-col↓ (dtb-name->idx dtb view-tab view-col)]
                 [constraint-col↓ (dtb-name->idx dtb constraint-tab constraint-col)]
                 [op↓ (string->op op)]
                 [constraint-op↓ (string->op constraint-op)]
                 [constraint-type (third (list-ref (table-desc-attributes (first tab)) constraint-col↓))]
                 [value↓ ((type->converter constraint-type) value)])
            (cond
              [(and (integer? view-col↓) (integer? constraint-col↓))
               (apply
                op↓
                (for/list ([obj (in-hash-values (second tab))]
                           #:when (constraint-op↓ (list-ref obj constraint-col↓) value↓))
                  (list-ref obj view-col↓)))]
              [else (error "Unknown path(s), check for typos" (list view path))]))]
         [else (error "Query enot supported yet" xexpr)]))]
    [q (error "Query not supported yet" q)]))

(define desc (read-xexpr (open-input-file "../file_formats/exon_expr.xml")))

;;;;; tests
(define Exon (match-let ([(<> format _ body ...) desc])
               (xexpr->table-desc (first (filter-tag 'class body)))))
(define Experiment (match-let ([(<> format _ body ...) desc])
                     (xexpr->table-desc (second (filter-tag 'class body)))))
(define exon_expr (match-let ([(<> format _ body ...) desc])
                    (xexpr->table-desc (third (filter-tag 'class body)))))
(define query-mean `(operation ([name "mean"])
                      (param
                       (query ([view "exon_expr.raw_counts"])
                         (constraint ([path "exon_expr.barcode"] [op "="] [value "experiment1"]))))))
(require racket/trace)
#;(trace dtb-name->idx)