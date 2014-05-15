#lang racket
;; this module uses Rob's XML to load data
(provide (all-defined-out))
(require "utils.rkt" "xexpr.rkt" "rob.rkt")

;; Dtb       ::= Listof (Id × Table)
;; Table     ::= Map Id Obj
;; Obj       ::= Listof (Id × Val)
;; (associative lists are usually faster that hash tables for <20 elements)

(struct table-desc (name primary-ids attributes references collections) #:transparent)

(define (load-dtb format-desc in)
  (match format-desc
    [(<> format (@: [ext name] [delimited "true"] [delimiter d]) body ...)
     (let*-values ([(class-descs)
                    (map xexpr->table-desc (filter-tag 'class body))]
                   [(main-desc aux-descs)
                    (partition (match-lambda
                                 [(table-desc n _ _ _ _) (equal? n name)])
                               class-descs)]
                   [(main-table)
                    (list (first main-desc)
                          (load-main-table (first main-desc) in (escape d)))]
                   [(aux-tables)
                    (for/list ([aux-desc aux-descs])
                      (list aux-desc (make-aux-table (second main-table) (first main-table) aux-desc)))])
       (cons main-table aux-tables))]
    [desc (error "BS format description" desc)]))

;; String -> String
;; escape special characters. FIXME: there must be a standard way
(define (escape s)
  (string-replace s "\\t" "\t"))

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
(define (load-main-table desc in [delim "\t"])
  (let* ([a-labels (map first (table-desc-attributes desc))]
         [a-names (map second (table-desc-attributes desc))]
         [a-converters (map (compose type->converter third) (table-desc-attributes desc))]
         [id-names (table-desc-primary-ids desc)])
    (for/hash ([l (sequence-tail (in-lines in) 1)])
      (let* ([fields (for/list ([s (string-split l delim)] [convert a-converters])
                       (convert s))]
             [id-fields (for/list ([v fields] [n a-names] #:when (member n id-names)) v)])
        (values id-fields fields)))))

;; Table TableDesc TableDesc -> Table
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

;; convert string to other data
(define (type->converter t)
  (match t
    [(or "float" "int" "integer" "real" "number") string->number]
    [(or "bool" "boolean") (match-lambda ["true" #t] ["false" #f])]
    [_ identity]))

(define desc (read-xexpr (open-input-file "../file_formats/exon_expr.xml")))
(define Exon (match-let ([(<> format _ body ...) desc])
               (xexpr->table-desc (first (filter-tag 'class body)))))
(define Experiment (match-let ([(<> format _ body ...) desc])
                     (xexpr->table-desc (second (filter-tag 'class body)))))
(define exon_expr (match-let ([(<> format _ body ...) desc])
                    (xexpr->table-desc (third (filter-tag 'class body)))))