#lang racket/base
;; this module uses Rob's XML to load data
(provide (all-defined-out))
(require racket/list racket/match racket/string racket/sequence racket/function
         "utils.rkt" "xexpr.rkt" "data.rkt" "rob.rkt")

;; (Listof TableDesc) (Listof (Listof Input-Ports)) -> Dtb
(define (load-dtbs descs in-lists)
  (apply append (for/list ([desc descs] [ins in-lists])
                  (load-dtb desc ins))))

;; XExpr Input-Port -> (Listof (TableDesc × (Map (Listof Id) (Listof Val))))
;; load database from given description and input stream
(define (load-dtb format-desc ins)
  (match format-desc
    [(<> format (@: [ext name] [delimited "true"] [delimiter d]) body ...)
     (match-let* ([class-descs (map xexpr->table-desc (filter-tag 'class body))]
                  [tables (for/list ([_ class-descs]) (make-hash))]
                  [delim (escape d)])
       ;; first pass reading everything in
       (for ([in (in-list ins)])
         (let* ([lines (in-lines in)]
                [header (string-split (sequence-ref lines 0) delim)]
                [col->idx (map->fun (for/hash ([col header] [i (in-naturals)])
                                      (values col i)))]
                [update!s (for/list ([desc class-descs]) (make-updater desc col->idx))])
           (for ([l lines])
             (let ([fields (string-split l delim)])
               (for ([update! update!s] [tab tables])
                 (update! tab fields))))))
       (when (verbose?)
         (printf "Done loading. Objects created:~n")
         (for ([t tables] [c class-descs])
           (printf "-- ~a ~a(s)~n" (hash-count t) (table-desc-name c))))
       ;; TODO second pass resolving references
       (map list class-descs tables))]
    [desc (error "BS format description" desc)]))

;; TableDesc (Id -> Int) -> (Table (Listof Val) -> Void)
(define (make-updater desc label->idx)
  ;; TODO references and collecions
  (let* ([a-labels (map first (table-desc-attributes desc))]
         [a-names (map second (table-desc-attributes desc))]
         [a-converters (map (∘ type->converter third) (table-desc-attributes desc))]
         [id-names (table-desc-primary-ids desc)])
    (λ (table fields)
      (let* ([fields↓ (for/list ([l a-labels] [convert a-converters])
                        (convert (list-ref fields (label->idx l))))]
             [id-fields↓ (for/list ([v fields↓] [n a-names] #:when (member n id-names)) v)])
        (unless (hash-has-key? table id-fields↓)
          (hash-set! table id-fields↓ fields↓))))))

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

;; Table Table -> Table


;; XExpr -> TableDesc
(define (xexpr->table-desc xexpr)
  (match xexpr
    [(<> class (@: [name name]) body ...)
     (let ([prim-ids (string-split (rob/class->primary-id xexpr) ",")]
           [attributes (for/list ([x (filter-tag 'attribute body)])
                         (lookup/list (xexpr->attributes x) '(label name type)))]
           [references (for/list ([x (filter-tag 'reference body)])
                         (lookup/list (xexpr->attributes x) '(label name type)))]
           [collections (for/list ([x (filter-tag 'collection body)])
                          (lookup/list (xexpr->attributes x) '(name type)))])
       (table-desc name prim-ids attributes references collections))]
    [desc (error "BS class description" desc)]))