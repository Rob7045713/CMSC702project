#lang racket/base
;; this module uses Rob's XML to load data
(provide (all-defined-out))
(require racket/list racket/match racket/string racket/sequence racket/function
         "utils.rkt" "xexpr.rkt" "rob.rkt")

(struct table-desc (name primary-ids attributes references collections) #:transparent)

(define verbose? (make-parameter #f))

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

;; XExpr Input-Port -> (Listof (TableDesc × (Map (Listof Id) (Listof Val))))
;; load database from given description and input stream
(define (load-dtb format-desc ins)
  (match format-desc
    [(<> format (@: [ext name] [delimited "true"] [delimiter d]) body ...)
     (match-let* ([class-descs
                   (map xexpr->table-desc (filter-tag 'class body))]
                  [tables (for/list ([_ class-descs]) (make-hash))]
                  [delim (escape d)])
       ;; first pass reading everything in
       (for ([in (in-list ins)])
         (let* ([lines (in-lines in)]
                [line0 (sequence-ref lines 0)]
                [header (string-split line0 delim)]
                [col->idx (map->fun (for/hash ([col header] [i (in-naturals)])
                                      (values col i)))]
                [update!s (for/list ([desc class-descs]) (make-updater desc col->idx))])
           (for ([l (sequence-tail lines 1)])
             (let ([fields (string-split l delim)])
               (for ([update! update!s] [tab tables])
                 (update! tab fields))))))
       ;; TODO second pass resolving references
       (when (verbose?)
         (printf "Done loading. Objects created:~n")
         (for ([t tables] [c class-descs])
           (printf "-- ~a ~a(s)~n" (hash-count t) (table-desc-name c))))
       (map list class-descs tables))]
    [desc (error "BS format description" desc)]))

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

;; Dtb String String -> Int
;; return column index (counting from 0) for given column name in dtb name
(define (dtb-name->idx dtb tab-name col-name)
  (let ([tab (table-by-name dtb tab-name)])
    (for/first ([attr (table-desc-attributes (first tab))]
                [i (in-naturals)]
                #:when (equal? col-name (second attr)))
      i)))

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

;; (Listof (TableDesc Table)) String -> Table
(define (table-by-name dtb name)
  (for*/first ([tb dtb] #:when (equal? name (in-value (table-desc-name (first tb)))))
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

;; String -> (List String String)
(define (string->path s) (string-split s "."))