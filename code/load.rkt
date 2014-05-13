#lang racket
;; this module uses Rob's XML to load data
(provide (all-defined-out))
(require "xexpr.rkt" "rob.rkt")

;; Dtb       ::= Listof (Id × Table)
;; Table     ::= Map Id Obj
;; Obj       ::= Listof (Id × Val)
;; Pre-Dtb   ::= Listof (Id × Pre-Table)
;; Pre-Table ::= Map Id Pre-Obj
;; Pre-Obj   ::= Obj × (Listof (Id × Id))
;; (associative lists are usually faster that hash tables for <20 elements)

;; retrives 'label' attribute of all given XExpr with given tag
(define (project xexprs tag key)
  (for/list ([a (filter-tag tag xexprs)])
    (lookup (second a) key)))

;; convert string to other data
(define (string->data s t)
  ((match t
     [(or "float" "int" "integer" "real" "number") string->number]
     [(or "bool" "boolean") (match-lambda ["true" #t] ["false" #f])]
     [_ identity])
   s))

;; XExpr String? -> (Input-Port -> Dtb)
(define ((mk-class-loader class-desc [delim "\t"]) fn)
  (match class-desc
    [(<> class (@: [name name]) body ...)
     (let ([prim-ids (string-split (rob/class->primary-id class-desc) ",")]
           [attributes (project body 'attribute 'label)]
           [types (project body 'attribute 'type)]
           [references (project body 'reference 'label)]
           [collections (project body 'collection 'label)])
       ;; first pass loading pre-objects
       (for/hash ([l (rest (file->lines fn))])
         (let* ([fields (for/list ([f (string-split l delim)] [t types])
                          (string->data f t))]
                [obj (map list attributes fields)]
                [id (lookup/list obj prim-ids)])
           (values id obj))))]))

(define x ((mk-class-loader `(class ([name exon_expr])
                     (primaryId ([field "barcode,exon"]))
                     (attribute ([label "barcode"] [type "string"]))
                     (attribute ([label "exon"] [type "string"]))
                     (attribute ([label "raw_counts"] [type "float"]))
                     (attribute ([label "median_length_normalized"] [type "float"]))
                     (attribute ([label "RPKM"] [type "float"]))
                     (reference ([label "exon"] [type "Exon"]))
                     (reference ([label "barcode"] [type "Experiment"]))))
 "test.data"))