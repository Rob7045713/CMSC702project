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
(define (label-at xexprs tag)
  (for/list ([a (filter-tag tag xexprs)])
    (lookup (second a) 'label)))

;; XExpr String? -> (Input-Port -> Dtb)
(define ((mk-class-loader class-desc [delim "\t"]) fn)
  (match class-desc
    [(<> class (@: [name name]) body ...)
     (let ([prim-ids (string-split (rob/class->primary-id class-desc) ",")]
           [attributes (label-at body 'attribute)]
           [references (label-at body 'reference)]
           [collections (label-at body 'collection)])
       ;; first pass loading pre-objects
       (for/hash ([l (rest (file->lines fn))])
         (let* ([fields (string-split l delim)]
                [obj (map list attributes fields)]
                [id (lookup/list obj prim-ids)])
           (values id obj))))]))

(define x ((mk-class-loader `(class ([name exon_expr])
                     (primaryId ([field "barcode,exon"]))
                     (attribute ([label "barcode"]))
                     (attribute ([label "exon"]))
                     (attribute ([label "raw_counts"]))
                     (attribute ([label "median_length_normalized"]))
                     (attribute ([label "RPKM"]))
                     (reference ([label "exon"]))
                     (reference ([label "barcode"]))))
 "test.data"))