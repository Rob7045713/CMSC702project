#lang racket
(require (only-in xml write-xexpr xexpr? display-xml/content xexpr->xml empty-tag-shorthand
                  xml->xexpr read-xml/element))

(empty-tag-shorthand 'always)
(define display-xexpr (compose display-xml/content xexpr->xml))
(define read-xexpr (compose xml->xexpr read-xml/element))

;;;;; xml pattern matching
(define-match-expander <>
  (syntax-rules () [(_ t p ...) (list 't p ...)]))
(define-match-expander @:
  (syntax-rules () [(_ [k v] ...) (list-no-order [list 'k v] ... _ (... ...))]))

;;;;; convenient functions for creating Intermine's XML

;; (Listof (∪ Xexpr (Listof Xexpr))) -> (Listof Xexpr)
(define (flatten-xexpr xmls)
  (foldr (λ (x xs) (if (xexpr? x) (cons x xs) (append x xs))) '() xmls))

;; String String Xexpr* -> <model>
(define (model name package . body)
  `(model ([name ,name] [package ,package]) ,@(flatten-xexpr body)))

;; String [#:ext (∪ #f String)] [#:int? Bool] Xexpr* -> <class>
(define (class name #:ext [ext #f] #:int? [int? #t] . body)
  `(class ([name ,name] [is-interface ,(if int? "true" "false")] ,@(if ext `([extends ,ext]) '()))
     ,@(flatten-xexpr body)))

;; String String -> <attribute>
(define (attribute n t) `(attribute ([name ,n] [type ,t])))

(define ((link T) n t [r #f])
  `(,T ([name ,n] [referenced-type ,t] ,@(if r `([reverse-reference ,r]) '()))))
(define collection (link 'collection)) ; String String String? -> <collection>
(define reference (link 'reference))   ; String String String? -> <reference>

(define-syntax-rule (pluralize f f*)
  (define-syntax-rule (f* [x (... ...)] (... ...)) (list (f x (... ...)) (... ...))))
(pluralize attribute attributes)
(pluralize collection collections)
(pluralize reference references)


;;;;; TRANSLATION

;; checks whether element has given tag
(define (with-tag? t)
  (match-lambda [(cons s _) (equal? t s)]
                [_ #f]))

;; retain elements with given tag
(define (filter-tag t xs) (filter (with-tag? t) xs))

;; look up key(s) in attribute list
(define (lookup l k) (second (assoc k l)))
(define (lookup* l . ks) (map (curry lookup l) ks))

;;; extract data from Rob's <class>
(define rob/class->prim-id ; <class> -> String
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
(define rob/format->classes ; <format> -> (Listof <class>)
  (match-lambda [(<> format body ...) (filter-tag 'class body)]))

;; translate Rob's XML to Intermine's XML
(define translate
  (match-lambda
    [(<> format (@: [ext ext]) body ...)
     (model ext "FIXME:package"
            (for/list ([b (filter-tag 'class body)])
              (class (rob/class->name b)
                (map (curry apply attribute) (rob/class->attributes b))
                (map (curry apply reference) (rob/class->references b))
                (map (curry apply collection) (rob/class->collections b)))))]))

;;;;; test data
(define rob (read-xexpr (open-input-file "../file_formats/jnct_expr.xml")))
(display-xexpr (translate rob))