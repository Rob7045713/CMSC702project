#lang racket/base

;; this module provides convenient functions/macros for dealing with XML
(provide (all-defined-out))

(require racket/match racket/list racket/function)
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

;; checks whether element has given tag
(define (with-tag? t)
  (match-lambda [(cons s _) (equal? t s)]
                [_ #f]))

;; retain elements with given tag
(define (filter-tag t xs) (filter (with-tag? t) xs))

;; look up key(s) in attribute list
(define (lookup l k) (second (assoc k l)))
(define (lookup/list l ks) (map (curry lookup l) ks))
(define (lookup* l . ks) (lookup/list l ks))

(define xexpr->attributes second)