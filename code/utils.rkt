#lang racket/base

;; this module provides generic helper utilities
(provide (all-defined-out))
(require racket/set racket/match racket/string)

;; empty set
(define ∅ {set})

;; unbox a value or leave a non-box as is
(define maybe-unbox
  (match-lambda [(box x) x]
                [x x]))

;; returns the index of x's first occurence in xs or #f if it's not in there
(define (index-of x xs)
  (for/first ([z xs] [i (in-naturals)] #:when (equal? z x)) i))

;; takes values in xs at given indices
(define (take-indices xs indices)
  (for/list ([i indices]) (list-ref xs i)))

;; String -> String
;; escape special characters. FIXME: there must be a standard way
(define (escape s)
  (string-replace s "\\t" "\t"))

(define ∘ compose)

;; convert a hashtable to a function
(define ((map->fun m) x)
  (hash-ref m x (λ () (error "out of domain" x (hash-keys m)))))
