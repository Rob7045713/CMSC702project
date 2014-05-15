#lang racket

;; this module provides generic helper utilities
(provide (all-defined-out))

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