#lang racket/base
;; this module provides convenient functions for constructing Intermine's xml
(provide (all-defined-out))
(require "xexpr.rkt")

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