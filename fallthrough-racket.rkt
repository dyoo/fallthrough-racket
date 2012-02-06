#lang racket/base

(require (for-syntax racket/base))

(provide (except-out (all-from-out racket/base) #%app)
         (rename-out (my-app #%app))
         register-fallthrough)

(begin-for-syntax
  (define fallthrough-transformers
    '())
  (define (add! f)
    (set! fallthrough-transformers
          (cons f fallthrough-transformers))))

(define-syntax (my-app stx)
  ;; First, see if any other macro that registers
  ;; with us is interested.
  (syntax-case stx ()
    [(_ op ...)
     (begin
       (define unwrapped-stx (syntax/loc stx (op ...)))
       (let loop ([transformers fallthrough-transformers])
         (cond
           [(null? transformers)
            (syntax/loc stx
              (#%app op ...))]
           [((car transformers)
             unwrapped-stx)
            => 
            (lambda (x) x)]
           [else
            (loop (cdr transformers))])))]))


(define-syntax (register-fallthrough stx)
  (syntax-case stx ()
    [(_ body)
     (syntax/loc stx
       (begin-for-syntax
         (add! body)))]))