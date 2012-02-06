#lang s-exp "fallthrough-racket.rkt"
(require (for-syntax racket/base))

(register-fallthrough

 ;; Code by Chris Jester-Young.
 ;; See: http://stackoverflow.com/questions/9152279/cadr-macro-in-racket
 (lambda (stx)
   (define (id->string id)
     (symbol->string (syntax->datum id)))
   (define (decomp id)
     (define match (regexp-match #rx"^c([ad])(.*)r$" (id->string id)))
     (define func (case (string-ref (cadr match) 0)
                    ((#\a) 'car)
                    ((#\d) 'cdr)))
     (datum->syntax id (list func (string->symbol (format "c~ar" (caddr match))))))

   (syntax-case stx ()
     ((c*r x) 
      (regexp-match #rx"^c[ad]+r$" (id->string #'c*r))
      (with-syntax (((a d) (decomp #'c*r)))
        (syntax-case #'d (cr)
          (cr 
           #'(#%plain-app a x))
          (_ 
           #'(#%plain-app a (d x))))))
     (else
      #f))))

(define (my-sixth x)
  (cadddddr x))