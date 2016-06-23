#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
(define (sequence spacing low high)
  (if (> low high)
      null
      (cons low (sequence spacing (+ low spacing) high))))

(define (string-append-map xs suffix)
  (map (lambda (i) (string-append i suffix)) xs))
  
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-k-steps s k)
  (if (= k 0)
      null
      (cons (car (s)) (stream-for-k-steps (cdr (s)) (- k 1)))))

(define funny-number-stream
  (letrec ([f (lambda (x) (if (= (remainder x 6) 0)
                              (cons (- x) (lambda () (f (+ x 1))))
                              (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x) (if (= x 1)
                              (cons "dan.jpg" (lambda () (f 0)))
                              (cons "dog.jpg" (lambda () (f 1)))))])
    (lambda () (f 1))))

(define (stream-add-one s)
  (letrec ([f (lambda (x) (cons (cons 1 (car (x))) (lambda () (f (cdr (x))))))])
    (lambda () (f s))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([len (vector-length vec)]
           [helper (lambda (i) (if (< i len)
                                   (letrec ([p (vector-ref vec i)])
                                     (if (and (pair? p) (equal? v (car p)))
                                         p
                                         (helper (+ i 1))))
                                   #f))])
    (helper 0)))

(define (caching-assoc xs n)
  (letrec ([index 0]
           [cache (make-vector n #f)]
           [ret-func (lambda (v) (letrec ([r (vector-assoc v cache)])
                                   (if r
                                   (begin (print "found in cache") r)
                                   (letrec ([t (vector-assoc v xs)])
                                     (if t
                                         (begin (print "found in xs")
                                                (vector-set! cache index t)
                                                (set! index (remainder (+ index 1) n))
                                                t)
                                         #f)))))])
    ret-func))

;(define-syntax while-greater
;  (syntax-rules (do)
;    [(while-greater e1 do e2)
;     (let ([t e1])
;       (if (< t e2)
;           (while-greater t do e2)
;           #t))]))

(define-syntax while-greater
  (syntax-rules (do)
    [(while-greater e1 do e2)
     (letrec ([t e1]
              [loop (lambda (i)
                      (if (< t i)
                          (loop e2)
                          #t))])
       (loop e2))]))

(define (cycle-lists-chal xs ys)
  (letrec ([xlen (length xs)]
           [ylen (length ys)]
           [f (lambda (i)
                (cons (cons (list-ref xs (remainder i xlen)) (list-ref ys (remainder i ylen))) (lambda () (f (+ i 1)))))])
    (lambda () (f 0))))

                     
  
