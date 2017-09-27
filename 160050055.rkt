#lang racket

(provide pred-p single-digit-p single-alphabet-p seq alt epsilon-p
         zero-or-more one-or-more whitespace-p number-p identifier-p
         variable-p term-p expression-p assignment-p combine-cc
         combine-sc combine-ss combine-cs)

(define-struct gnode (sym list) #:transparent)
(define-struct ident (str) #:transparent)
(define-struct num (val) #:transparent)

(define (combine-cc char1 char2)
  (string char1 char2))

(define (combine-sc str char)
  (list->string (append (string->list str)
                        (list char))))

(define (combine-cs char str)
  (list->string (cons char (string->list str))))

(define (combine-ss str1 str2)
  (list->string (append (string->list str1)
                        (string->list str2))))

(define (pred-p p)
  (lambda (str) (if (string=? str "") 'fail
                    (let ([first (string-ref str 0)])
                      (if (p first) (cons first (substring str 1))
                          'fail)))))

(define single-digit-p
  (pred-p (lambda (c) (char-numeric? c))))

(define single-alphabet-p
  (pred-p (lambda (c) (char-alphabetic? c))))

(define (seq p1 p2 f)
  (lambda (str) (if (string=? str "") 'fail
                    (let ([x (p1 str)])
                      (if (equal? x 'fail) 'fail
                          (let ([y (p2 (cdr x))])
                            (if (equal? y 'fail) 'fail
                                (cons (f (car x) (car y)) (cdr y)))))))))

(define (alt p1 p2)
  (lambda (str) (let* ([x (p1 str)]
                       [y (p2 str)])
                  (if (not (equal? x 'fail)) (p1 str)
                      (if (not (equal? y 'fail)) (p2 str)
                          'fail)))))

(define epsilon-p
  (lambda (str) (cons "" str)))

(define (zero-or-more p f)
  (lambda (str) (let ([x (p str)])
                  (if (equal? x 'fail) (epsilon-p str)
                      (let ([y ((zero-or-more p f) (substring str 1))])
                        (cons (f (car x) (car y)) (cdr y)))))))

(define (one-or-more p f)
  (lambda (str) (let ([x ((zero-or-more p f) str)])
                  (if (string=? (car x) "") 'fail
                      x))))

(define whitespace-p
  (lambda (str) (epsilon-p (cdr ((zero-or-more
                                  (pred-p (lambda (c) (equal? c #\space)))
                                  combine-cs) str)))))

(define number-p
  (lambda (str) (let* ([x (whitespace-p str)]
                       [y ((one-or-more single-digit-p combine-cs) (cdr x))])
                  (if (equal? y 'fail) 'fail
                      (cons (num (string->number (car y))) (cdr y))))))

(define identifier-p
  (lambda (str) (let* ([x (whitespace-p str)]
                       [y ((seq (one-or-more single-alphabet-p combine-cs)
                                (zero-or-more single-digit-p combine-cs)
                                combine-ss) (cdr x))])
                  (if (equal? y 'fail) 'fail
                      (cons (ident (car y)) (cdr y))))))

(define variable-p
  (lambda (str) (let ([x (identifier-p str)])
                  (if (equal? x 'fail) 'fail
                      (let ([y1 ((pred-p (lambda (c) (equal? c #\[)))
                                 (cdr (whitespace-p (cdr x))))])
                        (if (equal? y1 'fail) x
                            (let ([z (expression-p (cdr (whitespace-p (cdr y1))))])
                              (if (equal? z 'fail) x
                                  (let ([y2 ((pred-p (lambda (c) (equal? c #\])))
                                             (cdr (whitespace-p (cdr z))))])
                                    (if (equal? y2 'fail) x
                                        (cons (gnode 'ARRAY (list (car x) (car z)))
                                              (cdr y2))))))))))))

(define term-p
  (lambda (str) (let ([x ((alt number-p variable-p) str)])
                  (if (not (equal? x 'fail)) x
                      (let ([y1 ((pred-p (lambda (c) (equal? c #\()))
                                 (cdr (whitespace-p str)))])
                        (if (equal? y1 'fail) 'fail
                            (let ([z (expression-p (cdr (whitespace-p (cdr y1))))])
                              (if (equal? z 'fail) 'fail
                                  (let ([y2 ((pred-p (lambda (c) (equal? c #\))))
                                             (cdr (whitespace-p (cdr z))))])
                                    (if (equal? y2 'fail) 'fail
                                        (cons (car z) (cdr y2))))))))))))


(define expression-p
  (lambda (str) (let ([x (term-p str)])
                  (if (equal? x 'fail) 'fail
                      (let ([y1 ((pred-p (lambda (c) (equal? c #\+)))
                                 (cdr (whitespace-p (cdr x))))])
                        (if (equal? y1 'fail) x
                            (let ([z (expression-p (cdr (whitespace-p (cdr y1))))])
                              (if (equal? z 'fail) x
                                  (cons (gnode 'PLUS (list (car x) (car z)))
                                        (cdr z))))))))))

(define assignment-p
  (lambda (str) (let ([x (variable-p str)])
                  (if (equal? x 'fail) 'fail
                      (let ([y ((pred-p (lambda (c) (equal? c #\=)))
                                (cdr (whitespace-p (cdr x))))])
                        (if (equal? y 'fail) 'fail
                            (let ([z (expression-p (cdr (whitespace-p (cdr y))))])
                              (if (equal? z 'fail) 'fail
                                  (cons (gnode 'ASSIGN (list (car x) (car z)))
                                        (cdr z))))))))))