#lang racket

(define (atomic? expr h)
  (match expr
    [(or `(neg ,a)
         `(inv ,a)
         `(exp ,a)
         `(log ,a)
         `(fst ,a)
         `(snd ,a)) (atomic? a)]
    [(or `(plus ,a ,n)
         `(plus ,n ,a)
         `(times ,a ,n)
         `(times ,n ,a)
         `(less ,a ,n)
         `(less ,n ,a))
     (and (head-normal? n) (atomic? a))]
    [x (not (heap-bound? x h))]))

;; TODO
(define (head-normal? n) #t)

;; TODO
(define (heap-bound? x h) #t)

(define (fwdExec expr c h)
  (match expr    
    ['lebesgue `(bind lebesgue (lambda (x) (,c x ,h)))] ;; should this be gensym instead of x?
    [`(return ,e) (fwdEval e c h)]
    [`(let ,in ,e1 (lambda (,x) ,e2)) (fwdEval e2 (lambda (m) (fwdExec m c)) (append h `(let ,in ,x = ,e1)))]
    [`(bind ,e1 (lambda (,x) ,e2))    (fwdEval e2 (lambda (m) (fwdExec m c)) (append h `(,x <~ ,e1)))]
    [`(factor ,e1 ,e2)                (fwdEval e2 (lambda (m) (fwdExec m c)) (append h `(factor ,e1)))]
    ['mzero 'mzero]
    [`(mplus ,e1 ,e2) `(mplus ,(fwdEval e1 (lambda (m) (fwdExec m c)) h)
                              ,(fwdEval e2 (lambda (m) (fwdExec m c)) h))]
    [a #:when (atomic? a) `(bind ,a (lambda (x) (,c x ,h)))] ;; should this be gensym instead of x?
    ))

(define (fwdEval expr c h)
  ;; (match expr )
  ;; TODO
  expr)

;; ----------------------------------------------------------------------------
;; tests

(fwdExec 'lebesgue (lambda (x) x) 5)
(fwdExec '(return lebesgue) 7 7)
(fwdExec '(let inl (5 or 6) (lambda (y) lebesgue)) (lambda (x) x) '())
(fwdExec '(bind lebesgue (lambda (y) (return lebesgue))) (lambda (x) x) '())
