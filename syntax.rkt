#lang racket

(provide atomic?
         head-normal?
         heap-bound?
         fwdExec
         fwdEval)

(define (atomic? expr h)
  (match expr
    [(or `(neg ,a)
         `(inv ,a)
         `(exp ,a)
         `(log ,a)
         `(fst ,a)
         `(snd ,a)) (atomic? a h)]
    [(or `(plus ,a ,n)
         `(plus ,n ,a)
         `(times ,a ,n)
         `(times ,n ,a)
         `(less ,a ,n)
         `(less ,n ,a))
     (and (head-normal? n) (atomic? a h))]
    [x (not (heap-bound? x h))]))

(define (head-normal? n h)
  (match n
    [(or (? real? _)
         'unit
         `(pair _ _)
         `(inl _)
         `(inr _)
         'lebesgue
         `(return _)
         `(let _ _ _)
         `(bind _ _)
         'mzero
         `(mplus _ _)) #t]
    [a (atomic? a h)]))

;; TODO
(define (heap-bound? x h) #f)

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
  (match expr
    [`(fst ,e) #:when (not (atomic? e)) (fwdEval e (lambda (n) (fwdEval (car n) c)) h)]
    [`(snd ,e) #:when (not (atomic? e)) (fwdEval e (lambda (n) (fwdEval (cdr n) c)) h)]
    
    ))
