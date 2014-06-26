
(define (id obj) obj)
(define (flip func) (lambda (arg1 arg2) (func arg2 arg1)))
(define (curry func arg1) (lambda (arg) (apply func (cons arg1 (list arg)))))
(define (compose f g) (lambda (arg) (f (apply g arg))))

; 6.4 Pairs and lists
(define (set-car! pair obj) (cons obj (cdr pair)))
(define (set-cdr! pair obj) (cons (car pair) obj))
(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))
(define (cddar x) (car (cdr (cdr x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (cdddar x) (car (cdr (cdr (cdr x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))
(define (null? obj) (if (eqv? obj '()) #t #f))
(define (list . objs) objs)
;(define (make-list . args))

(define (foldr func end lst)
    (if (null? lst)
        end
        (func (car lst) (foldr func end (cdr lst)))))
(define (foldl func accum lst)
  (if (null? lst)
    accum
    (foldl func (func accum (car lst)) (cdr lst))))
(define fold foldl)
(define reduce fold)
(define (unfold func init pred)
  (if (pred init)
    (cons init '())
    (cons init (unfold func (func init) pred))))

(define (append1 a b)
  (foldr (lambda (x xs) (cons x xs)) b a))
(define (append . lst)
  (foldr (lambda (x xs) (append1 x xs)) '() lst))

(define (sum . lst) (fold + 0 lst))
(define (product . lst) (fold * 1 lst))
(define (and . lst) (fold && #t lst))
(define (or . lst) (fold || #f lst))

(define (length lst) (fold (lambda (x y) (+ x 1)) 0 lst))
(define (reverse lst) (fold (flip cons) '() lst))

(define (map func lst) (foldr (lambda (x y) (cons (func x) y)) '() lst))
(define (filter pred lst) (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() lst))

(define list-tail
  (lambda (x k)
    (if (zero? k)
      x
      (list-tail (cdr x) (- k 1)))))
(define drop list-tail)

(define list-ref
  (lambda (x k)
    (cond
      ((null? x) #f)
      ((zero? k) (car x))
      (else (list-ref (cdr x) (- k 1))))))

;;
;; basic predicates
;;
(define (not x) (if x #f #t))
(define zero? (curry = 0))
(define positive? (curry < 0))
(define negative? (curry > 0))
(define (odd? num) (= (mod num 2) 1))
(define (even? num) (= (mod num 2) 0))

;;
;; assosiative array
;;
(define (mem-helper pred op) (lambda (acc next) (if (and (not acc) (pred (op next))) next acc)))
(define (memq obj lst) (fold (mem-helper (curry eq? obj) id) #f lst))
(define (memv obj lst) (fold (mem-helper (curry eqv? obj) id) #f lst))
(define (member obj lst) (fold (mem-helper (curry equal? obj) id) #f lst))
(define (assq obj alist) (fold (mem-helper (curry eq? obj) car) #f alist))
(define (assv obj alist) (fold (mem-helper (curry eqv? obj) car) #f alist))
(define (assoc obj alist) (fold (mem-helper (curry equal? obj) car) #f alist))

;;
;; mathmatical functions
;;
(define (max first . rest) (fold (lambda (old new) (if (> old new) old new)) first rest))
(define (min first . rest) (fold (lambda (old new) (if (< old new) old new)) first rest))
