;; 2.1.1 Example: Arithmetic Operations for Rational Numbers

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(begin (newline)
       (display "Hello, World!")
       (newline)
       (newline)
       (display "Hello, World!")
       (newline))

(define one-half (make-rat 1 2))
(print-rat one-half)
(define one-third (make-rat 1 3))
(print-rat one-third)

(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))

;; simplify rational fractions using gcd
(define (make-rat n d)
  ;; find the gcd, then divide both numer and denom by it
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(print-rat (add-rat one-third one-third))

(define (sgn d)
  "return 1 or -1 depending on the sign of d"
  ((if (< d 0) - +) 1))

(define (make-rat n d) 
  (let ((g ((if (< d 0) - +) (abs (gcd n d))))) 
    (cons (/ n g) (/ d g))))


;; 2.2.1 Representing Sequences

(list 1 2 3 4)

'(1 2 3 4)

(define one-through-four (list 1 2 3 4))
(cdr one-through-four)
(car one-through-four)

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(list-ref one-through-four 0)
(list-ref one-through-four 1)
(list-ref one-through-four 2)
(list-ref one-through-four 3)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
(length one-through-four)


(define squares (list 1 4 9 16 25))
(define odds (list 1 3 5 7))

(append squares odds)
(append odds squares)

;; *Exercise 2.17:* Define a procedure `last-pair' that returns the
;; list that contains only the last element of a given (nonempty)
;; list:

;;      (last-pair (list 23 72 149 34))
;;      (34)


;; note: fails for (last-pair ())
(define (last-pair items)
  (let ((tail (cdr items)))
    (if (null? tail)
        items
        (last-pair tail))))

(last-pair (list 23 72 149 34))

;; *Exercise 2.18:* Define a procedure `reverse' that takes a list as
;; argument and returns a list of the same elements in reverse order:

;; (reverse (list 1 4 9 16 25))
;; (25 16 9 4 1)

(define (reverse items)
  (define (looper items rev)
    (if (null? items)
        rev
        (looper (cdr items) (cons (car items) rev))))
  (looper items (list)))
(reverse (list 1 4 9 16 25))



;; * Exercise 2.20:* The procedures `+', `*', and `list' take
;; arbitrary numbers of arguments. One way to define such procedures
;; is to use `define' with notation "dotted-tail notation".  In a
;; procedure definition, a parameter list that has a dot before the
;; last parameter name indicates that, when the procedure is called,
;; the initial parameters (if any) will have as values the initial
;; arguments, as usual, but the final parameter's value will be a "list"
;; of any remaining arguments.  For instance, given the definition

(define (same-parity . w)
  ;; doesn't work 
  (define (iter xs comp)
    (if (null? xs)
        (list)
        ;; (cons (= comp (even? (car xs))) (iter (cdr xs)))
        (let ((head (car xs))
            (rest (cdr xs))
            (val (even? head)))
          (cons val (iter rest comp)))))
  (if (null? w)
      (list)
      (iter w (even? (car w)))))

(same-parity 1 2 3 4 5 6)
(same-parity)

;; Mapping over lists

(define (scale-list items factor)
  (if (null? items)
      '()
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(define xs (list 1 2 3 4 5))
(scale-list xs 10)

(define (map proc items)
  (if (null? items)
      (list)
      (cons (proc (car items))
            (map proc (cdr items)))))

(map (lambda (x) (* x 14)) xs)
(map (lambda (x) (* x (+ 1 x))) xs)

(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))

(scale-list xs 15)

(define (square-list items)
  (map (lambda (x) (* x x)) items))
(square-list xs)

(define x (cons (list 1 2) (list 3 4)))
(length x)

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
(count-leaves x)

(list x x)
(length (list x x))
(count-leaves (list x x))

(define xs (list 1 (list 2 (list 3 4))))
(count-leaves xs)

;; *Exercise 2.25:* Give combinations of `car's and `cdr's that will
;; pick 7 from each of the following lists:

;; (1 3 (5 7) 9)

;; ((7))

;; (1 (2 (3 (4 (5 (6 7))))))

(list 1 3 (list 5 7) 9)
(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))

;; exercise 2.26

(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y) 
(cons x y)
(car (cons x y))
(cdr (cons x y))

(list x y)

;; *Exercise 2.27:* Modify your `reverse' procedure of *Note Exercise
;; 2-18:: to produce a `deep-reverse' procedure that takes a list as
;; argument and returns as its value the list with its elements
;; reversed and with all sublists deep-reversed as well.  For example,

;; (define x (list (list 1 2) (list 3 4)))

;; x
;; ((1 2) (3 4))

;; (reverse x)
;; ((3 4) (1 2))

;; (deep-reverse x)
;; ((4 3) (2 1))

(define x (list (list 1 2) (list 3 (list 4 5))))
(define (reverse items)
  (define (rev-recur items result)
    (if (null? items)
        result
        (rev-recur (cdr items) (cons (car items) result))))
  (rev-recur items '()))

(define (deep-reverse items)
  (define (dr-recur items result)
    (if (null? items)
        result
        (let ((first (car items)))
          (dr-recur (cdr items)
                    (cons (if (not (pair? first))
                              first
                              (deep-reverse first))
                          result)))))
  (dr-recur items '()))

x
(deep-reverse x)


;; Exercise 2.28
;; *Exercise 2.28:* Write a procedure `fringe' that takes as argument
;; a tree (represented as a list) and returns a list whose elements
;; are all the leaves of the tree arranged in left-to-right order.
;; For example,

(define x (list (list 1 2) (list 3 4)))


(define (fringe tree)
  (define (iter x accum)
    (cond ((null? x) accum)
          ((list? (car x))
           (append (iter (car x) accum)
                   (iter (cdr x) accum)))
          (else (cons (car x) (iter (cdr x) accum)))))
  (iter tree '()))

(fringe '())
(fringe '(1 2))

x
(fringe x)
(1 2 3 4)

(fringe '())
(fringe '(1))
(fringe '(2 1))
(fringe (list 2 1 (list 3 4)))
(fringe (list 1 2 3 (list 3 4) 5 6))

(fringe (list x x))
(1 2 3 4 1 2 3 4)

;; Exercise 2.29 Binary Mobile

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  ;; structure can be a number (weight) or another mobile
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  ;; take the car of the cdr to remove trailing nil
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))


(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((not (pair? mobile)) mobile)
        (else (+ (total-weight (branch-structure (left-branch mobile)))
                 (total-weight (branch-structure (right-branch mobile)))))))

 ;; Test  
(define a (make-mobile (make-branch 2 3) (make-branch 2 7)))
(total-weight a) ;; 6 


(define (torque branch)
  (* (branch-length branch) (total-weight branch)))

(branch-length (make-branch 2 3))
(total-weight (make-branch 2 3))
(torque (make-branch 2 3))



;; Mapping over trees
(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))
(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 15)

(define (scale-tree-two tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree-two sub-tree factor)
             (* sub-tree factor)))
       tree))

(scale-tree-two (list 1 (list 2 (list 3 4) 5) (list 6 7)) 12)

;; Exercise 2.30
;; square-tree

(define (square x) (* x x))
(define (square-tree x)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       x))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

;; Exercise 2.32
;; subsets

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
                            (cons (car s) x)) rest)))))
(subsets (list 1))
(subsets (list 1 2))
(subsets (list 1 2 3))
(subsets (list 1 2 3 4))

;; 2.2.3 Sequences as Conventional Interfaces

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(map square (list 1 2 3 4 5))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(filter odd? (list 1 2 3 4 5))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))
(accumulate * 1 (list 1 2 3 4 5))
(accumulate cons '() (list 1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))
(enumerate-interval 2 7)

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

(define (sum-odd-squares tree)
  (accumulate 
   + 0
   (map square (filter odd? (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate
   cons '()
   (filter 
    even?
    (map fib (enumerate-interval 0 n)))))

;; Nested mappings

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))
(prime-sum-pairs 5)


;; Exercise 2.37



(define x '(1 2 3 4))
(define y '(4 5 6 6))
(map * x y)
(dot-product x y)
(+ 4 10 18 24)

(define v1 (list 1 2 3 4))
(define v2 (list 5 6 7 8))
(define v3 (list 9 10 11 12))
(define matrix (list v1 v2 v3)) 
matrix

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;; test
(dot-product v1 v2)

(define (matrix-*-vector m v)
  (map (lambda m-row) (dot-product m-row v)
       m))

;; test
(matrix-*-vector matrix (list 2 3 4 5))

;; Sec 2-2-3 Nested mappings
Consider this problem:
;; Given a positive integer n, find all ordered pairs of distinct positive
;; integers i and j, where 1 <= j< i<= n, such that i + j is prime.  For
;; example, if n is 6, then the pairs are the following:

;;        i   | 2 3 4 4 5 6 6
;;        j   | 1 2 1 3 2 1 5
;;      ------+---------------
;;      i + j | 3 5 5 7 7 7 11


(define (get-pairs n)
  "generate all pairs (i, j) where i <= n and j < i"
  )

;; these are defined above (and copied here)
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(enumerate-interval 1 5)

(accumulate append
            '()
            (map (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 n)))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (contains list x)
  (cond 
   ((null? list) #f)
   ((eq? (car list) x) #t)
   (else (contains (cdr list) x))))

;; dummy version of prime?
(define (prime? n) (contains '(1 2 3 5 7 11 13 17) n))
(prime? 8)

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))
(prime-sum-pairs 8)


(define (permutations s)
  (if (null? s)                    ; empty set?
      (list '())                   ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))
(remove 3 '(1 3 5 7))

(permutations (list 1 2 3))

;; 2.40

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
(unique-pairs 5)


;; flatmap takes a function of 1 variable, and applies
;; it to every item in seq
;; (flatmap func seq)
;; then accumulates all the results into a flat seq

;; 2.41
(define nil '())
(define (unique-tuples n k)
  (cond ((< n k) nil)
        ((= k 0) (list nil))
        (else (append (unique-tuples (- n 1) k)
                      (map (lambda (tuple) (cons n tuple))
                           (unique-tuples (- n 1) (- k 1)))))))
(unique-tuples 4 3)

(define (triples-of-sum s n)
  (filter (lambda (seq) (= (accumulate + 0 seq) s))
          (unique-tuples n 3)))

;; (accumulate + 0 '(1 2 3 4))

(triples-of-sum 12 30)

;; 2.42 eight-queens puzzle

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
