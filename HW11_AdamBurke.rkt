;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW11_AdamBurke) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;CS2500 HW11
;Adam Burke and Gabby Garcia, ltd.
;11/28/2020


;                                                                                   
;                                                                                   
;                                                                                   
;                                                                                   
;   ;;;;;;;                                    ;;             ;                     
;   ;  ;  ;                                     ;             ;                     
;   ;  ;  ;                                     ;             ;                     
;      ;       ;;;;   ;; ;;;; ;;;;   ;; ;;;     ;    ;;;;    ;;;;     ;;;;    ;;;;; 
;      ;      ;    ;   ;;   ;;   ;    ;;   ;    ;   ;    ;    ;      ;    ;  ;    ; 
;      ;      ;    ;   ;    ;    ;    ;    ;    ;       ;;    ;      ;    ;   ;   ; 
;      ;      ;;;;;    ;    ;    ;    ;    ;    ;   ;;;; ;    ;      ;;;;;     ;;   
;      ;      ;        ;    ;    ;    ;    ;    ;   ;    ;    ;      ;       ;   ;; 
;      ;      ;    ;   ;    ;    ;    ;    ;    ;   ;    ;    ;  ;   ;    ;  ;    ; 
;     ;;;      ;;;;   ;;;  ;;;  ;;;   ;;  ;    ;;;   ;;;; ;    ;;;    ;;;;   ;;;;;  
;                                     ; ;;                                          
;                                     ;                                             
;                                    ;;;                                            
;                                                                                   
;                                                                                   

;PARALLEL LIST TRAVERSAL:
(define (parallel-traversal-templ lst1 lst2)
  (cond [(and (empty? lst1) (empty? lst2)) ...]
        [(and (empty? lst1) (cons? lst2)) (... (first lst2)
                                               (parallel-traversal-templ lst1 (rest lst2)))]
        [(and (cons? lst1) (empty? lst2)) (... (first lst1)
                                               (parallel-traversal-templ (rest lst1) lst2))]
        [(and (cons? lst1) (cons? lst2)) (... (... (first lst1) ... (first lst2)) 
                                              (parallel-traversal-templ ((rest lst1) ...
                                                                         (rest lst2))))]))

;Exercise 1

;list-prefix?: [List-of Number] [List-of Number] -> Boolean
;is _lon1_ a valid prefix of _lon2_?
(check-expect (list-prefix? '(1 2 3) '(1 2 3)) #t)
(check-expect (list-prefix? '(1 2 3) '(1 2)) #f)
(check-expect (list-prefix? '(1 2 3) '(1 2 3 4)) #t)
(check-expect (list-prefix? '(1 2 3) '()) #f)
(check-expect (list-prefix? '() '(1 2 3)) #t)
(check-expect (list-prefix? '() '()) #t)
(check-expect (list-prefix? '(1 2 3) '(3 2 1)) #f)
(check-expect (list-prefix? '(1 2 3) '(1 0 2 0 3)) #f)
(check-expect (list-prefix? '(1 2 3) '(1 1 2 2 3 3)) #f)
;from the parallel traversal template:
#;(define (list-prefix? lon1 lon2)
    (cond [(and (empty? lon1) (empty? lon2)) #t]
          [(and (empty? lon1) (cons? lon2)) #t]
          [(and (cons? lon1) (empty? lon2)) #f]
          [(and (cons? lon1) (cons? lon2)) (and (= (first lon1) (first lon2)) 
                                                (list-prefix? (rest lon1)
                                                              (rest lon2)))]))
;simplifying:
(define (list-prefix? lon1 lon2)
  (cond [(and (cons? lon1) (empty? lon2)) #f]
        [(and (cons? lon1) (cons? lon2)) (and (= (first lon1) (first lon2)) 
                                              (list-prefix? (rest lon1)
                                                            (rest lon2)))]
        [else #t]))


;Exercise 2

;max-splice: [List-of Number] [List-of Number] ->  [List-of Number]
;splices _lon1_ and _lon2_ such that the resulting [List-of Number] contains all the elements of
;_lon1_ in original order and ends with all the elements of _lon2_, but omits elements that are common
;to the end of _lon1_ and the beginning of _lon2_ in order to shorten the resulting [List-of Number]
#|(check-expect (max-splice '(1 2 3 4) '(2 3 4 5)) '(1 2 3 4 5))
(check-expect (max-splice '(1 2 3 4) '(2 2 3 4 5)) '(1 2 3 4 2 2 3 4 5))
(check-expect (max-splice '(1 2 3) '()) '(1 2 3))
(check-expect (max-splice '() '(4 5 6)) '(4 5 6))
(check-expect (max-splice '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
(check-expect (max-splice '() '()) '())
(check-expect (max-splice '(1 1 1) '(1 1 1)) '(1 1 1))
(check-expect (max-splice '(1 1 1) '(1 1)) '(1 1 1))
(check-expect (max-splice '(1 1) '(1 1 1)) '(1 1 1))|#
#;(define (max-splice lon1 lon2)
    (cond [(and (empty? lon1) (empty? lon2)) '()]
          [(and (empty? lon1) (cons? lon2)) lon2]
          [(and (cons? lon1) (empty? lon2)) lon1]
          [(and (cons? lon1) (cons? lon2)) (cons (first lon1)
                                                 (if (list-prefix? (rest lon1) lon2)
                                                     (cons (rest lon1) (max-splice (rest lon1) (rest lon2)))
                                                     (max-splice (rest lon1) (rest lon2))))]))


;Exercise 3

;valid-results?: (X Y) [List-of X] [List-of [X -> Y]] [List-of Y] -> Boolean
;for all members Xi, XYi, Yi, of _lox_, _lox->y_, and _loy_, respectively,
;does (XYi Xi) == Yi?
;i.e., at each index, i, of the three lists, does the function [X -> Y] applied to the input X match
;the output Y?
(check-expect (valid-results? (list 1 2 3)
                              (list (λ (n) (+ n 1))
                                    (λ (n) (+ n 2))
                                    (λ (n) (+ n 3)))
                              (list 2 4 6)) #t)
(check-expect (valid-results? (list 1 2 3)
                              (list (λ (n) (+ n 1))
                                    (λ (n) (+ n 2))
                                    (λ (n) (+ n 3)))
                              (list 2 4 5)) #f)
(check-expect (valid-results? (list 1 2)
                              (list (λ (n) (+ n 1))
                                    (λ (n) (+ n 2))
                                    (λ (n) (+ n 3)))
                              (list 2 4 6)) #f)
(check-expect (valid-results? (list 1 2 3)
                              (list (λ (n) (+ n 1))
                                    (λ (n) (+ n 2)))
                              (list 2 4 6)) #f)
(check-expect (valid-results? (list 1 2 3)
                              (list (λ (n) (+ n 1))
                                    (λ (n) (+ n 2))
                                    (λ (n) (+ n 3)))
                              (list 2 4)) #f)

(check-expect (valid-results? (list "12" "345" "6789" "01234")
                              (local [(define (divis? n d) ;; Integer Integer -> Boolean
                                        (= (modulo n d) 0))]
                                (list (λ (str) (divis? (string-length str) 2))
                                      (λ (str) (divis? (string-length str) 3))
                                      (λ (str) (divis? (string-length str) 4))
                                      (λ (str) (divis? (string-length str) 5))))
                              (list #t #t #t #t)) #t)
(check-expect (valid-results? '()
                              (local [(define (divis? n d) ;; Integer Integer -> Boolean
                                        (= (modulo n d) 0))]
                                (list (λ (str) (divis? (string-length str) 2))
                                      (λ (str) (divis? (string-length str) 3))
                                      (λ (str) (divis? (string-length str) 4))
                                      (λ (str) (divis? (string-length str) 5))))
                              (list #t #t #t #t)) #f)
(check-expect (valid-results? (list "12" "345" "6789" "01234")
                              '()
                              (list #t #t #t #t)) #f)
(check-expect (valid-results? (list "12" "345" "6789" "01234")
                              (local [(define (divis? n d) ;; Integer Integer -> Boolean
                                        (= (modulo n d) 0))]
                                (list (λ (str) (divis? (string-length str) 2))
                                      (λ (str) (divis? (string-length str) 3))
                                      (λ (str) (divis? (string-length str) 4))
                                      (λ (str) (divis? (string-length str) 5))))
                              '()) #f)
(check-expect (valid-results? '()
                              '()
                              (list #t #t #t #t)) #f)
(check-expect (valid-results? (list "12" "345" "6789" "01234")
                              '()
                              '()) #f)
(check-expect (valid-results? '()
                              (local [(define (divis? n d) ;; Integer Integer -> Boolean
                                        (= (modulo n d) 0))]
                                (list (λ (str) (divis? (string-length str) 2))
                                      (λ (str) (divis? (string-length str) 3))
                                      (λ (str) (divis? (string-length str) 4))
                                      (λ (str) (divis? (string-length str) 5))))
                              '()) #t)
(check-expect (valid-results? '()
                              '()
                              '()) #t)
(check-expect (valid-results? (list "12" "345" "6789" "01234")
                              '()
                              (list "12" "345" "6789" "01234")) #t)
(check-expect (valid-results? (list "12" "345" "6789" "0123")
                              (local [(define (divis? n d) ;; Integer Integer -> Boolean
                                        (= (modulo n d) 0))]
                                (list (λ (str) (divis? (string-length str) 2))
                                      (λ (str) (divis? (string-length str) 3))
                                      (λ (str) (divis? (string-length str) 4))
                                      (λ (str) (divis? (string-length str) 5))))
                              (list #t #t #t #t)) #f)
(check-expect (valid-results? (list "12" "345" "6789" "01234")
                              (local [(define (divis? n d) ;; Integer Integer -> Boolean
                                        (= (modulo n d) 0))]
                                (list (λ (str) (divis? (string-length str) 2))
                                      (λ (str) (divis? (string-length str) 3))
                                      (λ (str) (divis? (string-length str) 4))
                                      (λ (str) (divis? (string-length str) 4))))
                              (list #t #t #t #t)) #f)
(check-expect (valid-results? (list "12" "345" "6789" "01234")
                              (local [(define (divis? n d) ;; Integer Integer -> Boolean
                                        (= (modulo n d) 0))]
                                (list (λ (str) (divis? (string-length str) 2))
                                      (λ (str) (divis? (string-length str) 3))
                                      (λ (str) (divis? (string-length str) 4))
                                      (λ (str) (divis? (string-length str) 5))))
                              (list #t #t #t #f)) #f)

#;(define (valid-results? lox lox->y loy)
    (cond [(and (empty? lox) (empty? lox->y) (empty? loy)) #t]
          [(and (cons? lox) (empty? lox->y) (empty? loy)) #f]
          [(and (empty? lox) (cons? lox->y) (empty? loy)) #t]
          [(and (empty? lox) (empty? lox->y) (cons? loy)) #f]
          [(and (cons? lox) (cons? lox->y) (empty? loy)) #f]
          [(and (empty? lox) (cons? lox->y) (cons? loy)) #f]
          [(and (cons? lox) (empty? lox->y) (cons? loy)) (and (eq? (first lox) (first loy))
                                                              (valid-results? (rest lox)
                                                                              '()
                                                                              (rest loy)))]
          [(and (cons? lox) (cons? lox->y) (cons? loy)) (and (eq? ((first lox->y) (first lox)) (first loy))
                                                             (valid-results? (rest lox)
                                                                             (rest lox->y)
                                                                             (rest loy)))]))
;simplifying:
(define (valid-results? lox lox->y loy)
  (cond [(or (and (empty? lox) (empty? lox->y) (empty? loy))
             (and (empty? lox) (cons? lox->y) (empty? loy))) #t]
        [(and (cons? lox) (empty? lox->y) (cons? loy)) (and (eq? (first lox) (first loy))
                                                            (valid-results? (rest lox)
                                                                            '()
                                                                            (rest loy)))]
        [(and (cons? lox) (cons? lox->y) (cons? loy)) (and (eq? ((first lox->y) (first lox)) (first loy))
                                                           (valid-results? (rest lox)
                                                                           (rest lox->y)
                                                                           (rest loy)))]
        [else #f]))


;Exercise 4

(define-struct assignment [role person])
;An Assignment is a (make-assignment Symbol [Maybe String]
;interp.: An Assignment, (make-assignment r p), describes an assigned task to a person _p_ who has
;the role _r_
;EXAMPLES:
(define ass-1 (make-assignment 'accountant "Alice"))
(define ass-2 (make-assignment 'banker "Bob"))
(define ass-3 (make-assignment 'contractor "Charlie"))
(define ass-4-f (make-assignment 'developer #f)) ;; the developer role is unfilled here
(define ass-5-f (make-assignment 'electrician #f))
(define ass-6-f (make-assignment 'financier #f))
;ass-templ: Assignment -> ???
#;(define (ass-templ a)
    (... (assignment-role a)
         (cond [(string? (assignment-person a)) ...] ;; String case
               [else ...]))) ;; #f case

;assign: [List-of Symbol] [List-of String] -> [List-of Assignment]
;matches up the given list of roles _losym_ with the given list of persons _lostr_ to construct a
;a [List-of Assignment] where:
;- If  _lostr_ is shorter than _losym_--if there are more roles than persons--then all remaining
;  roles are paired with #f in [List-of Assignment]
;- If _losym_ is shorter than _losym_--if there are more persons than roles--then all remaining
;  the extra people are ignored when constructing the resulting [List-of Assignment]
(check-expect (assign (list 'accountant 'banker 'contractor 'developer 'electrician)
                      (list "Alice" "Bob" "Charlie" "David" "Elsie"))
              (list (make-assignment 'accountant "Alice")
                    (make-assignment 'banker "Bob")
                    (make-assignment 'contractor "Charlie")
                    (make-assignment 'developer "David")
                    (make-assignment 'electrician "Elsie")))
(check-expect (assign '()
                      (list "Alice" "Bob" "Charlie" "David" "Elsie"))
              '())
(check-expect (assign (list 'accountant 'banker 'contractor 'developer 'electrician)
                      '())
              (list (make-assignment 'accountant #f)
                    (make-assignment 'banker #f)
                    (make-assignment 'contractor #f)
                    (make-assignment 'developer #f)
                    (make-assignment 'electrician #f)))
(check-expect (assign '()
                      '())
              '())
(check-expect (assign (list 'accountant 'banker 'contractor 'developer )
                      (list "Alice" "Bob" "Charlie" "David" "Elsie"))
              (list (make-assignment 'accountant "Alice")
                    (make-assignment 'banker "Bob")
                    (make-assignment 'contractor "Charlie")
                    (make-assignment 'developer "David")))
(check-expect (assign (list 'accountant 'banker 'contractor 'developer 'electrician)
                      (list "Alice" "Bob" "Charlie" "David"))
              (list (make-assignment 'accountant "Alice")
                    (make-assignment 'banker "Bob")
                    (make-assignment 'contractor "Charlie")
                    (make-assignment 'developer "David")
                    (make-assignment 'electrician #f)))
(define (assign losym lostr)
  (cond [(and (empty? losym) (empty? lostr)) '()]
        [(and (empty? losym) (cons? lostr)) '()]
        [(and (cons? losym) (empty? lostr)) (cons (make-assignment (first losym) #f)
                                                  (assign (rest losym) '()))]
        [(and (cons? losym) (cons? lostr)) (cons (make-assignment (first losym) (first lostr))
                                                 (assign (rest losym) (rest lostr)))]))



;Exercise 5

(define-struct bt [value left right])
;A BinaryTree is one of:
;- 'none
;- (make-bt Symbol BinaryTree BinaryTree)
;interp.: A self-referential data structure where each non-terminating node, (make-bt i l r)
;contains some information _i_ and references its left and right children, _l_ and _r_, respectively
;EXAMPLES:
(define bt-0 'none)
(define bt-1 (make-bt 'a
                      (make-bt 'b
                               (make-bt 'd bt-0 bt-0)
                               (make-bt 'e bt-0 bt-0))
                      (make-bt 'c
                               (make-bt 'f bt-0 bt-0)
                               (make-bt 'g bt-0 bt-0))))
(define bt-2 (make-bt 'a
                      (make-bt 'c
                               (make-bt 'f bt-0 bt-0)
                               (make-bt 'g bt-0 bt-0))
                      (make-bt 'b
                               (make-bt 'e bt-0 bt-0)
                               (make-bt 'd bt-0 bt-0))))
;bt-templ: BinaryTree -> ???
#;(define (bt-templ bt)
    (cond [(symbol? bt) ...]
          [(bt? bt) (... (bt-value bt)
                         (bt-templ (bt-left bt))
                         (bt-templ (bt-right bt)))]))

;tree-equiv?: BinaryTree BinaryTree -> Boolean
;Are there any equivalent subtrees (including the top-level tree) in the two BinaryTrees _bt1_ and
;_bt2_?
(check-expect (tree-equiv? bt-0 bt-0) #t)
(check-expect (tree-equiv? bt-1 bt-2) #t)
#;(define (tree-equiv? bt1 bt2)
    (cond [(and (symbol? bt1) (symbol? bt2)) #f]
          [(and (symbol? bt1) (bt? bt2)) #f]
          [(and (bt? bt1) (symbol? bt2)) #f]
          [(and (bt? bt1) (bt? bt2)) (or (... (first bt1) ... (first bt2)) 
                                         (tree-equiv? ((rest bt1) ...
                                                       (rest bt2))))]))
#|(define (tree-equiv?* t subt)
  (local [(define (tree-equiv? t subt) ;; BinaryTree BinaryTree -> Boolean
            (cond [(and (symbol? t) (symbol? subt)) #t]
                  [(and (symbol? t) (bt? subt)) #f]
                  [(and (bt? t) (symbol? subt)) #f]
                  [(and (bt? t) (bt? subt)) (and (symbol=? (bt-value t) (bt-value subt))
                                                 (tree-equiv? (bt-left t) (bt-left subt))
                                                 (tree-equiv? (bt-right t) (bt-right subt)))]))
          (define (any-subtrees-equiv? t1 t2) ;; BinaryTree BinaryTree -> Boolean
            (or (tree-equiv? t1 t2)
                (tree-equiv? t1 t2]
    (cond [(and (symbol? t) (symbol? subt)) #t]
          [(and (symbol? t) (bt? subt)) #f]
          [(and (bt? t) (symbol? subt)) #t] ;; A leaf is a subtree of any BinaryTree
          [(and (bt? t) (bt? subt)) (and (symbol=? (bt-value t) (bt-value subt))
                                         (or (tree-equiv? (bt-left t) (bt-left subt))
                                             (tree-equiv?|#


;Exercise 6

;find-subtree?: BinaryTree BinaryTree -> Boolean
;does the BinaryTree _tr_ contain the subtree _subtr_?
;(check-expect (find-subtree (make-bt 'a
(check-expect (find-subtree bt-1 (make-bt 'c
                                          (make-bt 'g bt-0 bt-0)
                                          (make-bt 'f bt-0 bt-0))) #f)
(check-expect (find-subtree bt-1 (make-bt 'c
                                          (make-bt 'f bt-0 bt-0)
                                          (make-bt 'g bt-0 bt-0))) #t)
(check-expect (find-subtree bt-1 'none) #t)
(check-expect (find-subtree 'none bt-1) #f)
(check-expect (find-subtree 'none 'none) #t)
;write checks (need more)
#;(define (find-subtree? tr subtr)
    (local [(define (trequal? tr1 tr2) ;; BinaryTree BinaryTree -> Boolean
              (cond [(and (symbol? tr1) (symbol? tr2)) (symbol=? tr1 tr2)]
                    [(and (symbol? tr1) (bt? tr2)) #f]
                    [(and (bt? tr1) (symbol? tr2)) #f]
                    [(and (bt? tr1) (bt? tr2)) (and (symbol=? (bt-value tr1) (bt-value tr2))
                                                    (trequal? (bt-left tr1) (bt-left tr2))
                                                    (trequal? (bt-right tr1) (bt-right tr2)))]))]
      (cond [(and (symbol? tr) (symbol? subtr)) #t]
            [(and (symbol? tr) (bt? subtr)) #f]
            [(and (bt? tr) (symbol? subtr)) #t]
            [(and (bt? tr) (bt? subtr)) (or (trequal? tr subtr)
                                            (trequal? (bt-right tr) subtr)
                                            (trequal? (bt-left tr) subtr))])))

;simplifying:
(define (find-subtree? tr subtr)
  (local [(define (trequal? tr1 tr2) ;; BinaryTree BinaryTree -> Boolean
            (cond [(and (symbol? tr1) (symbol? tr2)) (symbol=? tr1 tr2)]
                  [(and (bt? tr1) (bt? tr2)) (and (symbol=? (bt-value tr1) (bt-value tr2))
                                                  (trequal? (bt-left tr1) (bt-left tr2))
                                                  (trequal? (bt-right tr1) (bt-right tr2)))]
                  [else #f]))]
    (cond [(and (symbol? tr) (bt? subtr)) #f]
          [(and (bt? tr) (bt? subtr)) (or (trequal? tr subtr)
                                          (trequal? (bt-right tr) subtr)
                                          (trequal? (bt-left tr) subtr))]
          [else #t])))


;Exercise 7

;max-common-tree: BinaryTree BinaryTree -> BinaryTree
;produces the largest possible BinaryTree in which each node is a member of _tr1_ and _tr2_
;write checks
(define ex7-t1 (make-bt 'A
                        (make-bt 'B
                                 (make-bt 'D bt-0 bt-0)
                                 (make-bt 'E bt-0 bt-0))
                        (make-bt 'C
                                 (make-bt 'F bt-0 bt-0)
                                 (make-bt 'G bt-0 bt-0))))
(define ex7-t2 (make-bt 'A
                        (make-bt 'B
                                 (make-bt 'D bt-0 bt-0)
                                 (make-bt 'Y bt-0 bt-0))
                        (make-bt 'X
                                 (make-bt 'F bt-0 bt-0)
                                 bt-0)))
(check-expect (max-common-tree ex7-t1 ex7-t2)
              (make-bt 'A
                       (make-bt 'B
                                (make-bt 'D bt-0 bt-0)
                                bt-0)
                       bt-0))
(check-expect (max-common-tree bt-0 ex7-t2) ex7-t2)
(check-expect (max-common-tree ex7-t1 bt-0) ex7-t1)
(check-expect (max-common-tree bt-0 bt-0) bt-0)
(check-expect (max-common-tree ex7-t1 (make-bt 'START ex7-t1 ex7-t1)) 'none)
#;(check-expect (max-common-tree (make-bt 'COMMON-1-1
                                          (make-bt 'COMMON-2-1
                                                   (make-bt 'COMMON-3-1
                                                            (make-bt 'COMMON-4-1)
                                                            (make-bt 'COMMON-4-2))
                                                   (make-bt 'COMMON)))))
(define (max-common-tree tr1 tr2)
  (cond [(and (symbol? tr1) (symbol? tr2)) 'none]
        [(and (symbol? tr1) (bt? tr2)) (make-bt (bt-value tr2)
                                                (max-common-tree tr1 (bt-left tr2))
                                                (max-common-tree tr1 (bt-right tr2)))]
        [(and (bt? tr1) (symbol? tr2)) (make-bt (bt-value tr1)
                                                (max-common-tree (bt-left tr1) tr2)
                                                (max-common-tree (bt-right tr1) tr2))]
        [(and (bt? tr1) (bt? tr2)) (if (symbol=? (bt-value tr1) (bt-value tr2))
                                       (make-bt (bt-value tr1)
                                                (max-common-tree (bt-left tr1) (bt-left tr2))
                                                (max-common-tree (bt-right tr1) (bt-right tr2)))
                                       'none)]))


;Exercise 8

;A BinarySearchTree is one of:
;- 'none
;- (make-bt Number BinarySearchTree BinarySearchTree)

;A BinarySearchTree is a BinaryTree in which for all nodes _n_ whose data is _n'_, nodes left of n
;have data less than n' and all nodes right of n have data greater than n'

;A Dir is one of:
;- 'left
;- 'right

;valid-bst-path?: BinarySearchTree Number [List-of Dir] -> Boolean
;is there a path along the given BinarySearchTree _bst_ to the node containing the data _n_
;if that path is defined by a series of turns at each node _lod_?
;NOTE: assumes _n_ exists in _bst_

;Examples for tests:
(define bst-10 (make-bt 10
                        (make-bt 5
                                 (make-bt 2 'none 'none)
                                 (make-bt 6 'none 'none))
                        (make-bt 15
                                 (make-bt 12 'none 'none)
                                 (make-bt 16 'none 'none))))
;bst-10:
;                    10
;                   /  \
;                  5    15
;                 / \  /  \
;                2  6 12  16

(define bst-ugly (make-bt 20
                          (make-bt 5
                                   (make-bt 2
                                            (make-bt 1 'none 'none)
                                            (make-bt 3 'none 'none))
                                   'none)
                          'none))
;bst-ugly:
;                   20
;                  /
;                 5
;                /
;               2
;              / \
;             1   3
;Null Cases:
(check-expect (valid-bst-path? 'none 12 '()) #f)
(check-expect (valid-bst-path? 'none 12 '(right left)) #f)
(check-expect (valid-bst-path? bst-10 12 '()) #f)
;same cases are trivial for bst-ugly

;_n_ found with "perfect" directions in _lod_:
(check-expect (valid-bst-path? bst-10 12 '(right left)) #t)
(check-expect (valid-bst-path? bst-ugly 3 '(left left right)) #t)
(check-expect (valid-bst-path? bst-ugly 2 '(left left left)) #t)

;_n_ found with "excessive" directions in _lod_:
(check-expect (valid-bst-path? bst-10 6 '(left right left right left left right)) #t)
(check-expect (valid-bst-path? bst-ugly 5 '(left left right left)) #t)

;path does not quite reach _n_:
(check-expect (valid-bst-path? bst-10 16 '(right)) #f)
(check-expect (valid-bst-path? bst-ugly 1 '(left left)) #f)

;path completely misses _n_
(check-expect (valid-bst-path? bst-10 2 '(right right right right)) #f)
(check-expect (valid-bst-path? bst-ugly 3 '(left right right right left)) #f)
(check-expect (valid-bst-path? bst-ugly 2 '(right)) #f)

(define (valid-bst-path? bst n lod)
  (local [(define (dir->bst d bst) ;; Dir BinarySearchTree -> BinarySearchTree
            (cond [(symbol? bst) 'none]
                  [(symbol=? d 'left) (bt-left bst)]
                  [(symbol=? d 'right) (bt-right bst)]))]
    (cond [(and (symbol? bst) (empty? lod)) #f]
          [(and (bt? bst) (empty? lod)) (= (bt-value bst) n)]
          [(and (symbol? bst) (cons? lod)) #f]
          [(and (bt? bst) (cons? lod)) (if (symbol? (dir->bst (first lod) bst))
                                           (= (bt-value bst) n)
                                           (or (= (bt-value bst) n)
                                               (= (bt-value (dir->bst (first lod) bst)) n)
                                               (valid-bst-path? (dir->bst (first lod) bst) n
                                                                (rest lod))))])))


;Exercise 9

;merge: (X) [List-of X] [List-of X] [X X -> Boolean] -> [List-of X]
;merges _lst1_ and _lst2_ into a single list where for each element _elem_ and its successor _succ_
;(lt-func _elem_ _succ_) returns #true and (lt-func _succ_ _elem_) returns #false
(check-expect (merge '() '() <) '())
(check-expect (merge '() (list 1 2 3) (λ (elem succ) (< (modulo elem 5) (modulo succ 5))))
              (list 1 2 3))
(check-expect (merge '()  (list 3 2 1) <)
              (list 1 2 3))
(check-expect (merge (list (list 1 2 3) (list 4 5 6)) '()
                     (λ (elem succ) (< (foldr + 0 elem) (foldr + 0 succ))))
              (list (list 1 2 3) (list 4 5 6)))
(check-expect (merge (list "a" "b" "c") (list "d" "e" "f") string<?)
              (list "a" "b" "c" "d" "e" "f"))
(check-expect (merge (list 1 1 2 2 3 3) (list 3 4 2 5 1 6) <)
              (list 1 1 2 2 3 3 4 5 6))
(check-expect (merge (list (list 'a 'b 'c) (list '1 '2) (list 'foo) (list 'w 'x 'y 'z))
                     (list (list 'foo) (list 'z 'y 'x 'w) (list 'c 'b 'a) (list '2 '1))
                     (λ (elem succ) (< (length elem) (length succ))))
              (list (list 'foo) (list 'foo) (list '1 '2) (list '2 '1) (list 'a 'b 'c) (list 'c 'b 'a)
                    (list 'w 'x 'y 'z) (list 'z 'y 'x 'w)))

(define (merge lst1 lst2 lt-func)
  (cond [(and (empty? lst1) (empty? lst2)) '()]
        [(and (empty? lst1) (cons? lst2)) (my-sort lst2)]
        [(and (cons? lst1) (empty? lst2)) (my-sort lst1)]
        [(and (cons? lst1) (cons? lst2)) (if (lt-func (first lst1) (first lst2))
                                             (cons (first lst1)
                                                   (merge (rest lst1)
                                                          lst2
                                                          lt-func))
                                             (cons (first lst2)
                                                   (merge lst1
                                                          (rest lst2)
                                                          lt-func)))]))
