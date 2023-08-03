#lang racket

(require redex)

(define-language
  ListMachine
  (l ::= string)
  (a ::= natural nil (cons a a))
  (v ::= string)
  (r ::= ((v a) ...))
  (i ::=
     (jump l)
     (branch-if-nil v l)
     (fetch-field v natural v)
     (cons v v v)
     halt)
  (il ::= (i ...))
  (p ::= natural)
  (b ::= (l il))
  (blist ::= (b ...))
  (state ::= (blist r l p)))

(define ->e
  (reduction-relation
   ListMachine
   #:domain state
   (--> (blist r l_10 p_10)
        (blist r l_1 0)
        (where (l_10 il_10) (fetch-b blist l_10))
        (where (jump l_1) (fetch-i il_10 p_10))
        "jump")

   (--> (blist r l_10 p_10)
        (blist r l_1 0)
        (where (l_10 il_10) (fetch-b blist l_10))
        (where (branch-if-nil v_1 l_1) (fetch-i il_10 p_10))
        (where nil (fetch-value v_1 r))
        "branch-if-nil-yes")
   
   (--> (blist r l_10 p_10)
        (blist r l_10 ,(add1 (term p_10)))
        (where (l_10 il_10) (fetch-b blist l_10))
        (where (branch-if-nil v_1 l_1) (fetch-i il_10 p_10))
        (side-condition (not (term (check-nil (fetch-value v_1 r)))))
        "branch-if-nil-false")

   (--> (blist r l_10 p_10)
        (blist (set-value v_2 a_1 r) l_10 ,(add1 (term p_10)))
        (where (l_10 il_10) (fetch-b blist l_10))
        (where (fetch-field v_1 0 v_2) (fetch-i il_10 p_10))
        (where (cons a_1 a_2) (fetch-value v_1 r))
        "fetch-field-0")

   (--> (blist r l_10 p_10)
        (blist (set-value v_2 a_2 r) l_10 ,(add1 (term p_10)))
        (where (l_10 il_10) (fetch-b blist l_10))
        (where (fetch-field v_1 1 v_2) (fetch-i il_10 p_10))
        (where (cons a_1 a_2) (fetch-value v_1 r))
        "fetch-field-1")

   (--> (blist r l_10 p_10)
        (blist (set-value v_3 (cons (fetch-value v_1 r) (fetch-value v_2 r)) r) l_10 ,(add1 (term p_10)))
        (where (l_10 il_10) (fetch-b blist l_10))
        (where (cons v_1 v_2 v_3) (fetch-i il_10 p_10))
        "cons")
   ))

(define-metafunction ListMachine
  fetch-b : blist l -> b
  [(fetch-b ((l_1 il_1) (l_2 il_2) ...) l_1) (l_1 il_1)]
  [(fetch-b ((l_1 il_1) (l_2 il_2) ...) l_3) (fetch-b ((l_2 il_2) ...) l_3)])
  
(define-metafunction ListMachine
  fetch-i : il p -> i
  [(fetch-i (i ...) p) ,(list-ref (term (i ...)) (term p))])

(define-metafunction ListMachine
  check-nil : a -> boolean
  [(check-nil nil) #t]
  [(check-nil _) #f])

(define-metafunction ListMachine
  fetch-value : v r -> a
  [(fetch-value v_1 ((v_1 a_1) (v_2 a_2) ...)) a_1]
  [(fetch-value v ((v_1 a_1) (v_2 a_2) ...)) (fetch-value v ((v_2 a_2) ...))]
  [(fetch-value v ()) nil])

(define-metafunction ListMachine
  set-value : v a r -> r
  [(set-value v_2 a_4 ((v_1 a_1) ... (v_2 a_2) (v_3 a_3) ...)) ((v_1 a_1) ... (v_2 a_4) (v_3 a_3) ...)]
  [(set-value v_1 a_1 ((v_2 a_2) ...)) ((v_2 a_2) ... (v_1 a_1))])

(traces ->e
        (term ((
                ("L0" ((cons "v0" "v0" "v1") (cons "v0" "v_1" "v_1") (cons "v0" "v_1" "v_1") (jump "L1")))
                ("L1" ((branch-if-nil "v1" "L2") (fetch-field "v1" 1 "v1") (branch-if-nil "v0" "L1") (jump "L2")))
                ("L2" (halt)))
              (("v0" nil))
              "L0"
              0)))
