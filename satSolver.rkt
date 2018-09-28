#lang racket
(require "utilities.rkt")


(define assign #hash())

; Fill in your code here. Should finally define a function
; called dpll which returns true or false. Should additionally
; store the satisfying assignment in the variable assign.

(define (dpll tr)
  (define corrlist (trtolist tr))
  (define temp (dpll-h corrlist #hash()))
  (cond [(equal? temp #hash())
         (set! assign temp)
         #f]
        [else (set! assign temp)
              #t]))
(define (pseudo-assign e hl)
  (cond [(> e 0)
         (dict-set hl e #t)]
        [else
         (dict-set hl (- e) #f)]))
(define (dpll-h es hashlist)
  (define (assigner e)
    (cond [(> e 0)
           (set! hashlist (dict-set hashlist e #t))]
          [else
           (set! hashlist (dict-set hashlist (- e) #f))]))
  (define (unit-propagate lol)
    (define pl lol)
    (define (uph lol)
      (cond [(null? lol) pl]
            [(= (length (car lol)) 1)
             (assigner (caar lol))
             (operate pl (caar lol))]
            [else (uph (cdr lol))]))
    (uph lol))
  (define (literal-eliminate lol)
    (define temp (gewsp lol))
    (cond [(number? temp)
           (assigner temp)
           (operate lol temp)]
          [else lol]))
  (define (gewsp lol)
    (define check-list '())
    (define (gewsp-h ll)
      (cond [(null? ll) ll]
            [(null? (car ll)) (gewsp-h (cdr ll))]
            [(or (present? (caar ll) check-list) (present? (- (caar ll)) check-list))
             (gewsp-h (cons (cdar ll) (cdr ll)))]
            [else
             (cond [(same-polarity? (caar ll) ll)
                    (caar ll)]
                   [else
                    (set! check-list (cons (caar ll) check-list))
                    (gewsp-h (cons (cdar ll) (cdr ll)))])]))
    (gewsp-h lol))
  (define (same-polarity? e ll)
    (cond [(null? ll) #t]
          [else
           (and (not (present? (- e) (car ll))) (same-polarity? e (cdr ll)))]))
  (define (operate ll e)
    (cond [(null? ll) ll]
          [(present? e (car ll))
           (operate (cdr ll) e)]
          [(present? (- e) (car ll))
           (cons (remove (- e) (car ll)) (operate (cdr ll) e))]
          [else (cons (car ll) (operate (cdr ll) e))])) 
           
  
  (set! es (unit-propagate es))
  
  
  (set! es (literal-eliminate es))
  
  
  (cond [(null? es) hashlist]
        [(nullpresent? es) #hash()]
        [else
         
         
         (define f (first-element es))
         
         (define A (dpll-h (operate es f)
                           (pseudo-assign f hashlist)))
         (cond [(equal? #hash() A)
                (dpll-h (operate es (- f)) (pseudo-assign (- f) hashlist))]
               [else A])]))
(define (present? e l)
  (cond [(null? l) #f]
        [(= e (car l)) #t]
        [else (present? e (cdr l))]))
(define (nullpresent? l)
  (cond [(null? l) #f]
        [(equal? '() (car l)) #t]
        [else (nullpresent? (cdr l))]))
(define (first-element es)
  (cond [(null? (car es)) (first-element (cdr es))]
        [else (caar es)]))
(define (trtolist tr)
  (cond [(And? tr)
         (cons (convert-element (And-x tr))
               (trtolist (And-y tr)))]
        [else (list (convert-element tr))]))
(define (convert-element e)
  (cond [(Or? e) (cons (val (Or-x e)) (convert-element (Or-y e)))]
        [else (list (val e))]))
(define (literal? e)
  (or (Var? e) (Not? e)))

(define (val lit)
  (cond [(Var? lit) (Var-lit lit)]
        [else (- (Var-lit (Not-e lit)))]))
(define (new-assigner lit)
  
  (cond [(Var? lit)
         (set! assign (dict-set assign (Var-lit lit) #t))]
        [else
         (set! assign (dict-set assign (Var-lit (Not-e lit)) #f))
         ]))
