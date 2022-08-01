#lang racket
(require racket/trace)
(provide execute)

;Combining all lists into one list
(define (join-lst lst [acc null])
  (cond
    ([null? lst] acc)
    ([pair? lst] (join-lst (car lst) (join-lst (cdr lst) acc)))
    (else (cons lst acc))
    ))

#|
========== CREATE MAIN HASH TABLE ==========
I create a hash table where, by the name of the function,
you can refer to its body and its parmaters, if they are present.
|#

(define hash-process (make-hash)) ; save all process for image

(define (create-hash lst)
  (define name (car(second lst))) ;(caadr lst)) ; name 
  (define body (cons (cdr(second lst)) (cddr lst))) ;params and definition body
  (match (car lst)
    ['define (hash-set! hash-process name body)]))

;select value or function and select type of image or return number

#|
========== MATCHING ==========
In the OPERATION function, I pass the sign and compare it with all possible
 mathematical signs. If it doesn’t fit, then I pass the sign to the
SELECT-ELEMENT function, where it can be one of 3 figures or it can be a
variable from the hash table.
|#
(define (select-element expr hash-value)
  (cond

    ;select line 
    ([or (eqv? expr 'line) (and (list? expr) (not (empty? expr)) (eqv? (car expr) 'line)) ]
     (lambda (x1 y1 x2 y2 style) (format "<line x1=\"~a\" y1=\"~a\" x2=\"~a\" y2=\"~a\" style=\"~a\"/>" x1 y1 x2 y2 style)))

    ;select circle 
    ([or (eqv? expr 'circle) (and (list? expr) (not (empty? expr)) (eqv? (car expr) 'circle))]
     (lambda (cx cy r style) (format "<circle cx=\"~a\" cy=\"~a\" r=\"~a\" style=\"~a\"/>" cx cy r style)))

    ;select rectangle
    ([or (eqv? expr 'rect) (and (list? expr) (not (empty? expr)) (eqv? (car expr) 'rect))]
     (lambda (x y width height style) (format "<rect x=\"~a\" y=\"~a\" width=\"~a\" height=\"~a\" style=\"~a\"/>" x y width height style)))

    ;return value
    ([eqv? (not (hash-ref hash-value expr #f))#f]
     (hash-ref hash-value expr))  

    ;return process
    ([eqv? (not (hash-ref hash-process expr #f)) #f]
     (hash-ref hash-process expr)) 

    ;number
    (else expr) ))

(define (operation expr hash-value)
  (match expr
    ['+ +]
    ['- -]
    ['/ /]
    ['* *]
    ['cos cos]
    ['sin sin]
    ['floor floor]
    ['> >]
    ['< <]
    ['= =]
    ['if "if"]
    ['when "when"]
    [symbol? (select-element expr hash-value)]
    [_ expr]
  ))

#|
========== PROCESSING ==========
1) if math process or circle, rect or line -> procedure? -> #t

I receive a line which I want to process. If this is some kind of mathematical
operation -> will apply it to all elements in the given list, which will be
recursively counted if they also have other mathematical operations.

2)if not math process -> procedure? -> #f

Combine the already calculated expression or text with subsequent expressions
that we will calculate recursively.

3)if args not list
return number 
|#
(define (preprocess-line args hash-value)
  (cond

    ([and (list? args) (not (empty? args)) (procedure? (operation (car args) hash-value))]
     (apply (operation (car args) hash-value) (map (lambda (x) (preprocess-line x hash-value)) (cdr args))))

    ([and (list? args) (not (empty? args))]
     (cons (preprocess-line (car args) hash-value) (map (lambda (x) (preprocess-line x hash-value)) (cdr args))))

    (else (operation args hash-value))
    ))
#|
========== PROCESSING INSTRUCTION ==========
In the eval-prg, we immediately determine the type of operation,
because the type of operation will change during recursive passage.

1) If oper is list ->
I create a hash table of variables for a function. And in each iteration I write
down new values there. And I continue to go down the line of definitions further.

2) If oper is procedure ->
Just a math operation
|#
(define (eval-prg args [value-table hash])
  (define oper (operation (car args) value-table))
  
  (cond
    ([list? oper]

     (define hash-value (make-hash))

     ;create hash
     (map (lambda (x y) (hash-set! hash-value x y))
          (car oper) (preprocess-line (cdr args) value-table))

     ;continue work
     (map (lambda (x) (eval-prg x hash-value)) (cdr oper)))

    ([procedure? oper] (preprocess-line args value-table))

    ;true
    ([and (eqv? oper "if") (preprocess-line (cadr args) value-table)]
     (eval-prg (caddr args) value-table) )

    ;false
    ([and (eqv? oper "if") (not (preprocess-line (cadr args) value-table))]
     (eval-prg (car (reverse args)) value-table))

    ;true
    ([and (eqv? oper "when") (preprocess-line (cadr args) value-table)]
     (map (lambda (x) (eval-prg x value-table)) (cddr args)))

    ;false
    ([and (eqv? oper "when") (not (preprocess-line (cadr args) value-table))] "") 
    ))

#|
========== MAIN FUNCTION ==========
1) Creates a hash table of all the definitions that I go through for each word, number, sign.
2) I count the values for the first line in SVG and get a string.
3) From the final result I get a list with lists and combine it into one list.
|#
(define (execute width height prg expr)
  (map (lambda (x) (create-hash x)) prg)
  
  (define result (format "<svg width=\"~a\" height=\"~a\">" (preprocess-line width hash-process) (preprocess-line height hash-process)))
  (string-append (string-append result (string-join (join-lst (eval-prg expr hash-process)) "")) "</svg>"))
