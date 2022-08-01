#lang racket

(require 2htdp/image)
(require racket/trace)

(provide img->mat ascii-art)

;(define img (bitmap "doge.png"))

(define chars " .,:;ox%#@")

(define (RGB->grayscale color)
  (+ (* 0.3 (color-red color))
     (* 0.59 (color-green color))
     (* 0.11 (color-blue color)))
  )

;(define lst-img (map RGB->grayscale (image->color-list img)))

;I get the matrix of the picture
(define (img->mat img [lst '()] [result '()] [between-result '()])
  (cond

    ([= (length between-result) (image-width img)] (img->mat img lst (cons between-result result) '()))
    ([and (null? lst) (null? between-result)] (map reverse (reverse result)))
    
    ([not (= (length between-result) (image-width img))] (img->mat img (cdr lst) result (cons (car lst) between-result)))
    ;([null? lst] '())
    )
  )

;I cut the matrix to fit the block size
(define (cut-mat width height mat)
  (cond
    ((null? mat) '())
    ([not (= (remainder (length (last mat)) width) 0)] (cut-mat width height (map reverse(map cdr (map reverse mat))))) ; width
    ([not (= (remainder (length mat ) height) 0)] (cut-mat width height (reverse(cdr(reverse mat))))) ; height
    (#t mat)
    )
  )

;I count the amount in a small block
(define (sum-num mat )
  (map (lambda (y) (apply + y)) mat)
  )

;Dividing a matrix into blocks
(define (split-block width lst [result '()])
  (if (null? lst) (reverse result) (split-block width (drop lst width) (cons (take lst width) result))) 
  )

;I calculate the sum of numbers by the height of the block
(define (sum-height height mat)
  (apply map + (take mat height))
  )
;I get a ready block with the sum of all the numbers in the block
(define (between-mat width height mat) (sum-num (split-block width (sum-height height mat) )))

;I get a matrix with the sum of all blocks
(define (mat-result width height mat [result '()] )
  (cond
    ([null? mat] '())
    ([= (length mat) height] (reverse(cons (between-mat width height mat) result)))
    (#t (mat-result width height (drop mat height) (cons (between-mat width height mat) result)))
    )
  ;(if (= (length mat) height) (reverse(cons (between-mat width height mat) result)) (mat-result width height (drop mat height) (cons (between-mat width height mat) result)))
  )

;Calculate the average of each number
(define (my-average width height mat [result '()])
  (if (null? mat) (reverse result) (my-average width height (cdr mat) (cons (map (lambda (x) (/ x (* width height) ) ) (car mat)) result)))
  )

;Substitute the number in the formula to calculate the index
(define (formula-index num )
  (if (not (number? num)) -1 (exact-floor(floor(/ (* (string-length chars) (- 255 (floor num))) 256))))
  )

;I get an index based on a number from a formula with which I will get a char
(define (get-index chars mat)
  ;(display mat)
  (map (lambda (x) (map formula-index x ))mat)
  )

;Get sign based on each index in matrix
(define (get-char mat chars)
  (map (lambda (x) (map (lambda (y) (list-ref(string->list chars) y)) x) )mat)
  )

;I translate the entire matrix into a list of all string
(define (mat->string mat)
  (map (lambda (x) (list->string x) ) mat)
  )

;Add a newline between each line
(define (string-result lst)
  (if (null? lst) "" (string-append (string-join lst "\n") "\n"))
  )

;I write out the finished picture
(define (ascii-art width height chars)
  
  (lambda (img)
    (define lst-img (map RGB->grayscale (image->color-list img)))
    
    (string-result
     (mat->string
      (get-char
       (get-index chars
        (my-average width height
                    (mat-result width height
                                (cut-mat width height
                                         (img->mat img lst-img ))))) chars))))
  
  )

;(display ((ascii-art 7 14 " .,:;ox%#@") (bitmap "doge.png")))