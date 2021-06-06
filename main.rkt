#!/usr/bin/env racket
#lang racket

; please don't judge too harshly...i am terrible at writing scheme...

; reads a line from the file
(define (read-row-from file) 
    (do ((ch (read-char file) (read-char file))
         (acc '() (cons ch acc)))
        ((or (eof-object? ch) (eq? ch #\newline)) (list->vector (reverse acc)))))

; returns a vector of vectors representing the lines of the source file
(define (matrix-from port)
    (define lines 
        (do ((acc '() (cons (read-row-from port) acc)))
            ((eof-object? (peek-char port)) (list->vector (reverse acc)))))
    lines)

; returns the character at the provided x and y in the matrix, or #f if out of bounds
(define (matrix-at matrix x y)
    (if (or (< y 0) (>= y (vector-length matrix)))
        #f
        (let ((line (vector-ref matrix y)))
            (if (or (< x 0) (>= x (vector-length line)))
                #f
                (vector-ref line x)))))

; skips whitespace horizontally, returning first non-whitespace x
(define (skip-space-horiz matrix x y)
    (let ((ch (matrix-at matrix x y)))
        (if (or (not ch) (not (char-whitespace? ch))) 
            x
            (skip-space-horiz matrix (+ x 1) y))))

; skips whitespace vertically, returning first non-whitespace y
(define (skip-space-vert matrix x y)
    (let ((ch (matrix-at matrix x y)))
        (if (or (and (not ch) (< y (vector-length matrix))) (char-whitespace? ch)) ; if we're past the end of a line that's ok
            (skip-space-vert matrix x (+ y 1))
            y)))

; returns a pair (expr . pos), containing both a single parameter string as well as the next x position
(define (read-vert-param matrix x y)
    (cond 
        ((eq? (matrix-at matrix (- x 1) y) #\<) (cons (read-horiz matrix (- x 1) y) (+ y 1))) ; parse horizontal expression
        (else (let ((acc ""))
                (do ((i y (+ i 1))) ; iterate until next space/bracket/newline/eof
                  ((let ((ch (matrix-at matrix x i)))
                    (or (not ch) ; stop looping if we go out of bounds
                        (char-whitespace? ch) ; ...or if we hit a space
                        (eq? ch #\v))) ; ...or if we hit a closing bracket
                    (cons acc i))
                  (set! acc (string-append acc (string (matrix-at matrix x i)))))))))

; returns a horizontal expression starting from the provided (x, y) position, or #f if any errors occur
(define (read-vert matrix x y)
    (if (eq? (matrix-at matrix x y) #\^)
        (let ((expr ""))  ; read list
            (do ((param 
                    (read-vert-param matrix x (skip-space-vert matrix x (+ y 1))) 
                    (read-vert-param matrix x (skip-space-vert matrix x (cdr param)))))
                ((or (not (car param)) (= 0 (string-length (car param)))) 
                 (string-append "(" expr ")"))
                (set! expr (string-append expr " " (car param)))))
        (car (read-vert-param matrix x (skip-space-vert matrix x y))))) ; read single parameter

; returns a pair (expr . pos), containing both a single parameter string as well as the next x position
(define (read-horiz-param matrix x y)
    (cond 
        ((eq? (matrix-at matrix x (- y 1)) #\^) (cons (read-vert matrix x (- y 1)) (+ x 1))) ; parse vertical expression
        (else (let ((acc ""))
                (do ((i x (+ i 1))) ; iterate until next space/bracket/newline/eof
                  ((let ((ch (matrix-at matrix i y)))
                    (or (not ch) ; stop looping if we go out of bounds
                        (char-whitespace? ch) ; ...or if we hit a space
                        (eq? ch #\>))) ; ...or if we hit a closing bracket
                    (cons acc i))
                  (set! acc (string-append acc (string (matrix-at matrix i y)))))))))

; returns a horizontal expression starting from the provided (x, y) position, or #f if any errors occur
(define (read-horiz matrix x y)
    (if (eq? (matrix-at matrix x y) #\<)
        (let ((expr ""))  ; read list
            (do ((param 
                    (read-horiz-param matrix (skip-space-horiz matrix (+ x 1) y) y) 
                    (read-horiz-param matrix (skip-space-horiz matrix (cdr param) y) y)))
                ((or (not (car param)) (= 0 (string-length (car param)))) 
                 (string-append "(" expr ")"))
                (set! expr (string-append expr " " (car param)))))
        (car (read-horiz-param matrix (skip-space-horiz matrix x y) y)))) ; read single parameter

; returns scheme code parsed from the provided matrix, or #f if any errors occur
(define (parse-matrix matrix)
    (do ((i 0 (+ i 1))
         (exprs '()
            (if (eq? (matrix-at matrix 0 i) #\<) 
                (let ([expr (read-horiz matrix 0 i)])
                  (if (string=? expr "()") exprs (cons expr exprs)))
                exprs)))
        ((= i (vector-length matrix)) 
         (string-join (reverse exprs) "\n"))))

(module* reader #f
  (require syntax/module-reader)
  (provide (rename-out [read-syntax-scheme2d read-syntax]
                       [read-scheme2d read]
                       [get-info-scheme2d get-info]))
  (define-values (read-scheme2d read-syntax-scheme2d get-info-scheme2d)
    (make-meta-reader
     'scheme2d
     "language path"
     lang-reader-module-paths
     (lambda (read-fn)
       (lambda ([in (current-input-port)])
         (read-fn (open-input-string (parse-matrix (matrix-from in))))))
     (lambda (read-fn)
       (lambda (src in . args)
         (apply read-fn src (open-input-string (parse-matrix (matrix-from in))) args)))
     identity)))
