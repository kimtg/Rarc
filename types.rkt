#lang racket/base

(require racket/string
         racket/format)

(provide arc-nil
         arc-t
         arc-nil?
         arc-true?
         arc-false?
         arc-no
         arc-boolean?
         
         (struct-out acons)
         arc-cons
         arc-car
         arc-cdr
         arc-scar!
         arc-scdr!
         arc-list?
         arc-len
         racket-list->arc-list
         arc-list->racket-list
         arc-list->vector
         
         (struct-out arc-table)
         make-arc-table
         
         (struct-out arc-closure)
         (struct-out arc-macro)
         (struct-out arc-cont)
         (struct-out arc-pipe)
         
         arc-is
         arc-iso
         arc-to-string
         arc-disp
         arc-write)

;; Constant symbols
(define arc-nil 'nil)
(define arc-t 't)

(define (arc-nil? x)
  (eq? x 'nil))

(define (arc-true? x)
  (not (eq? x 'nil)))

(define (arc-false? x)
  (eq? x 'nil))

(define (arc-no x)
  (if (eq? x 'nil) 't 'nil))

(define (arc-boolean? x)
  (or (eq? x 't) (eq? x 'nil)))

;; Cons cell definition with structural equality for hash tables (matching Arc's iso)
(define (acons-equal? a b recur)
  (and (recur (acons-car a) (acons-car b))
       (recur (acons-cdr a) (acons-cdr b))))

(define (acons-hash a recur)
  (let loop ([curr a] [h 1])
    (cond
      [(acons? curr)
       (loop (acons-cdr curr)
             (bitwise-and (+ (* h 31) (recur (acons-car curr))) #x3FFFFFFF))]
      [(eq? curr 'nil) h]
      [else
       (bitwise-and (+ (* h 31) (recur curr)) #x3FFFFFFF)])))

(struct acons ([car #:mutable] [cdr #:mutable])
  #:transparent
  #:property prop:equal+hash
  (list acons-equal? acons-hash acons-hash))

(define (arc-cons a b)
  (acons a b))

(define (arc-car x)
  (cond
    [(acons? x) (acons-car x)]
    [(eq? x 'nil) 'nil]
    [else (error 'car "Wrong type: ~a" x)]))

(define (arc-cdr x)
  (cond
    [(acons? x) (acons-cdr x)]
    [(eq? x 'nil) 'nil]
    [else (error 'cdr "Wrong type: ~a" x)]))

(define (arc-scar! c v)
  (if (acons? c)
      (begin (set-acons-car! c v) v)
      (error 'scar "Wrong type: ~a" c)))

(define (arc-scdr! c v)
  (if (acons? c)
      (begin (set-acons-cdr! c v) v)
      (error 'scdr "Wrong type: ~a" c)))

;; Checks if expr is a proper list ending in nil
(define (arc-list? expr)
  (let loop ([a expr])
    (cond
      [(eq? a 'nil) #t]
      [(acons? a) (loop (acons-cdr a))]
      [else #f])))

;; Length function matching len in arc.cpp
(define (arc-len x)
  (cond
    [(string? x) (string-length x)]
    [(arc-table? x) (hash-count (arc-table-hash x))]
    [(acons? x)
     (let loop ([curr x] [n 0])
       (cond
         [(acons? curr) (loop (acons-cdr curr) (+ n 1))]
         [else n]))]
    [(eq? x 'nil) 0]
    [else 0]))

;; Conversions
(define (racket-list->arc-list lst)
  (if (null? lst)
      'nil
      (acons (car lst) (racket-list->arc-list (cdr lst)))))

(define (arc-list->racket-list a)
  (let loop ([curr a])
    (cond
      [(eq? curr 'nil) '()]
      [(acons? curr) (cons (acons-car curr) (loop (acons-cdr curr)))]
      [else (error 'arc-list->racket-list "Improper list: ~a" a)])))

(define (arc-list->vector a)
  (let loop ([curr a] [acc '()])
    (cond
      [(eq? curr 'nil) (list->vector (reverse acc))]
      [(acons? curr) (loop (acons-cdr curr) (cons (acons-car curr) acc))]
      [else (list->vector (reverse (cons curr acc)))])))

;; Tables
(struct arc-table (hash) #:transparent)

(define (make-arc-table)
  (arc-table (make-hash)))

;; Closures, macros, continuations, pipes
(struct arc-closure (env args body [name #:mutable]) #:transparent)
(struct arc-macro (closure) #:transparent)
(struct arc-cont (proc) #:transparent)
(struct arc-pipe (in-port subproc) #:transparent)

;; Arc pointer equality (is)
(define (arc-is a b)
  (cond
    [(and (eq? a 'nil) (eq? b 'nil)) #t]
    [(and (acons? a) (acons? b)) (eq? a b)]
    [(and (arc-closure? a) (arc-closure? b)) (eq? a b)]
    [(and (arc-macro? a) (arc-macro? b)) (eq? a b)]
    [(and (arc-table? a) (arc-table? b)) (eq? a b)]
    [(and (arc-cont? a) (arc-cont? b)) (eq? a b)]
    [(and (arc-pipe? a) (arc-pipe? b)) (eq? a b)]
    [(and (symbol? a) (symbol? b)) (eq? a b)]
    [(and (number? a) (number? b)) (= a b)]
    [(and (string? a) (string? b)) (string=? a b)]
    [(and (char? a) (char? b)) (char=? a b)]
    [(and (procedure? a) (procedure? b)) (eq? a b)]
    [(and (port? a) (port? b)) (eq? a b)]
    [else #f]))

;; Arc structural equality (iso)
(define (arc-iso a b)
  (cond
    [(and (acons? a) (acons? b))
     (and (arc-iso (acons-car a) (acons-car b))
          (arc-iso (acons-cdr a) (acons-cdr b)))]
    [else (arc-is a b)]))

;; Formatting
(define (escape-string str)
  (let ([p (open-output-string)])
    (for ([c (in-string str)])
      (case c
        [(#\newline) (display "\\n" p)]
        [(#\return) (display "\\r" p)]
        [(#\tab) (display "\\t" p)]
        [(#\\) (display "\\\\" p)]
        [(#\") (display "\\\"" p)]
        [else (write-char c p)]))
    (get-output-string p)))

(define (format-cons a write-mode)
  (let ([out (open-output-string)])
    (display "(" out)
    (display (arc-to-string (acons-car a) write-mode) out)
    (let loop ([curr (acons-cdr a)])
      (cond
        [(eq? curr 'nil)
         (display ")" out)]
        [(acons? curr)
         (display " " out)
         (display (arc-to-string (acons-car curr) write-mode) out)
         (loop (acons-cdr curr))]
        [else
         (display " . " out)
         (display (arc-to-string curr write-mode) out)
         (display ")" out)]))
    (get-output-string out)))

(define (arc-to-string a [write-mode 1])
  (cond
    [(eq? a 'nil) "nil"]
    [(acons? a)
     ;; Check for quotes formatting: '(...) `(...) ,(...) ,@(...)
     (if (and (arc-list? a) (= (arc-len a) 2))
         (let ([head (acons-car a)]
               [second (acons-car (acons-cdr a))])
           (cond
             [(eq? head 'quote)
              (string-append "'" (arc-to-string second write-mode))]
             [(eq? head 'quasiquote)
              (string-append "`" (arc-to-string second write-mode))]
             [(eq? head 'unquote)
              (string-append "," (arc-to-string second write-mode))]
             [(eq? head 'unquote-splicing)
              (string-append ",@" (arc-to-string second write-mode))]
             [else (format-cons a write-mode)]))
         (format-cons a write-mode))]
    [(symbol? a) (symbol->string a)]
    [(string? a)
     (if (= write-mode 1)
         (string-append "\"" (escape-string a) "\"")
         a)]
    [(number? a)
     (if (exact-integer? a)
         (number->string a)
         (number->string (exact->inexact a)))]
    [(char? a)
     (if (= write-mode 1)
         (case a
           [(#\nul) "#\\nul"]
           [(#\return) "#\\return"]
           [(#\newline) "#\\newline"]
           [(#\tab) "#\\tab"]
           [(#\space) "#\\space"]
           [else (string-append "#\\" (string a))])
         (string a))]
    [(arc-closure? a)
     (let ([cls a])
       (string-append "#<closure>(fn "
                      (arc-to-string (arc-closure-args cls) 1)
                      " "
                      (arc-to-string (arc-closure-body cls) 1)
                      ")"))]
    [(arc-macro? a)
     (let ([cls (arc-macro-closure a)])
       (string-append "#<macro:"
                      (arc-to-string (arc-closure-args cls) write-mode)
                      " "
                      (arc-to-string (arc-closure-body cls) write-mode)
                      ">"))]
    [(arc-table? a)
     (let* ([entries (for/list ([(k v) (in-hash (arc-table-hash a))])
                       (string-append "("
                                      (arc-to-string k write-mode)
                                      " . "
                                      (arc-to-string v write-mode)
                                      ")"))]
            [inner (string-join entries " ")])
       (string-append "#<table:(" inner ")>"))]
    [(arc-cont? a) "#<continuation>"]
    [(arc-pipe? a) "#<input-pipe>"]
    [(input-port? a) "#<input>"]
    [(output-port? a) "#<output>"]
    [(procedure? a) "#<procedure>"]
    [else (format "~a" a)]))

(define (arc-disp a [port (current-output-port)])
  (display (arc-to-string a 0) port))

(define (arc-write a [port (current-output-port)])
  (display (arc-to-string a 1) port))
