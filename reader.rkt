#lang racket/base

(require racket/string
         "types.rkt")

(provide arc-read
         arc-read-expr
         arc-read-all
         arc-read-from-string
         arc-read-all-from-string
         arc-readtable
         expand-arc-sugars
         (struct-out exn:fail:arc-incomplete)
         (struct-out exn:fail:arc-syntax))

(struct exn:fail:arc-incomplete exn:fail () #:transparent)
(struct exn:fail:arc-syntax exn:fail (expr) #:transparent)

(define (raise-arc-syntax msg [expr 'nil])
  (raise (exn:fail:arc-syntax (format "Syntax error: ~a" msg) (current-continuation-marks) expr)))

(define (raise-arc-incomplete msg)
  (raise (exn:fail:arc-incomplete (format "Incomplete expression: ~a" msg) (current-continuation-marks))))

;; Racket readtable macro for [...] bracket syntax
(define arc-readtable
  (make-readtable (current-readtable)
    #\[ 'terminating-macro
    (lambda (ch port src line col pos)
      (let loop ([items '()])
        (let ([c (peek-char port)])
          (cond
            [(eof-object? c) (raise-arc-incomplete "Unclosed bracket")]
            [(char=? c #\])
             (read-char port)
             (let ([body (reverse items)])
               (list 'fn '(_) (if (null? body) 'nil body)))]
            [(char-whitespace? c)
             (read-char port)
             (loop items)]
            [else
             (let ([item (read/recursive port #f arc-readtable)])
               (loop (cons item items)))]))))
    #\] 'terminating-macro
    (lambda (ch port src line col pos)
      (raise-arc-syntax "Unexpected ']'"))))

;; Syntax sugar transformer for ., !, :, ~
(define (expand-arc-sugars s)
  (cond
    [(null? s) 'nil]
    [(symbol? s)
     (let* ([str (symbol->string s)]
            [len (string-length str)])
       (cond
         [(string=? str "nil") 'nil]
         [(string=? str ".") (string->symbol ".")]
         [else
          (let check-infix ([i (- len 2)])
            (if (>= i 1)
                (let ([c (string-ref str i)])
                  (cond
                    [(char=? c #\.)
                     (let ([left (expand-arc-sugars (string->symbol (substring str 0 i)))]
                           [right (expand-arc-sugars (string->symbol (substring str (+ i 1))))])
                       (arc-cons left (arc-cons right 'nil)))]
                    [(char=? c #\!)
                     (let ([left (expand-arc-sugars (string->symbol (substring str 0 i)))]
                           [right (expand-arc-sugars (string->symbol (substring str (+ i 1))))])
                       (arc-cons left (arc-cons (arc-cons 'quote (arc-cons right 'nil)) 'nil)))]
                    [(char=? c #\:)
                     (let ([left (expand-arc-sugars (string->symbol (substring str 0 i)))]
                           [right (expand-arc-sugars (string->symbol (substring str (+ i 1))))])
                       (arc-cons 'compose (arc-cons left (arc-cons right 'nil))))]
                    [else
                     (check-infix (- i 1))]))
                (if (and (>= len 2) (char=? (string-ref str 0) #\~))
                    (arc-cons 'complement (arc-cons (expand-arc-sugars (string->symbol (substring str 1))) 'nil))
                    s)))]))]
    [(pair? s)
     (arc-cons (expand-arc-sugars (car s))
               (expand-arc-sugars (cdr s)))]
    [else s]))

(define (skip-whitespace-and-bom port)
  (let loop ()
    (let ([c (peek-char port)])
      (when (and (not (eof-object? c))
                 (or (char-whitespace? c) (char=? c #\uFEFF)))
        (read-char port)
        (loop)))))

(define (arc-read-expr [port (current-input-port)])
  (skip-whitespace-and-bom port)
  (parameterize ([current-readtable arc-readtable])
    (with-handlers ([exn:fail:read:eof? (lambda (e) (raise-arc-incomplete (exn-message e)))]
                    [exn:fail:read? (lambda (e) (raise-arc-syntax (exn-message e)))])
      (let ([raw (read port)])
        (if (eof-object? raw)
            eof
            (expand-arc-sugars raw))))))

(define (arc-read [port (current-input-port)] [eof-val 'nil])
  (let ([res (arc-read-expr port)])
    (if (eof-object? res)
        eof-val
        res)))

(define (arc-read-all [port (current-input-port)])
  (let loop ([acc '()])
    (let ([res (arc-read-expr port)])
      (if (eof-object? res)
          (reverse acc)
          (loop (cons res acc))))))

(define (arc-read-from-string str [eof-val 'nil])
  (let ([p (open-input-string str)])
    (arc-read p eof-val)))

(define (arc-read-all-from-string str)
  (let ([p (open-input-string str)])
    (arc-read-all p)))
