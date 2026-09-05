#lang racket/base

(require "arc.rkt")

(define VERSION "1.0")

(define (print-help)
  (displayln "Usage: arc [OPTIONS...] [FILES...]")
  (newline)
  (displayln "OPTIONS:")
  (displayln "    -h    print this screen.")
  (displayln "    -v    print version."))

(define (main)
  (define args (current-command-line-arguments))
  (cond
    [(= (vector-length args) 0)
     ;; Interactive REPL
     (define env (arc-init #t))
     (arc-repl env)]
    [(and (= (vector-length args) 1)
          (or (string=? (vector-ref args 0) "-h")
              (string=? (vector-ref args 0) "--help")))
     (print-help)]
    [(and (= (vector-length args) 1)
          (or (string=? (vector-ref args 0) "-v")
              (string=? (vector-ref args 0) "--version")))
     (displayln VERSION)]
    [(and (>= (vector-length args) 2)
          (string=? (vector-ref args 0) "-e"))
     (define env (arc-init #t))
     (with-handlers
         ([exn:fail?
           (lambda (e)
             (eprintf "Evaluation error:\n~a\n" (exn-message e))
             (exit 1))])
       (define res (arc-eval-string (vector-ref args 1) env))
       (displayln (arc-to-string res 1)))]

    [else
     ;; Execute files sequentially
     (define env (arc-init #t))
     (for ([arg (in-vector args)])
       (with-handlers
           ([exn:fail?
             (lambda (e)
               (eprintf "In file ~a:\n~a\n" arg (exn-message e))
               (exit 1))])
         (arc-load-file arg env)))]))

(main)
