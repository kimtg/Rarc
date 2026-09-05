#lang racket/base

(require racket/file
         racket/port
         racket/string
         "types.rkt"
         "reader.rkt"
         "eval.rkt"
         "builtins.rkt")

(provide arc-init
         arc-eval-string
         arc-load-file
         arc-repl
         current-arc-env
         arc-to-string)


(define current-arc-env (make-parameter #f))

(define (arc-load-file path [env (or (current-arc-env) (current-arc-namespace))])
  (unless (file-exists? path)
    (error 'arc-load-file "File not found: ~a" path))
  (let ([content (file->string path)])
    (arc-eval-string content env)))

(define (arc-eval-string str [env (or (current-arc-env) (current-arc-namespace))])
  (let ([p (open-input-string str)])
    (let loop ([last-result 'nil])
      (let ([expr (arc-read-expr p)])
        (if (eof-object? expr)
            last-result
            (let ([res (arc-eval expr env)])
              (loop res)))))))

(define (arc-init [load-stdlib? #t])
  (define ns (make-arc-namespace (lambda (path) (arc-load-file path ns))))
  (current-arc-env ns)
  (current-arc-namespace ns)
  (when load-stdlib?
    (let* ([this-dir (or (current-load-relative-directory) (current-directory))]
           [lib-path (build-path this-dir "library.arc")])
      (if (file-exists? lib-path)
          (arc-load-file (path->string lib-path) ns)
          (error 'arc-init "Standard library file library.arc not found"))))
  ns)

(define (arc-repl [env (or (current-arc-env) (arc-init))])
  (printf "Arc Lisp (Racket) 1.0\n")
  (let loop ()
    (display "> ")
    (flush-output)
    (let read-input ([accum ""])
      (let ([line (read-line (current-input-port) 'any)])
        (cond
          [(eof-object? line)
           (newline)]
          [else
           (let ([full-input (if (string=? accum "") line (string-append accum "\n" line))])
             (with-handlers
                 ([exn:fail:arc-incomplete?
                   (lambda (e)
                     (display "  ")
                     (flush-output)
                     (read-input full-input))]
                  [exn:fail?
                   (lambda (e)
                     (eprintf "Error: ~a\n" (exn-message e))
                     (loop))])
               (let ([p (open-input-string full-input)])
                 (let eval-loop ()
                   (let ([expr (arc-read-expr p)])
                     (unless (eof-object? expr)
                       (let ([res (arc-eval expr env)])
                         (displayln (arc-to-string res 1))
                         (eval-loop))))))
               (loop)))])))))
