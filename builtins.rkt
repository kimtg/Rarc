#lang racket/base

(require racket/system
         racket/file
         racket/string
         racket/math
         racket/port
         racket/date
         "types.rkt"
         "reader.rkt")

(provide register-builtins!
         ac-global-name
         arc-+
         arc--
         arc-*
         arc-/
         arc-<
         arc->
         arc-apply
         arc-type
         arc-coerce
         arc-mod)

(define (ac-global-name s)
  (string->symbol (string-append "ar-" (symbol->string s))))

(define (to-int x)
  (if (exact-integer? x)
      x
      (inexact->exact (truncate x))))

(define (copy-arc-list a)
  (cond
    [(eq? a 'nil) 'nil]
    [(acons? a)
     (let ([res (arc-cons (acons-car a) 'nil)])
       (let loop ([src (acons-cdr a)] [dst res])
         (cond
           [(eq? src 'nil) res]
           [(acons? src)
            (let ([next-cell (arc-cons (acons-car src) 'nil)])
              (set-acons-cdr! dst next-cell)
              (loop (acons-cdr src) next-cell))]
           [else
            (set-acons-cdr! dst src)
            res])))]
    [else a]))

(define (arc-append a b)
  (if (eq? a 'nil)
      (copy-arc-list b)
      (let* ([a1 (copy-arc-list a)]
             [b1 (copy-arc-list b)])
        (let loop ([p a1])
          (if (or (eq? (acons-cdr p) 'nil) (not (acons? (acons-cdr p))))
              (begin
                (set-acons-cdr! p b1)
                a1)
              (loop (acons-cdr p)))))))

;; + (numbers, strings, and lists)
(define arc-+
  (case-lambda
    [(a b)
     (cond
       [(and (number? a) (number? b)) (+ a b)]
       [(string? a) (string-append (arc-to-string a 0) (arc-to-string b 0))]
       [(or (acons? a) (eq? a 'nil)) (arc-append a b)]
       [else (error '+ "Unsupported argument types: ~a and ~a" a b)])]
    [args
     (cond
       [(null? args) 0]
       [(number? (car args))
        (apply + args)]
       [(string? (car args))
        (apply string-append (map (lambda (x) (arc-to-string x 0)) args))]
       [(or (acons? (car args)) (eq? (car args) 'nil))
        (for/fold ([acc 'nil]) ([x args])
          (arc-append acc x))]
       [else (error '+ "Unsupported argument type: ~a" (car args))])]))

;; -
(define arc--
  (case-lambda
    [(a b)
     (if (and (number? a) (number? b))
         (- a b)
         (error '- "Expected numbers: ~a and ~a" a b))]
    [(a)
     (if (number? a)
         (- a)
         (error '- "Expected number: ~a" a))]
    [args (apply - args)]))

;; *
(define arc-*
  (case-lambda
    [(a b)
     (if (and (number? a) (number? b))
         (* a b)
         (error '* "Expected numbers: ~a and ~a" a b))]
    [args (apply * args)]))

;; /
(define arc-/
  (case-lambda
    [(a b)
     (if (and (number? a) (number? b))
         (/ a b)
         (error '/ "Expected numbers: ~a and ~a" a b))]
    [(a)
     (if (number? a)
         (/ 1.0 a)
         (error '/ "Expected number: ~a" a))]
    [args (apply / args)]))

;; <
(define arc-<
  (case-lambda
    [(a b)
     (cond
       [(and (number? a) (number? b)) (if (< a b) 't 'nil)]
       [(and (string? a) (string? b)) (if (string<? a b) 't 'nil)]
       [else 'nil])]
    [args
     (if (or (null? args) (null? (cdr args)))
         't
         (let loop ([prev (car args)] [rest (cdr args)])
           (if (null? rest)
               't
               (let ([curr (car rest)])
                 (if (if (number? prev) (< prev curr) (string<? prev curr))
                     (loop curr (cdr rest))
                     'nil)))))]))

;; >
(define arc->
  (case-lambda
    [(a b)
     (cond
       [(and (number? a) (number? b)) (if (> a b) 't 'nil)]
       [(and (string? a) (string? b)) (if (string>? a b) 't 'nil)]
       [else 'nil])]
    [args
     (if (or (null? args) (null? (cdr args)))
         't
         (let loop ([prev (car args)] [rest (cdr args)])
           (if (null? rest)
               't
               (let ([curr (car rest)])
                 (if (if (number? prev) (> prev curr) (string>? prev curr))
                     (loop curr (cdr rest))
                     'nil)))))]))

;; mod
(define (arc-mod a b)
  (let* ([r (- a (* (truncate (/ a b)) b))]
         [adj (if (and (not (= r 0)) (< (* a b) 0)) (+ r b) r)])
    (if (and (exact-integer? a) (exact-integer? b))
        (exact-round adj)
        (exact->inexact adj))))

;; apply
(define (arc-apply fn . args)
  (let ([flat
         (cond
           [(null? args) '()]
           [(null? (cdr args))
            (let ([last (car args)])
              (cond
                [(list? last) last]
                [(or (acons? last) (eq? last 'nil)) (arc-list->racket-list last)]
                [else (list last)]))]
           [else
            (let loop ([xs args])
              (cond
                [(null? xs) '()]
                [(null? (cdr xs))
                 (let ([last (car xs)])
                   (cond
                     [(list? last) last]
                     [(or (acons? last) (eq? last 'nil)) (arc-list->racket-list last)]
                     [else (list last)]))]
                [else (cons (car xs) (loop (cdr xs)))]))])])
    (cond
      [(procedure? fn) (apply fn flat)]
      [(arc-table? fn)
       (let ([n (length flat)])
         (cond
           [(= n 1) (hash-ref (arc-table-hash fn) (car flat) 'nil)]
           [(= n 2) (hash-ref (arc-table-hash fn) (car flat) (cadr flat))]
           [else (error 'apply "Table indexing takes 1 or 2 arguments")]))]
      [(string? fn)
       (if (= (length flat) 1)
           (let ([idx (to-int (car flat))])
             (if (and (>= idx 0) (< idx (string-length fn)))
                 (string-ref fn idx)
                 'nil))
           (error 'apply "String indexing takes 1 argument"))]
      [(or (acons? fn) (eq? fn 'nil))
       (if (= (length flat) 1)
           (let ([idx (to-int (car flat))])
             (if (< idx 0)
                 'nil
                 (let loop ([curr fn] [i idx])
                   (cond
                     [(eq? curr 'nil) 'nil]
                     [(= i 0) (arc-car curr)]
                     [(acons? curr) (loop (arc-cdr curr) (- i 1))]
                     [else 'nil]))))
           (error 'apply "List indexing takes 1 argument"))]
      [(arc-cont? fn)
       (if (= (length flat) 1)
           ((arc-cont-proc fn) (car flat))
           (error 'apply "Continuation takes 1 argument"))]
      [else (error 'apply "Cannot apply non-function: ~a" fn)])))

;; type
(define (arc-type x)
  (cond
    [(acons? x) 'cons]
    [(or (symbol? x) (eq? x 'nil)) 'sym]
    [(or (procedure? x) (arc-cont? x)) 'fn]
    [(string? x) 'string]
    [(number? x) 'num]
    [(arc-macro? x) 'mac]
    [(arc-table? x) 'table]
    [(char? x) 'char]
    [(arc-pipe? x) 'input-pipe]
    [(input-port? x) 'input]
    [(output-port? x) 'output]
    [else 'nil]))

;; coerce
(define (arc-coerce obj type-sym)
  (cond
    [(char? obj)
     (cond
       [(or (eq? type-sym 'int) (eq? type-sym 'num)) (char->integer obj)]
       [(eq? type-sym 'string) (string obj)]
       [(eq? type-sym 'sym) (string->symbol (string obj))]
       [(eq? type-sym 'char) obj]
       [else (error 'coerce "Cannot coerce char to ~a" type-sym)])]
    [(number? obj)
     (cond
       [(eq? type-sym 'int) (floor obj)]
       [(eq? type-sym 'char) (integer->char (to-int obj))]
       [(eq? type-sym 'string) (arc-to-string obj 0)]
       [(eq? type-sym 'num) obj]
       [else (error 'coerce "Cannot coerce number to ~a" type-sym)])]
    [(string? obj)
     (cond
       [(eq? type-sym 'sym) (string->symbol obj)]
       [(eq? type-sym 'cons) (racket-list->arc-list (string->list obj))]
       [(eq? type-sym 'num) (or (string->number obj) 0)]
       [(eq? type-sym 'int) (or (to-int (string->number obj)) 0)]
       [(eq? type-sym 'string) obj]
       [else (error 'coerce "Cannot coerce string to ~a" type-sym)])]
    [(acons? obj)
     (cond
       [(eq? type-sym 'string)
        (apply string-append
               (map (lambda (x)
                      (if (char? x)
                          (string x)
                          (arc-to-string x 0)))
                    (arc-list->racket-list obj)))]
       [(eq? type-sym 'cons) obj]
       [else (error 'coerce "Cannot coerce cons to ~a" type-sym)])]
    [(symbol? obj)
     (cond
       [(eq? type-sym 'string) (symbol->string obj)]
       [(eq? type-sym 'sym) obj]
       [else (error 'coerce "Cannot coerce sym to ~a" type-sym)])]
    [else obj]))

(define (register-builtins! ns [eval-file-fn #f])
  (define (bind! name val)
    (namespace-set-variable-value! (ac-global-name (string->symbol name)) val #f ns))

  ;; Cons operations
  (bind! "car" arc-car)
  (bind! "cdr" arc-cdr)
  (bind! "cons" arc-cons)
  (bind! "scar" arc-scar!)
  (bind! "scdr" arc-scdr!)

  ;; Math & Logic
  (bind! "+" arc-+)
  (bind! "-" arc--)
  (bind! "*" arc-*)
  (bind! "/" arc-/)
  (bind! "<" arc-<)
  (bind! ">" arc->)
  (bind! "mod" arc-mod)
  (bind! "sin" sin)
  (bind! "cos" cos)
  (bind! "tan" tan)
  (bind! "log" log)
  (bind! "sqrt" sqrt)
  (bind! "expt" expt)
  (bind! "floor" floor)
  (bind! "trunc" truncate)
  (bind! "exact" (lambda (x) (if (exact? x) 't 'nil)))
  (bind! "int" (lambda (x)
                 (cond
                   [(string? x) (or (string->number x) 0)]
                   [(symbol? x) (or (string->number (symbol->string x)) 0)]
                   [(number? x) (to-int x)]
                   [(char? x) (char->integer x)]
                   [else 0])))
  (bind! "rand" (case-lambda
                  [() (random)]
                  [(n) (if (exact-integer? n) (random n) (* (random) n))]))

  ;; Object identity & Types
  (bind! "is" (lambda (a b) (if (arc-is a b) 't 'nil)))
  (bind! "iso" (lambda (a b) (if (arc-iso a b) 't 'nil)))
  (bind! "type" arc-type)
  (bind! "coerce" arc-coerce)
  (bind! "len" arc-len)
  (bind! "sym" (lambda (x) (string->symbol (arc-to-string x 0))))
  (bind! "string" (lambda args (apply string-append (map (lambda (x) (arc-to-string x 0)) args))))
  (bind! "newstring" (lambda (len [c #\nul]) (make-string (to-int len) c)))

  ;; Tables
  (bind! "table" make-arc-table)
  (bind! "table-sref" (lambda (tbl val key) (hash-set! (arc-table-hash tbl) key val) val))
  (bind! "string-sref" (lambda (str val idx) (string-set! str (to-int idx) val) val))
  (bind! "maptable" (lambda (proc tbl) (for ([(k v) (in-hash (arc-table-hash tbl))]) (arc-apply proc k v)) tbl))

  ;; Application & Execution
  (bind! "apply" arc-apply)
  (bind! "ccc" (lambda (fn) (call/cc (lambda (k) (arc-apply fn (list (arc-cont k)))))))
  (bind! "bound" (lambda (s) (if (namespace-variable-value (ac-global-name s) #f (lambda () #f) ns) 't 'nil)))
  (bind! "atomic" (lambda (f) (arc-apply f '())))
  (bind! "new-thread" (lambda (f) (thread (lambda () (arc-apply f '())))))
  (bind! "kill-thread" (lambda (th) (kill-thread th) 'nil))
  (bind! "sleep" (lambda (secs) (sleep secs) 'nil))
  (bind! "system" (lambda (cmd) (if (system cmd) 0 1)))
  (bind! "err" (lambda msgs (for ([m msgs]) (eprintf "~a\n" (arc-to-string m 0))) (error 'arc-user-error "Execution halted by (err)")))
  (bind! "quit" (lambda () (exit 0)))
  (bind! "exit" (lambda () (exit 0)))

  ;; Time & Date
  (bind! "msec" current-inexact-milliseconds)
  (bind! "timedate" (lambda ([s #f])
                      (let ([d (if s (seconds->date (to-int s)) (current-date))])
                        (racket-list->arc-list
                         (list (date-second d) (date-minute d) (date-hour d)
                               (date-day d) (date-month d) (date-year d))))))

  ;; I/O & Files
  (bind! "disp" (lambda (x [p (current-output-port)])
                  (arc-disp x (if (arc-pipe? p) (arc-pipe-in-port p) p)) 'nil))
  (bind! "write" (lambda (x [p (current-output-port)])
                   (arc-write x (if (arc-pipe? p) (arc-pipe-in-port p) p)) 'nil))
  (bind! "writeb" (lambda (b [p (current-output-port)])
                    (write-byte (to-int b) (if (arc-pipe? p) (arc-pipe-in-port p) p)) 'nil))
  (bind! "readb" (lambda ([p (current-input-port)])
                   (let ([b (read-byte (if (arc-pipe? p) (arc-pipe-in-port p) p))])
                     (if (eof-object? b) 'nil b))))
  (bind! "readline" (lambda ([p (current-input-port)])
                      (let ([l (read-line (if (arc-pipe? p) (arc-pipe-in-port p) p) 'any)])
                        (if (eof-object? l) 'nil l))))
  (bind! "read" (lambda ([src (current-input-port)] [eof-val 'nil])
                  (cond
                    [(string? src) (arc-read-from-string src eof-val)]
                    [(port? src) (arc-read src eof-val)]
                    [(arc-pipe? src) (arc-read (arc-pipe-in-port src) eof-val)]
                    [else (error 'read "Unsupported source: ~a" src)])))
  (bind! "sread" (lambda (p eof-val)
                   (arc-read (if (arc-pipe? p) (arc-pipe-in-port p) p) eof-val)))
  (bind! "flushout" (lambda () (flush-output (current-output-port)) 't))

  (bind! "infile" (lambda (p) (open-input-file p #:mode 'binary)))
  (bind! "outfile" (lambda (p [mode 'replace])
                     (open-output-file p #:mode 'text #:exists (if (eq? mode 'append) 'append 'replace))))
  (bind! "instring" open-input-string)
  (bind! "outstring" open-output-string)
  (bind! "inside" get-output-string)
  (bind! "close" (lambda (p)
                   (cond
                     [(arc-pipe? p) (close-input-port (arc-pipe-in-port p))]
                     [(input-port? p) (close-input-port p)]
                     [(output-port? p) (close-output-port p)])
                   'nil))
  (bind! "call-w/stdin" (lambda (p f) (parameterize ([current-input-port p]) (arc-apply f '()))))
  (bind! "call-w/stdout" (lambda (p f) (parameterize ([current-output-port p]) (arc-apply f '()))))
  (bind! "pipe-from" (lambda (cmd)
                       (define-values (shell-exec shell-flag)
                         (if (eq? (system-type) 'windows)
                             (values (or (find-executable-path "cmd.exe")
                                         (getenv "COMSPEC")
                                         "cmd.exe")
                                     "/c")
                             (values (or (find-executable-path (or (getenv "SHELL") "sh"))
                                         (find-executable-path "sh")
                                         "/bin/sh")
                                     "-c")))
                       (let-values ([(proc stdout-port stdin-port stderr-port)
                                     (subprocess #f #f #f shell-exec shell-flag cmd)])
                         (close-output-port stdin-port)
                         (arc-pipe stdout-port proc))))


  (bind! "dir" (lambda (d) (racket-list->arc-list (map path->string (directory-list d)))))
  (bind! "file-exists" (lambda (p) (if (file-exists? p) 't 'nil)))
  (bind! "dir-exists" (lambda (p) (if (directory-exists? p) 't 'nil)))
  (bind! "rmfile" (lambda (p) (delete-file p) 'nil))
  (bind! "mvfile" (lambda (a b) (rename-file-or-directory a b #t) 'nil))
  (bind! "ensure-dir" (lambda (p) (make-directory* p) 'nil))

  ;; Sugars reflection
  (bind! "ssyntax" (lambda (s)
                     (if (symbol? s)
                         (let ([str (symbol->string s)])
                           (if (or (string-contains? str ".")
                                   (string-contains? str "!")
                                   (string-contains? str ":")
                                   (string-prefix? str "~"))
                               't 'nil))
                         'nil)))
  (bind! "sflag" (lambda (s)
                   (if (and (symbol? s) (string-prefix? (symbol->string s) "~"))
                       't 'nil)))
  (bind! "sval" (lambda (s) (expand-arc-sugars s)))

  ;; File loading
  (bind! "load" (lambda (path)
                  (if eval-file-fn
                      (eval-file-fn path)
                      (error 'load "eval-file-fn not configured"))
                  'nil))

  ;; Standard ports & constants
  (bind! "stdin" (current-input-port))
  (bind! "stdout" (current-output-port))
  (bind! "stderr" (current-error-port))
  (bind! "t" 't)
  (bind! "nil" 'nil))
