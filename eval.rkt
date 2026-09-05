#lang racket/base

(require racket/fixnum
         "types.rkt"
         "reader.rkt"
         "builtins.rkt")


(provide arc-eval
         arc-macex-eval
         arc-compile
         arc-apply
         macex
         make-arc-namespace
         current-arc-namespace
         arc-macro-table
         arc-fast-+
         arc-fast--
         arc-fast-*
         arc-fast-/
         arc-fast-<
         arc-fast->
         arc-fast-cond-<
         arc-fast-cond->
         (struct-out exn:fail:arc-eval))

(struct exn:fail:arc-eval exn:fail (expr) #:transparent)

(define (raise-arc-eval msg expr)
  (raise (exn:fail:arc-eval (format "Evaluation error: ~a in: ~a" msg (arc-to-string expr 1))
                            (current-continuation-marks)
                            expr)))

(define current-arc-namespace (make-parameter #f))
(define arc-macro-table (make-hasheq))

;; Fast primitive operations
(define (arc-fast-+ a b)
  (if (and (number? a) (number? b))
      (+ a b)
      (arc-+ a b)))

(define (arc-fast-- a b)
  (if (and (number? a) (number? b))
      (- a b)
      (arc-- a b)))

(define (arc-fast-* a b)
  (if (and (number? a) (number? b))
      (* a b)
      (arc-* a b)))

(define (arc-fast-/ a b)
  (if (and (number? a) (number? b))
      (/ a b)
      (arc-/ a b)))

(define (arc-fast-< a b)
  (if (and (number? a) (number? b))
      (if (< a b) 't 'nil)
      (arc-< a b)))

(define (arc-fast-> a b)
  (if (and (number? a) (number? b))
      (if (> a b) 't 'nil)
      (arc-> a b)))

(define (arc-fast-cond-< a b)
  (if (and (number? a) (number? b))
      (< a b)
      (if (and (string? a) (string? b))
          (string<? a b)
          #f)))

(define (arc-fast-cond-> a b)
  (if (and (number? a) (number? b))
      (> a b)
      (if (and (string? a) (string? b))
          (string>? a b)
          #f)))

;; Fast call dispatchers
(define (arc-call-slow fn args)
  (cond
    [(arc-table? fn)
     (let ([n (length args)])
       (cond
         [(= n 1) (hash-ref (arc-table-hash fn) (car args) 'nil)]
         [(= n 2) (hash-ref (arc-table-hash fn) (car args) (cadr args))]
         [else (error 'apply "Table indexing takes 1 or 2 arguments, got ~a" n)]))]
    [(string? fn)
     (if (= (length args) 1)
         (let ([idx (car args)])
           (if (and (exact-integer? idx) (>= idx 0) (< idx (string-length fn)))
               (string-ref fn idx)
               'nil))
         (error 'apply "String indexing takes 1 argument, got ~a" (length args)))]
    [(or (acons? fn) (eq? fn 'nil))
     (if (= (length args) 1)
         (let ([idx (car args)])
           (if (and (exact-integer? idx) (>= idx 0))
               (let loop ([curr fn] [i idx])
                 (cond
                   [(eq? curr 'nil) 'nil]
                   [(= i 0) (arc-car curr)]
                   [(acons? curr) (loop (arc-cdr curr) (- i 1))]
                   [else 'nil]))
               'nil))
         (error 'apply "List indexing takes 1 argument, got ~a" (length args)))]
    [(arc-cont? fn)
     ((arc-cont-proc fn) (car args))]
    [else (error 'apply "Cannot apply non-function: ~a" (arc-to-string fn 1))]))

(define (arc-call fn . args)
  (if (procedure? fn)
      (apply fn args)
      (arc-call-slow fn args)))

(define (arc-call0 fn)
  (if (procedure? fn) (fn) (arc-call-slow fn '())))

(define (arc-call1 fn a)
  (if (procedure? fn) (fn a) (arc-call-slow fn (list a))))

(define (arc-call2 fn a b)
  (if (procedure? fn) (fn a b) (arc-call-slow fn (list a b))))

(define (arc-call3 fn a b c)
  (if (procedure? fn) (fn a b c) (arc-call-slow fn (list a b c))))

(define (arc-global-set! sym val)
  (namespace-set-variable-value! sym val #f (or (current-arc-namespace) (current-namespace)))
  val)

;; Destructuring compilation
(define (compile-destruct-pattern pat expr-code)
  (cond
    [(symbol? pat)
     `((,pat ,expr-code))]
    [(acons? pat)
     (let ([tmp (gensym 'd)])
       `((,tmp ,expr-code)
         ,@(compile-destruct-pattern (acons-car pat) `(arc-car ,tmp))
         ,@(compile-destruct-pattern (acons-cdr pat) `(arc-cdr ,tmp))))]
    [else '()]))

(define (extract-symbols pat)
  (cond
    [(symbol? pat) (if (eq? pat 'nil) '() (list pat))]
    [(acons? pat) (append (extract-symbols (acons-car pat)) (extract-symbols (acons-cdr pat)))]
    [else '()]))

;; Parameter compilation for fn
(define (compile-params params body-arc-list env [self-name #f])
  (cond
    ;; Case 1: no params (fn () ...)
    [(eq? params 'nil)
     (let ([body (map (lambda (e) (arc-compile e env self-name)) (arc-list->racket-list body-arc-list))])
       `(() ,@body))]
    
    ;; Case 2: rest param only (fn args ...)
    [(symbol? params)
     (let* ([r-arg (gensym 'r)]
            [new-env (cons params env)]
            [body (map (lambda (e) (arc-compile e new-env self-name)) (arc-list->racket-list body-arc-list))])
       `(,r-arg
         (let ([,params (racket-list->arc-list ,r-arg)])
           ,@body)))]
    
    ;; Case 3: cons params
    [(acons? params)
     (let loop ([curr params] [r-params '()] [let-bindings '()] [new-env env])
       (cond
         [(eq? curr 'nil)
          (let ([body (map (lambda (e) (arc-compile e new-env self-name)) (arc-list->racket-list body-arc-list))])
            (if (null? let-bindings)
                `(,(reverse r-params) ,@body)
                `(,(reverse r-params) (let* ,let-bindings ,@body))))]
         [(symbol? curr)
          ;; Dotted rest param: (fn (x . rest) ...)
          (let* ([r-rest (gensym 'r)]
                 [new-env2 (cons curr new-env)]
                 [bindings (append let-bindings `([,curr (racket-list->arc-list ,r-rest)]))]
                 [body (map (lambda (e) (arc-compile e new-env2 self-name)) (arc-list->racket-list body-arc-list))])
            `(,(append (reverse r-params) r-rest)
              (let* ,bindings ,@body)))]
         [(acons? curr)
          (let ([item (acons-car curr)])
            (cond
              ;; Simple symbol param
              [(symbol? item)
               (loop (acons-cdr curr)
                     (cons item r-params)
                     let-bindings
                     (cons item new-env))]
              ;; Optional param: (o var [default])
              [(and (acons? item) (eq? (acons-car item) 'o))
               (let* ([tail (acons-cdr item)]
                      [var (if (acons? tail) (acons-car tail) (error 'fn "Malformed (o ...)"))]
                      [def-expr (if (and (acons? tail) (acons? (acons-cdr tail)))
                                    (arc-compile (acons-car (acons-cdr tail)) new-env self-name)
                                    ''nil)])
                 (loop (acons-cdr curr)
                       (cons `[,var ,def-expr] r-params)
                       let-bindings
                       (cons var new-env)))]
              ;; Destructuring pattern
              [else
               (let ([tmp (gensym 'p)])
                 (loop (acons-cdr curr)
                       (cons tmp r-params)
                       (append let-bindings (compile-destruct-pattern item tmp))
                       (append (extract-symbols item) new-env)))]))]
         [else (error 'compile "Invalid parameter list")]))]
    [else (error 'compile "Invalid parameters")]))

;; Chained if condition optimization
(define (compile-cond c env [self-name #f])
  (if (acons? c)
      (let ([op (acons-car c)]
            [args (acons-cdr c)])
        (cond
          [(and (eq? op '<)
                (not (memq '< env))
                (acons? args)
                (acons? (acons-cdr args))
                (eq? (acons-cdr (acons-cdr args)) 'nil))
           `(arc-fast-cond-< ,(arc-compile (acons-car args) env self-name)
                             ,(arc-compile (acons-car (acons-cdr args)) env self-name))]
          [(and (eq? op '>)
                (not (memq '> env))
                (acons? args)
                (acons? (acons-cdr args))
                (eq? (acons-cdr (acons-cdr args)) 'nil))
           `(arc-fast-cond-> ,(arc-compile (acons-car args) env self-name)
                             ,(arc-compile (acons-car (acons-cdr args)) env self-name))]
          [else
           `(arc-true? ,(arc-compile c env self-name))]))
      `(arc-true? ,(arc-compile c env self-name))))

;; Chained if compilation
(define (compile-if args env [self-name #f])
  (cond
    [(eq? args 'nil) ''nil]
    [(not (acons? args)) (raise-arc-eval "if: malformed branches" args)]
    [(eq? (acons-cdr args) 'nil)
     (arc-compile (acons-car args) env self-name)]
    [else
     (let ([c (acons-car args)]
           [t (acons-car (acons-cdr args))]
           [rest (acons-cdr (acons-cdr args))])
       `(if ,(compile-cond c env self-name)
            ,(arc-compile t env self-name)
            ,(compile-if rest env self-name)))]))

;; Compiles fn with optional self-referential letrec optimization
(define (compile-fn params body env [self-name #f])
  (if self-name
      (let* ([self-env (cons self-name env)]
             [compiled (compile-params params body self-env self-name)])
        `(letrec ([,self-name (lambda ,(car compiled) ,@(cdr compiled))])
           ,self-name))
      (let ([compiled (compile-params params body env #f)])
        `(lambda ,(car compiled) ,@(cdr compiled)))))

;; Core compiler from Arc AST to Racket S-expression
(define (arc-compile expr [env '()] [self-name #f])
  (cond
    [(eq? expr 'nil) ''nil]
    [(or (number? expr) (string? expr) (char? expr)) expr]
    [(symbol? expr)
     (cond
       [(eq? expr 'nil) ''nil]
       [(eq? expr 't) ''t]
       [(memq expr env) expr]
       [else (ac-global-name expr)])]
    [(acons? expr)
     (let ([op (acons-car expr)]
           [args (acons-cdr expr)])
       (cond
         ;; Macro expansion
         [(and (symbol? op) (hash-has-key? arc-macro-table op))
          (let* ([macro-proc (hash-ref arc-macro-table op)]
                 [r-args (arc-list->racket-list args)]
                 [expanded (apply macro-proc r-args)])
            (arc-compile expanded env self-name))]
         
         ;; Special form: quote
         [(eq? op 'quote)
          (if (and (acons? args) (eq? (acons-cdr args) 'nil))
              `(quote ,(acons-car args))
              (raise-arc-eval "quote takes exactly 1 argument" expr))]
         
         ;; Special form: assign
         [(eq? op 'assign)
          (if (and (acons? args)
                   (acons? (acons-cdr args))
                   (eq? (acons-cdr (acons-cdr args)) 'nil))
              (let ([var (acons-car args)]
                    [val (acons-car (acons-cdr args))])
                (if (symbol? var)
                    (let ([val-code
                           (if (and (acons? val) (eq? (acons-car val) 'fn))
                               ;; Pass var as self-name to enable local letrec recursion
                               (compile-fn (acons-car (acons-cdr val))
                                           (acons-cdr (acons-cdr val))
                                           env
                                           var)
                               (arc-compile val env self-name))])
                      (if (memq var env)
                          `(begin (set! ,var ,val-code) ,var)
                          `(arc-global-set! ',(ac-global-name var) ,val-code)))
                    (raise-arc-eval "assign: variable must be a symbol" expr)))
              (raise-arc-eval "assign: expected (assign sym val)" expr))]
         
         ;; Special form: fn (anonymous functions have no self-name)
         [(eq? op 'fn)
          (if (acons? args)
              (let ([params (acons-car args)]
                    [body (acons-cdr args)])
                (compile-fn params body env #f))
              (raise-arc-eval "fn: expected (fn parms . body)" expr))]
         
         ;; Special form: if
         [(eq? op 'if)
          (compile-if args env self-name)]
         
         ;; Special form: do
         [(eq? op 'do)
          (if (eq? args 'nil)
              ''nil
              `(begin ,@(map (lambda (e) (arc-compile e env self-name)) (arc-list->racket-list args))))]
         
         ;; Special form: mac
         [(eq? op 'mac)
          (if (and (acons? args)
                   (acons? (acons-cdr args))
                   (acons? (acons-cdr (acons-cdr args))))
              (let* ([name (acons-car args)]
                     [params (acons-car (acons-cdr args))]
                     [body (acons-cdr (acons-cdr args))]
                     [fn-code (arc-compile (arc-cons 'fn (arc-cons params body)) env)]
                     [ns (or (current-arc-namespace) (current-namespace))]
                     [macro-proc (eval fn-code ns)])
                (if (symbol? name)
                    (begin
                      (hash-set! arc-macro-table name macro-proc)
                      (namespace-set-variable-value! (ac-global-name name) (arc-macro macro-proc) #f ns)
                      `',name)
                    (raise-arc-eval "mac: macro name must be a symbol" expr)))
              (raise-arc-eval "mac: expected (mac name parms . body)" expr))]
         
         ;; Direct primitive inlining (when not locally shadowed)
         [(and (eq? op '+) (not (memq '+ env)) (acons? args) (acons? (acons-cdr args)) (eq? (acons-cdr (acons-cdr args)) 'nil))
          `(arc-fast-+ ,(arc-compile (acons-car args) env self-name) ,(arc-compile (acons-car (acons-cdr args)) env self-name))]
         
         [(and (eq? op '-) (not (memq '- env)) (acons? args) (acons? (acons-cdr args)) (eq? (acons-cdr (acons-cdr args)) 'nil))
          `(arc-fast-- ,(arc-compile (acons-car args) env self-name) ,(arc-compile (acons-car (acons-cdr args)) env self-name))]
         
         [(and (eq? op '-) (not (memq '- env)) (acons? args) (eq? (acons-cdr args) 'nil))
          `(- ,(arc-compile (acons-car args) env self-name))]
         
         [(and (eq? op '*) (not (memq '* env)) (acons? args) (acons? (acons-cdr args)) (eq? (acons-cdr (acons-cdr args)) 'nil))
          `(arc-fast-* ,(arc-compile (acons-car args) env self-name) ,(arc-compile (acons-car (acons-cdr args)) env self-name))]
         
         [(and (eq? op '/) (not (memq '/ env)) (acons? args) (acons? (acons-cdr args)) (eq? (acons-cdr (acons-cdr args)) 'nil))
          `(arc-fast-/ ,(arc-compile (acons-car args) env self-name) ,(arc-compile (acons-car (acons-cdr args)) env self-name))]
         
         [(and (eq? op '<) (not (memq '< env)) (acons? args) (acons? (acons-cdr args)) (eq? (acons-cdr (acons-cdr args)) 'nil))
          `(arc-fast-< ,(arc-compile (acons-car args) env self-name) ,(arc-compile (acons-car (acons-cdr args)) env self-name))]
         
         [(and (eq? op '>) (not (memq '> env)) (acons? args) (acons? (acons-cdr args)) (eq? (acons-cdr (acons-cdr args)) 'nil))
          `(arc-fast-> ,(arc-compile (acons-car args) env self-name) ,(arc-compile (acons-car (acons-cdr args)) env self-name))]
         
         [(and (eq? op 'car) (not (memq 'car env)) (acons? args) (eq? (acons-cdr args) 'nil))
          `(arc-car ,(arc-compile (acons-car args) env self-name))]
         
         [(and (eq? op 'cdr) (not (memq 'cdr env)) (acons? args) (eq? (acons-cdr args) 'nil))
          `(arc-cdr ,(arc-compile (acons-car args) env self-name))]
         
         [(and (eq? op 'cons) (not (memq 'cons env)) (acons? args) (acons? (acons-cdr args)) (eq? (acons-cdr (acons-cdr args)) 'nil))
          `(arc-cons ,(arc-compile (acons-car args) env self-name) ,(arc-compile (acons-car (acons-cdr args)) env self-name))]
         
         ;; Direct self-recursive call inlining (only when op is the self-recursive function)
         [(and (symbol? op) self-name (eq? op self-name))
          (let ([compiled-args (map (lambda (a) (arc-compile a env self-name)) (arc-list->racket-list args))])
            `(,op ,@compiled-args))]
         
         ;; Function application
         [else
          (let ([compiled-op (arc-compile op env self-name)]
                [compiled-args (map (lambda (a) (arc-compile a env self-name)) (arc-list->racket-list args))])
            (case (length compiled-args)
              [(0) `(arc-call0 ,compiled-op)]
              [(1) `(arc-call1 ,compiled-op ,(car compiled-args))]
              [(2) `(arc-call2 ,compiled-op ,(car compiled-args) ,(cadr compiled-args))]
              [(3) `(arc-call3 ,compiled-op ,(car compiled-args) ,(cadr compiled-args) ,(caddr compiled-args))]
              [else `(arc-call ,compiled-op ,@compiled-args)]))]))]
    [else expr]))

;; Macro-expand an expression recursively
(define (macex expr)
  (cond
    [(not (acons? expr)) expr]
    [(eq? (acons-car expr) 'quote) expr]
    [(and (symbol? (acons-car expr))
          (hash-has-key? arc-macro-table (acons-car expr)))
     (let* ([macro-proc (hash-ref arc-macro-table (acons-car expr))]
            [r-args (arc-list->racket-list (acons-cdr expr))]
            [expanded (apply macro-proc r-args)])
       (macex expanded))]
    [else
     (let map-list ([curr expr])
       (cond
         [(eq? curr 'nil) 'nil]
         [(acons? curr)
          (arc-cons (macex (acons-car curr))
                    (map-list (acons-cdr curr)))]
         [else (macex curr)]))]))

;; Namespace setup
(define (make-arc-namespace [eval-file-fn #f])
  (define ns (make-base-namespace))
  
  (namespace-attach-module (current-namespace) 'racket/fixnum ns)
  (parameterize ([current-namespace ns])
    (namespace-require 'racket/fixnum))
  
  ;; Inject compiler runtime helpers
  (namespace-set-variable-value! 'arc-true? arc-true? #f ns)
  (namespace-set-variable-value! 'arc-nil? arc-nil? #f ns)


  (eval '(define-syntax-rule (arc-fast-+ a b)
           (let ([x a] [y b])
             (if (and (number? x) (number? y)) (+ x y) (arc-+ x y)))) ns)
  (eval '(define-syntax-rule (arc-fast-- a b)
           (let ([x a] [y b])
             (if (and (fixnum? x) (fixnum? y))
                 (fx- x y)
                 (if (and (number? x) (number? y)) (- x y) (arc-- x y))))) ns)
  (eval '(define-syntax-rule (arc-fast-* a b)
           (let ([x a] [y b]) (if (and (number? x) (number? y)) (* x y) (arc-* x y)))) ns)
  (eval '(define-syntax-rule (arc-fast-/ a b)
           (let ([x a] [y b]) (if (and (number? x) (number? y)) (/ x y) (arc-/ x y)))) ns)
  (eval '(define-syntax-rule (arc-fast-< a b)
           (let ([x a] [y b])
             (if (and (fixnum? x) (fixnum? y))
                 (if (fx< x y) 't 'nil)
                 (if (and (number? x) (number? y)) (if (< x y) 't 'nil) (arc-< x y))))) ns)
  (eval '(define-syntax-rule (arc-fast-> a b)
           (let ([x a] [y b])
             (if (and (fixnum? x) (fixnum? y))
                 (if (fx> x y) 't 'nil)
                 (if (and (number? x) (number? y)) (if (> x y) 't 'nil) (arc-> x y))))) ns)
  (eval '(define-syntax-rule (arc-fast-cond-< a b)
           (let ([x a] [y b])
             (if (and (fixnum? x) (fixnum? y))
                 (fx< x y)
                 (if (and (number? x) (number? y))
                     (< x y)
                     (if (and (string? x) (string? y)) (string<? x y) #f))))) ns)
  (eval '(define-syntax-rule (arc-fast-cond-> a b)
           (let ([x a] [y b])
             (if (and (fixnum? x) (fixnum? y))
                 (fx> x y)
                 (if (and (number? x) (number? y))
                     (> x y)
                     (if (and (string? x) (string? y)) (string>? x y) #f))))) ns)
  (namespace-set-variable-value! 'arc-+ arc-+ #f ns)
  (namespace-set-variable-value! 'arc-- arc-- #f ns)
  (namespace-set-variable-value! 'arc-* arc-* #f ns)
  (namespace-set-variable-value! 'arc-/ arc-/ #f ns)
  (namespace-set-variable-value! 'arc-< arc-< #f ns)
  (namespace-set-variable-value! 'arc-> arc-> #f ns)
  (namespace-set-variable-value! 'arc-call arc-call #f ns)
  (namespace-set-variable-value! 'arc-call0 arc-call0 #f ns)
  (namespace-set-variable-value! 'arc-call1 arc-call1 #f ns)
  (namespace-set-variable-value! 'arc-call2 arc-call2 #f ns)
  (namespace-set-variable-value! 'arc-call3 arc-call3 #f ns)
  (namespace-set-variable-value! 'arc-call-slow arc-call-slow #f ns)
  (namespace-set-variable-value! 'arc-global-set! arc-global-set! #f ns)
  (namespace-set-variable-value! 'arc-cons arc-cons #f ns)
  (namespace-set-variable-value! 'arc-car arc-car #f ns)
  (namespace-set-variable-value! 'arc-cdr arc-cdr #f ns)
  (namespace-set-variable-value! 'racket-list->arc-list racket-list->arc-list #f ns)

  ;; Register all 58 builtins
  (register-builtins! ns eval-file-fn)
  (namespace-set-variable-value! (ac-global-name 'macex) macex #f ns)
  (namespace-set-variable-value! (ac-global-name 'eval) (lambda (e) (arc-eval e ns)) #f ns)
  
  ns)

;; Evaluates an Arc AST
(define (arc-eval expr [ns (or (current-arc-namespace) (current-namespace))])
  (parameterize ([current-namespace ns]
                 [current-arc-namespace ns])
    (let ([code (arc-compile expr '())])
      (eval code ns))))

;; Evaluates with explicit macro expansion first
(define (arc-macex-eval expr [ns (or (current-arc-namespace) (current-namespace))])
  (arc-eval expr ns))
