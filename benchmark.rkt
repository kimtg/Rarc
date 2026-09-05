#lang racket/base

(require racket/system
         racket/format
         racket/string
         "types.rkt"
         "arc.rkt")

(displayln "==================================================================")
(displayln "           Arc Lisp vs Native Racket vs Python Benchmark          ")
(displayln "                      Workload: Recursive (fib 34)                ")
(displayln "==================================================================")
(newline)

;; 1. Run Native Racket
(displayln "[1/3] Running Native Racket (Chez Scheme JIT)...")
(define (racket-fib n)
  (if (< n 2)
      n
      (+ (racket-fib (- n 1)) (racket-fib (- n 2)))))

;; Warm-up
(void (racket-fib 20))
(define t0-rkt (current-inexact-milliseconds))
(define rkt-ans (racket-fib 34))
(define t1-rkt (current-inexact-milliseconds))
(define rkt-time (- t1-rkt t0-rkt))
(printf "  -> Native Racket Result: ~a, Time: ~a ms\n\n" rkt-ans (~r rkt-time #:precision 2))

;; 2. Run Arc Lisp (Racket)
(displayln "[2/3] Running Arc Lisp (Racket JIT)...")
(define env (arc-init #t))
(void (arc-eval-string "
(def fib (n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))
" env))

;; Warm-up
(void (arc-eval-string "(fib 20)" env))
(define t0-arc (current-inexact-milliseconds))
(define arc-ans (arc-eval-string "(fib 34)" env))
(define t1-arc (current-inexact-milliseconds))
(define arc-time (- t1-arc t0-arc))
(printf "  -> Arc Result: ~a, Time: ~a ms\n\n" (arc-to-string arc-ans 1) (~r arc-time #:precision 2))

;; 3. Run Python Benchmark
(displayln "[3/3] Running Python 3.14 (CPython)...")
(define p-out (open-output-string))
(parameterize ([current-output-port p-out])
  (void (system "python benchmark.py 34")))

(define py-res-str (string-trim (get-output-string p-out)))
(define parts (string-split py-res-str ","))
(define py-ans (car parts))
(define py-time (string->number (cadr parts)))
(printf "  -> Python Result: ~a, Time: ~a ms\n\n" py-ans (~r py-time #:precision 2))

;; 4. Summary Table
(define rkt-ratio (/ arc-time rkt-time))
(define py-speedup (/ py-time arc-time))

(displayln "==================================================================")
(displayln "                        BENCHMARK SUMMARY                         ")
(displayln "==================================================================")
(printf " Language                | Result       | Time (ms)  | vs Native Racket | vs Python\n")
(printf "-------------------------+--------------+------------+------------------+----------\n")
(printf " Native Racket (Chez JIT)| ~a | ~a ms   | 1.00x (Baseline) | ~ax faster\n"
        (~a rkt-ans #:min-width 12)
        (~a (~r rkt-time #:precision 2) #:min-width 7)
        (~r (/ py-time rkt-time) #:precision 1))
(printf " Arc Lisp (Racket JIT)   | ~a | ~a ms   | ~ax (~a)   | ~ax faster\n"
        (~a (arc-to-string arc-ans 1) #:min-width 12)
        (~a (~r arc-time #:precision 2) #:min-width 7)
        (~r rkt-ratio #:precision 2)
        (if (< rkt-ratio 1.5) "NEAR-NATIVE" "close")
        (~r py-speedup #:precision 1))
(printf " Python 3.14.4 (CPython) | ~a | ~a ms   | ~ax slower | 1.00x\n"
        (~a py-ans #:min-width 12)
        (~a (~r py-time #:precision 2) #:min-width 7)
        (~r (/ py-time rkt-time) #:precision 1))
(displayln "==================================================================")
