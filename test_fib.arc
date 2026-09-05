; Recursive Fibonacci benchmark (test_fib.arc)
(def fib (n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(= t0 (msec))
(= ans (fib 30))
(= t1 (msec))
(prn "fib(30) = " ans ", time: " (- t1 t0) " ms")
