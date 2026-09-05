#lang racket

(require "types.rkt"
         "reader.rkt"
         "eval.rkt"
         "arc.rkt")

(define passed 0)
(define failed 0)

(define (test-case name actual expected)
  (let ([actual-str (arc-to-string actual 1)]
        [expected-str (if (string? expected) expected (arc-to-string expected 1))])
    (if (string=? actual-str expected-str)
        (begin
          (set! passed (+ passed 1))
          (printf "  [PASS] ~a\n" name))
        (begin
          (set! failed (+ failed 1))
          (printf "  [FAIL] ~a\n    Expected: ~a\n    Actual:   ~a\n"
                  name expected-str actual-str)))))

(printf "=== Arc Lisp in Racket Test Suite ===\n\n")

(define env (arc-init #t))

;; 1. Special Forms
(printf "1. Special Forms:\n")
(test-case "quote" (arc-eval-string "'(1 2 3)" env) "(1 2 3)")
(test-case "assign & lookup" (arc-eval-string "(= x 42) x" env) "42")
(test-case "fn & call" (arc-eval-string "((fn (a b) (+ a b)) 10 20)" env) "30")
(test-case "fn rest args" (arc-eval-string "((fn (x . rest) rest) 1 2 3 4)" env) "(2 3 4)")
(test-case "fn optional arg default" (arc-eval-string "((fn (x (o y 99)) (+ x y)) 1)" env) "100")
(test-case "fn optional arg provided" (arc-eval-string "((fn (x (o y 99)) (+ x y)) 1 5)" env) "6")
(test-case "if multi-branch (1st true)" (arc-eval-string "(if (< 1 2) 'yes 'no)" env) "yes")
(test-case "if multi-branch (else)" (arc-eval-string "(if (> 1 2) 'yes 'no)" env) "no")
(test-case "if multi-branch (chained)" (arc-eval-string "(if nil 1 nil 2 'three 3 4)" env) "3")
(test-case "do" (arc-eval-string "(do (= a 1) (= b 2) (+ a b))" env) "3")

;; 2. Reader & Syntax Sugars
(printf "\n2. Reader & Syntax Sugars:\n")
(test-case "bracket fn shorthand" (arc-eval-string "([+ _ 10] 5)" env) "15")
(test-case "bracket with list" (arc-eval-string "([list _ _] 42)" env) "(42 42)")
(test-case "dot syntax car.xs" (arc-eval-string "(let xs '(10 20 30) car.xs)" env) "10")
(test-case "dot syntax cdr.xs" (arc-eval-string "(let xs '(10 20 30) (car cdr.xs))" env) "20")
(test-case "bang syntax tbl!key" (arc-eval-string "(with (tbl (table)) (= tbl!alpha 77) tbl!alpha)" env) "77")
(test-case "colon compose syntax" (arc-eval-string "(let f +:sqrt (f 16))" env) "4")
(test-case "tilde complement syntax" (arc-eval-string "(let not-even ~even (list (not-even 3) (not-even 4)))" env) "(t nil)")
(test-case "quasiquote basic" (arc-eval-string "(let x 5 `(1 ,x 3))" env) "(1 5 3)")
(test-case "quasiquote splice" (arc-eval-string "(let xs '(2 3) `(1 ,@xs 4))" env) "(1 2 3 4)")

;; 3. Built-in Primitives
(printf "\n3. Built-in Primitives:\n")
(test-case "car" (arc-eval-string "(car '(a b))" env) "a")
(test-case "cdr" (arc-eval-string "(cdr '(a b))" env) "(b)")
(test-case "cons" (arc-eval-string "(cons 'a '(b))" env) "(a b)")
(test-case "scar" (arc-eval-string "(let p (cons 1 2) (scar p 10) p)" env) "(10 . 2)")
(test-case "scdr" (arc-eval-string "(let p (cons 1 2) (scdr p 20) p)" env) "(1 . 20)")
(test-case "+ numbers" (arc-eval-string "(+ 1 2 3 4)" env) "10")
(test-case "+ strings" (arc-eval-string "(+ \"hello\" \" \" \"world\")" env) "\"hello world\"")
(test-case "+ lists" (arc-eval-string "(+ '(1 2) '(3 4))" env) "(1 2 3 4)")
(test-case "- subtract" (arc-eval-string "(- 10 3 2)" env) "5")
(test-case "* multiply" (arc-eval-string "(* 2 3 4)" env) "24")
(test-case "/ divide" (arc-eval-string "(/ 24 3 2)" env) "4")
(test-case "mod" (arc-eval-string "(mod 17 5)" env) "2")
(test-case "type" (arc-eval-string "(list (type nil) (type '(1)) (type \"abc\") (type 123) (type (fn () nil)) (type (table)))" env)
           "(sym cons string num fn table)")
(test-case "len string" (arc-eval-string "(len \"hello\")" env) "5")
(test-case "len list" (arc-eval-string "(len '(1 2 3 4 5))" env) "5")
(test-case "is equality" (arc-eval-string "(list (is 'a 'a) (is 'a 'b) (is 10 10) (is \"ab\" \"ab\"))" env) "(t nil t t)")
(test-case "iso structural" (arc-eval-string "(iso '(1 (2 3)) '(1 (2 3)))" env) "t")
(test-case "coerce" (arc-eval-string "(list (coerce #\\a 'int) (coerce \"123\" 'num) (coerce '(a b c) 'string) (coerce 'sym 'string))" env)
           "(97 123 \"abc\" \"sym\")")
(test-case "ccc continuation" (arc-eval-string "(+ 2 (ccc (fn (k) (+ 3 (k 10)))))" env) "12")
(test-case "pipe-from" (arc-eval-string "(let p (pipe-from \"echo hello\") (readline p))" env) "\"hello\"")


;; 4. Implicit Indexing
(printf "\n4. Implicit Indexing:\n")
(test-case "table indexing" (arc-eval-string "(let tbl (table) (= (tbl 'key) 'val) (tbl 'key))" env) "val")
(test-case "table default" (arc-eval-string "(let tbl (table) (tbl 'missing 'default-value))" env) "default-value")
(test-case "list indexing" (arc-eval-string "(let lst '(a b c d) (list (lst 0) (lst 2) (lst 4)))" env) "(a c nil)")
(test-case "string indexing" (arc-eval-string "(let s \"hello\" (s 1))" env) "#\\e")

;; 5. Standard Library
(printf "\n5. Standard Library:\n")
(test-case "def" (arc-eval-string "(def add2 (x y) (+ x y)) (add2 7 8)" env) "15")
(test-case "let" (arc-eval-string "(let x 10 (+ x 5))" env) "15")
(test-case "with" (arc-eval-string "(with (x 10 y 20) (+ x y))" env) "30")
(test-case "map1" (arc-eval-string "(map1 [+ _ 1] '(1 2 3))" env) "(2 3 4)")
(test-case "reduce" (arc-eval-string "(reduce + '(1 2 3 4 5))" env) "15")
(test-case "rev" (arc-eval-string "(rev '(1 2 3 4))" env) "(4 3 2 1)")
(test-case "join" (arc-eval-string "(join '(1 2) '(3 4) '(5 6))" env) "(1 2 3 4 5 6)")
(test-case "pair" (arc-eval-string "(pair '(1 2 3 4 5 6))" env) "((1 2) (3 4) (5 6))")
(test-case "for loop" (arc-eval-string "(with (sum 0) (for i 1 5 (= sum (+ sum i))) sum)" env) "15")
(test-case "while loop" (arc-eval-string "(with (i 0 sum 0) (while (< i 5) (= sum (+ sum i)) (= i (+ i 1))) sum)" env) "10")
(test-case "sort" (arc-eval-string "(sort < '(4 2 7 1 3 9 5))" env) "(1 2 3 4 5 7 9)")

;; 6. Benchmark: (fib 30)
(printf "\n6. Benchmark: Recursive Fibonacci (fib 30)\n")
(arc-eval-string "
(def fib (n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))
" env)

(define t0 (arc-eval-string "(msec)" env))
(define fib-result (arc-eval-string "(fib 30)" env))
(define t1 (arc-eval-string "(msec)" env))
(define elapsed-ms (- t1 t0))

(test-case "fib(30) value" fib-result "832040")
(printf "  fib(30) = ~a, time: ~a ms\n" (arc-to-string fib-result 1) (exact-round elapsed-ms))

(printf "\n=== Summary: ~a passed, ~a failed ===\n" passed failed)

(when (> failed 0)
  (exit 1))
