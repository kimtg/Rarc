; Project Euler: Problems 1 - 10 in Arc Lisp
; Run with: racket main.rkt euler.arc

(prn "==================================================")
(prn "       Project Euler Problems 1 - 10 in Arc       ")
(prn "==================================================")

; --------------------------------------------------
; Problem 1: Multiples of 3 and 5
; Find the sum of all the multiples of 3 or 5 below 1000.
; Expected: 233168
; --------------------------------------------------
(def euler1 ()
  (with (sum 0 i 1)
    (while (< i 1000)
      (if (or (is (mod i 3) 0) (is (mod i 5) 0))
          (= sum (+ sum i)))
      (++ i))
    sum))

; --------------------------------------------------
; Problem 2: Even Fibonacci numbers
; Find the sum of the even-valued terms in the Fibonacci
; sequence whose values do not exceed 4,000,000.
; Expected: 4613732
; --------------------------------------------------
(def euler2 ()
  (with (a 1 b 2 sum 0)
    (while (<= b 4000000)
      (if (is (mod b 2) 0)
          (= sum (+ sum b)))
      (with (next (+ a b))
        (= a b)
        (= b next)))
    sum))

; --------------------------------------------------
; Problem 3: Largest prime factor
; What is the largest prime factor of 600851475143?
; Expected: 6857
; --------------------------------------------------
(def euler3 ()
  (with (n 600851475143 d 2)
    (while (<= (* d d) n)
      (if (is (mod n d) 0)
          (= n (/ n d))
          (++ d)))
    n))

; --------------------------------------------------
; Problem 4: Largest palindrome product
; Find the largest palindrome made from the product
; of two 3-digit numbers.
; Expected: 906609
; --------------------------------------------------
(def rev-int (n)
  (with (r 0 temp n)
    (while (> temp 0)
      (= r (+ (* r 10) (mod temp 10)))
      (= temp (floor (/ temp 10))))
    r))

(def palindrome (n)
  (is n (rev-int n)))

(def euler4 ()
  (with (best 0 a 999)
    (while (>= a 100)
      (let b a
        (while (>= b 100)
          (let prod (* a b)
            (if (and (> prod best) (palindrome prod))
                (= best prod)))
          (-- b)))
      (-- a))
    best))

; --------------------------------------------------
; Problem 5: Smallest multiple
; What is the smallest positive number that is evenly
; divisible by all of the numbers from 1 to 20?
; Expected: 232792560
; --------------------------------------------------
(def gcd (a b)
  (if (is b 0)
      a
      (gcd b (mod a b))))

(def lcm (a b)
  (/ (* a b) (gcd a b)))

(def euler5 ()
  (with (ans 1 i 2)
    (while (<= i 20)
      (= ans (lcm ans i))
      (++ i))
    ans))

; --------------------------------------------------
; Problem 6: Sum square difference
; Find the difference between the sum of the squares
; of the first 100 natural numbers and the square of the sum.
; Expected: 25164150
; --------------------------------------------------
(def euler6 ()
  (with (sum 0 sum-sq 0 i 1)
    (while (<= i 100)
      (= sum (+ sum i))
      (= sum-sq (+ sum-sq (* i i)))
      (++ i))
    (- (* sum sum) sum-sq)))

; --------------------------------------------------
; Problem 7: 10001st prime
; What is the 10001st prime number?
; Expected: 104743
; --------------------------------------------------
(def prime (n)
  (if (< n 2) nil
      (is n 2) t
      (is (mod n 2) 0) nil
      (with (d 3 is-p t)
        (while (and is-p (<= (* d d) n))
          (if (is (mod n d) 0)
              (= is-p nil)
              (= d (+ d 2))))
        is-p)))

(def euler7 ()
  (with (count 1 candidate 3)
    (while (< count 10001)
      (if (prime candidate)
          (++ count))
      (if (< count 10001)
          (= candidate (+ candidate 2))))
    candidate))

; --------------------------------------------------
; Problem 8: Largest product in a series
; Find the thirteen adjacent digits in the 1000-digit
; number that have the greatest product.
; Expected: 23514624000
; --------------------------------------------------
(= euler8-digits
   (+ "73167176531330624919225119674426574742355349194934"
      "96983520312774506326239578318016984801869478851843"
      "85861560789112949495459501737958331952853208805511"
      "12540698747158523863050715693290963295227443043557"
      "66896648950445244523161731856403098711121722383113"
      "62229893423380308135336276614282806444486645238749"
      "30358907296290491560440772390713810515859307960866"
      "70172427121883998797908792274921901699720888093776"
      "65727333001053367881220235421809751254540594752243"
      "52584907711670556013604839586446706324415722155397"
      "53697817977846174064955149290862569321978468622482"
      "83972241375657056057490261407972968652414535100474"
      "82166370484403199890008895243450658541227588666881"
      "16427171479924442928230863465674813919123162824586"
      "17866458359124566529476545682848912883142607690042"
      "24219022671055626321111109370544217506941658960408"
      "07198403850962455444362981230987879927244284909188"
      "84580156166097919133875499200524063689912560717606"
      "05886116467109405077541002256983155200055935729725"
      "71636269561882670428252483600823257530420752963450"))

(def euler8 ()
  (with (best 0
         n (len euler8-digits)
         i 0)
    (while (<= i (- n 13))
      (with (prod 1 j 0)
        (while (< j 13)
          (let digit (- (int (euler8-digits (+ i j))) (int #\0))
            (= prod (* prod digit)))
          (++ j))
        (if (> prod best)
            (= best prod)))
      (++ i))
    best))



; --------------------------------------------------
; Problem 9: Special Pythagorean triplet
; There exists exactly one Pythagorean triplet for which
; a + b + c = 1000. Find the product a * b * c.
; Expected: 31875000
; --------------------------------------------------
(def euler9 ()
  (with (ans 0 a 1)
    (while (< a 333)
      (let b (+ a 1)
        (while (< b 500)
          (let c (- 1000 a b)
            (if (and (> c b) (is (+ (* a a) (* b b)) (* c c)))
                (= ans (* a b c))))
          (++ b)))
      (++ a))
    ans))

; --------------------------------------------------
; Problem 10: Summation of primes
; Find the sum of all the primes below two million.
; Expected: 142913828922
; --------------------------------------------------
(def euler10 ()
  (let limit 2000000
    (with (sieve (newstring limit #\1)
           sum 0
           i 2)
      (= (sieve 0) #\0)
      (= (sieve 1) #\0)
      (while (< i limit)
        (when (is (sieve i) #\1)
          (= sum (+ sum i))
          (let j (* i i)
            (while (< j limit)
              (= (sieve j) #\0)
              (= j (+ j i)))))
        (++ i))
      sum)))

; --------------------------------------------------
; Run and report all 10 problems with timings
; --------------------------------------------------
(def run-problem (num name f expected)
  (let t0 (msec)
    (let ans (f)
      (let t1 (msec)
        (let elapsed (- t1 t0)
          (prn "Problem " num " [" name "]: " ans " (" elapsed " ms)"
               (if (is ans expected) " [PASS]" " [FAIL]")))))))


(run-problem 1  "Multiples of 3 and 5"       euler1  233168)
(run-problem 2  "Even Fibonacci numbers"     euler2  4613732)
(run-problem 3  "Largest prime factor"       euler3  6857)
(run-problem 4  "Largest palindrome product" euler4  906609)
(run-problem 5  "Smallest multiple"          euler5  232792560)
(run-problem 6  "Sum square difference"      euler6  25164150)
(run-problem 7  "10001st prime"              euler7  104743)
(run-problem 8  "Largest series product"     euler8  23514624000)
(run-problem 9  "Pythagorean triplet"        euler9  31875000)
(run-problem 10 "Summation of primes"        euler10 142913828922)

(prn "==================================================")
(prn "All Project Euler problems completed!")
(prn "==================================================")
