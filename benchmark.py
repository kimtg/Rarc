import sys
import time

n = int(sys.argv[1]) if len(sys.argv) > 1 else 34

def fib(n):
    if n < 2:
        return n
    return fib(n - 1) + fib(n - 2)

t0 = time.perf_counter()
ans = fib(n)
t1 = time.perf_counter()
elapsed = (t1 - t0) * 1000.0

print(f"{ans},{elapsed:.2f}")
