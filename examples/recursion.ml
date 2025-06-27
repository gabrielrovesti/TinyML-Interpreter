(* Recursive Function Examples *)
(* Demonstrates let rec and recursive programming patterns *)

(* Classic factorial function *)
let rec factorial = λn.
  if n = 0 then 1
  else n * factorial (n - 1)
in factorial 5;;  (* Should be 120 *)

(* Fibonacci sequence *)
let rec fibonacci = λn.
  if n <= 1 then n
  else fibonacci (n - 1) + fibonacci (n - 2)
in fibonacci 7;;  (* Should be 13 *)

(* Tail-recursive factorial for better performance *)
let rec factorial_tail = λacc.λn.
  if n = 0 then acc
  else factorial_tail (acc * n) (n - 1)
in factorial_tail 1 5;;  (* Should be 120 *)

(* Tail-recursive Fibonacci *)
let rec fib_tail = λa.λb.λn.
  if n = 0 then a
  else if n = 1 then b
  else fib_tail b (a + b) (n - 1)
in fib_tail 0 1 7;;  (* Should be 13 *)

(* Greatest Common Divisor using Euclidean algorithm *)
let rec gcd = λa.λb.
  if b = 0 then a
  else gcd b (a % b)
in gcd 48 18;;  (* Should be 6 *)

(* Power function *)
let rec power = λbase.λexp.
  if exp = 0 then 1
  else if exp = 1 then base
  else base * power base (exp - 1)
in power 2 10;;  (* Should be 1024 *)

(* Sum of numbers from 1 to n *)
let rec sum_to_n = λn.
  if n = 0 then 0
  else n + sum_to_n (n - 1)
in sum_to_n 10;;  (* Should be 55 *)

(* Tail-recursive sum *)
let rec sum_tail = λacc.λn.
  if n = 0 then acc
  else sum_tail (acc + n) (n - 1)
in sum_tail 0 100;;  (* Should be 5050 *)

(* Ackermann function (grows very fast!) *)
let rec ackermann = λm.λn.
  if m = 0 then n + 1
  else if n = 0 then ackermann (m - 1) 1
  else ackermann (m - 1) (ackermann m (n - 1))
in ackermann 3 2;;  (* Should be 29 *)

(* Count down to zero *)
let rec countdown = λn.
  if n = 0 then 0
  else countdown (n - 1)
in countdown 1000;;  (* Should be 0 *)

(* Simulate mutual recursion with nested let rec *)
let rec even = λn.
  if n = 0 then true
  else let rec odd = λm.
    if m = 0 then false
    else even (m - 1)
  in odd (n - 1)
in even 42;;  (* Should be true *)

(* Collatz conjecture (3n+1 problem) *)
let rec collatz_steps = λn.
  if n = 1 then 0
  else if n % 2 = 0 then 1 + collatz_steps (n / 2)
  else 1 + collatz_steps (3 * n + 1)
in collatz_steps 7;;  (* Should be 16 *)

(* Binary search tree operations (simplified) *)
(* Since we don't have algebraic data types, we'll use a simple approach *)

(* Check if a number is prime (trial division) *)
let rec is_prime_helper = λn.λd.
  if d * d > n then true
  else if n % d = 0 then false
  else is_prime_helper n (d + 1)
in
let is_prime = λn.
  if n < 2 then false
  else is_prime_helper n 2
in is_prime 17;;  (* Should be true *)

(* Find nth prime number *)
let rec nth_prime = λcount.λcandidate.λtarget.
  if count = target then candidate - 1
  else let is_prime = λn.
    let rec check = λd.
      if d * d > n then true
      else if n % d = 0 then false
      else check (d + 1)
    in if n < 2 then false else check 2
  in
  if is_prime candidate then nth_prime (count + 1) (candidate + 1) target
  else nth_prime count (candidate + 1) target
in nth_prime 0 2 10;;  (* Should be 29, the 10th prime *)

(* Digital root calculation *)
let rec digital_root = λn.
  if n < 10 then n
  else digital_root ((n % 10) + (n / 10))
in digital_root 9875;;  (* Should be 2 *)

(* Recursive function that computes sum of digits *)
let rec sum_digits = λn.
  if n = 0 then 0
  else (n % 10) + sum_digits (n / 10)
in sum_digits 12345;;  (* Should be 15 *)

(* Tower of Hanoi - count number of moves *)
let rec hanoi = λn.
  if n = 1 then 1
  else 2 * hanoi (n - 1) + 1
in hanoi 4;;  (* Should be 15 *)

(* Function to reverse a number *)
let rec reverse_number = λn.λacc.
  if n = 0 then acc
  else reverse_number (n / 10) (acc * 10 + n % 10)
in reverse_number 12345 0;;  (* Should be 54321 *)

(* Recursive function to find the maximum in a list (Church encoding) *)
let rec max_list = λlst.λcurrent_max.
  if lst = nil then current_max
  else let head = fold_right (λx.λy.x) lst in
       let tail = fold_right (λx.λy.y) lst in
       max_list tail (if head > current_max then head else current_max)
in
let test_list = cons 3 (cons 1 (cons 4 (cons 1 (cons 5 (cons 9 nil))))) in
max_list test_list 0;;  (* Should be 9 *)   

(* Recursive function to flatten a nested list (Church encoding) *) 
let rec flatten = λlst.
  if lst = nil then nil
  else let head = fold_right (λx.λy.cons x y) lst in
       let tail = fold_right (λx.λy.y) lst in
       cons head (flatten tail)
in
let nested_list = cons (cons 1 (cons 2 nil)) (cons (cons 3 nil) (cons (cons 4 (cons 5 nil)) nil)) in
flatten nested_list;;  (* Should be (1, 2, 3, 4, 5) *)

(* Recursive function to compute the length of a nested list (Church encoding) *)
let rec nested_length = λlst.
  if lst = nil then 0
  else let head = fold_right (λx.λy.y + 1) lst in
       let tail = fold_right (λx.λy.y) lst in
       head + nested_length tail
in
let nested_list = cons (cons 1 (cons 2 nil)) (cons (cons 3 nil) (cons (cons 4 (cons 5 nil)) nil)) in
nested_length nested_list;;  (* Should be 5 *)
