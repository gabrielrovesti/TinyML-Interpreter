(* Basic TinyML Examples *)
(* This file demonstrates fundamental language constructs *)

(* Simple literals *)
42;;
3.14;;
true;;
"Hello, TinyML!";;
'a';;
();;

(* Identity function - the most basic lambda *)
let id = λx.x in id;;

(* Test polymorphic identity *)
let id = λx.x in 
let a = id 42 in
let b = id true in
let c = id "hello" in
(a, b, c);;

(* Constant functions *)
let const = λx.λy.x in const 42 true;;

(* Function composition *)
let compose = λf.λg.λx.f (g x) in
let succ = λx.x + 1 in
let double = λx.x * 2 in
let succ_double = compose succ double in
succ_double 5;;  (* Should be 11: succ(double(5)) = succ(10) = 11 *)

(* Arithmetic expressions *)
1 + 2 * 3 - 4;;  (* Should be 3 *)
(1 + 2) * (3 - 4);;  (* Should be -3 *)

(* Boolean logic *)
true && false;;
true || false;;
not true;;
not false;;

(* Comparison operations *)
5 > 3;;
10 <= 10;;
"abc" = "abc";;
"abc" < "def";;

(* Conditional expressions *)
if true then "yes" else "no";;
if 5 > 3 then 42 else 24;;

(* Let bindings with lexical scoping *)
let x = 10 in
let f = λy.x + y in
let x = 20 in
f 5;;  (* Should be 15, not 25, due to lexical scoping *)

(* Nested let expressions *)
let x = 1 in
let y = let z = 2 in z + 3 in
x + y;;  (* Should be 6 *)

(* Tuples *)
(1, 2);;
(true, "hello", 42);;
((1, 2), (3, 4));;

(* Higher-order functions *)
let twice = λf.λx.f (f x) in
let increment = λx.x + 1 in
twice increment 5;;  (* Should be 7 *)

(* Currying demonstration *)
let add = λx.λy.x + y in
let add5 = add 5 in
add5 10;;  (* Should be 15 *)

(* Church encoding of booleans *)
let true_church = λx.λy.x in
let false_church = λx.λy.y in
let if_church = λp.λx.λy.p x y in
if_church true_church "yes" "no";;

(* Simple recursive function would go in recursion.ml *)

(* Sequence expressions *)
1; 2; 3;;
let x = (print_int 42; 24) in x;;