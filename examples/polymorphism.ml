(* Polymorphic Function Examples *)
(* Demonstrates parametric polymorphism and the Hindley-Milner type system *)

(* The identity function - simplest polymorphic function *)
(* Type: 'a -> 'a *)
let id = λx.x in id;;

(* Test polymorphic instantiation *)
let id = λx.x in
let int_result = id 42 in
let bool_result = id true in
let string_result = id "hello" in
(int_result, bool_result, string_result);;

(* Constant function - ignores second argument *)
(* Type: 'a -> 'b -> 'a *)
let const = λx.λy.x in const;;

(* Apply function - applies function to argument *)
(* Type: ('a -> 'b) -> 'a -> 'b *)
let apply = λf.λx.f x in apply;;

(* Function composition *)
(* Type: ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c *)
let compose = λf.λg.λx.f (g x) in compose;;

(* Test composition with different types *)
let compose = λf.λg.λx.f (g x) in
let succ = λx.x + 1 in
let is_even = λx.(x % 2) = 0 in
let succ_then_check = compose is_even succ in
succ_then_check 5;;  (* true, because 6 is even *)

(* Flip function - swaps argument order *)
(* Type: ('a -> 'b -> 'c) -> 'b -> 'a -> 'c *)
let flip = λf.λx.λy.f y x in flip;;

(* Test flip with subtraction *)
let flip = λf.λx.λy.f y x in
let sub = λx.λy.x - y in
let flipped_sub = flip sub in
flipped_sub 3 10;;  (* 10 - 3 = 7 *)

(* Curry function - converts binary function to curried form *)
(* Type: (('a * 'b) -> 'c) -> 'a -> 'b -> 'c *)
let curry = λf.λx.λy.f (x, y) in curry;;

(* Uncurry function - converts curried function to binary form *)
(* Type: ('a -> 'b -> 'c) -> ('a * 'b) -> 'c *)
let uncurry = λf.λp.let (x, y) = p in f x y in uncurry;;

(* First projection from pair *)
(* Type: ('a * 'b) -> 'a *)
let fst = λp.let (x, y) = p in x in fst;;

(* Second projection from pair *)
(* Type: ('a * 'b) -> 'b *)
let snd = λp.let (x, y) = p in y in snd;;

(* Swap pair elements *)
(* Type: ('a * 'b) -> ('b * 'a) *)
let swap = λp.let (x, y) = p in (y, x) in swap;;

(* Duplicate value into pair *)
(* Type: 'a -> ('a * 'a) *)
let dup = λx.(x, x) in dup;;

(* Church numerals - polymorphic encoding of natural numbers *)
(* Type: ('a -> 'a) -> 'a -> 'a *)
let zero = λf.λx.x in zero;;
let one = λf.λx.f x in one;;
let two = λf.λx.f (f x) in two;;
let three = λf.λx.f (f (f x)) in three;;

(* Church numeral successor *)
(* Type: (('a -> 'a) -> 'a -> 'a) -> ('a -> 'a) -> 'a -> 'a *)
let succ_church = λn.λf.λx.f (n f x) in succ_church;;

(* Church numeral addition *)
(* Type: (('a -> 'a) -> 'a -> 'a) -> (('a -> 'a) -> 'a -> 'a) -> ('a -> 'a) -> 'a -> 'a *)
let add_church = λm.λn.λf.λx.m f (n f x) in add_church;;

(* Convert Church numeral to integer *)
let to_int = λn.n (λx.x + 1) 0 in
to_int three;;  (* Should be 3 *)

(* Church boolean encoding *)
(* Type: 'a -> 'b -> 'a *)
let true_church = λx.λy.x in true_church;;
(* Type: 'a -> 'b -> 'b *)
let false_church = λx.λy.y in false_church;;

(* Church boolean conditional *)
(* Type: ('a -> 'b -> 'c) -> 'a -> 'b -> 'c *)
let if_then_else = λp.λx.λy.p x y in if_then_else;;

(* Test Church booleans *)
let if_then_else = λp.λx.λy.p x y in
let true_church = λx.λy.x in
if_then_else true_church "yes" "no";;

(* Higher-order functions *)

(* Twice function - applies function twice *)
(* Type: ('a -> 'a) -> 'a -> 'a *)
let twice = λf.λx.f (f x) in twice;;

(* Iterate function n times *)
(* In full ML, this would be recursive, but here's the concept *)
let twice = λf.λx.f (f x) in
let thrice = λf.λx.f (f (f x)) in
let quad = λf.λx.twice (twice f) x in
quad (λx.x + 1) 0;;  (* Should be 4 *)

(* Polymorphic option type simulation *)
(* Since we don't have algebraic data types, we use Church encoding *)

(* None constructor *)
(* Type: 'a -> 'b -> 'b *)
let none = λsome_func.λnone_val.none_val in none;;

(* Some constructor *)
(* Type: 'a -> ('a -> 'b -> 'b) -> 'b -> 'b *)
let some = λval.λsome_func.λnone_val.some_func val in some;;

(* Option map *)
let option_map = λf.λopt.opt (λval.some (f val)) none in option_map;;

(* Test option type *)
let none = λsome_func.λnone_val.none_val in
let some = λval.λsome_func.λnone_val.some_func val in
let opt_val = some 42 in
let extract = λopt.opt (λx.x) 0 in
extract opt_val;;  (* Should be 42 *)

(* Polymorphic list operations (Church encoding) *)

(* Empty list *)
(* Type: 'a -> ('b -> 'a -> 'a) -> 'a *)
let nil = λnil_val.λcons_func.nil_val in nil;;

(* Cons operation *)
(* Type: 'a -> (('a -> 'b -> 'b) -> 'b -> 'b) -> ('a -> 'b -> 'b) -> 'b -> 'b *)
let cons = λhead.λtail.λcons_func.λnil_val.cons_func head (tail cons_func nil_val) in cons;;

(* List folding (right fold) *)
let fold_right = λcons_func.λnil_val.λlist.list cons_func nil_val in fold_right;;

(* List length *)
let length = λlist.fold_right (λx.λacc.acc + 1) 0 list in
let test_list = cons 1 (cons 2 (cons 3 nil)) in
length test_list;;  (* Should be 3 *)

(* Fixed-point combinators (simplified) *)

(* Self-application *)
(* Type: ('a -> 'b) -> 'b where 'a = ('a -> 'b) *)
(* This would cause infinite type in standard ML, but demonstrates the concept *)

(* Polymorphic recursion patterns *)

(* Generic comparison function type *)
let make_comparator = λeq_func.λlt_func.λx.λy.
  if eq_func x y then 0
  else if lt_func x y then -1
  else 1
in make_comparator;;

(* Use with integers *)
let int_eq = λx.λy.x = y in
let int_lt = λx.λy.x < y in
let int_cmp = make_comparator int_eq int_lt in
int_cmp 5 3;;  (* Should be 1 *)

(* Polymorphic tuple operations *)

(* Map over pair *)
(* Type: ('a -> 'c) -> ('b -> 'd) -> ('a * 'b) -> ('c * 'd) *)
let map_pair = λf.λg.λp.let (x, y) = p in (f x, g y) in map_pair;;

(* Test map_pair *)
let map_pair = λf.λg.λp.let (x, y) = p in (f x, g y) in
let double = λx.x * 2 in
let negate = λx.not x in
map_pair double negate (21, true);;  (* Should be (42, false) *)

(* Polymorphic conditional *)
(* Type: bool -> 'a -> 'a -> 'a *)
let poly_if = λcond.λthen_val.λelse_val.
  if cond then then_val else else_val
in poly_if;;

(* Test with different types *)
let poly_if = λcond.λthen_val.λelse_val.
  if cond then then_val else else_val in
let result1 = poly_if true 42 24 in
let result2 = poly_if false "yes" "no" in
(result1, result2);;  (* Should be (42, "no") *)