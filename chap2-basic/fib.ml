(* naive fibonacci implementation *)
(** [fib n] is the n-th Fibonacci number. *)
let rec fib n:int = if n <= 2 then 1 else fib (n - 1) + fib (n - 2)



(* linear implementation *)
let rec fib_aux (n:int) (pp:int) (p:int) =
    if n < 2 then p else fib_aux (n-1) p (pp+p)

let fib_fast n:int = fib_aux n 0 1

let _ = assert ((fib 1) = (fib_fast 1))
let _ = assert ((fib 2) = (fib_fast 2))
let _ = assert ((fib 3) = (fib_fast 3))
let _ = assert ((fib 4) = (fib_fast 4))
let _ = assert ((fib 5) = (fib_fast 5))
let _ = assert ((fib 6) = (fib_fast 6))
let _ = assert ((fib 7) = (fib_fast 7))
let _ = assert ((fib 8) = (fib_fast 8))
let _ = assert ((fib 9) = (fib_fast 9))
let _ = assert ((fib 20) = (fib_fast 20))
let _ = assert ((fib 30) = (fib_fast 30))


(* Calculate first value of [n] for which [fib_fast n] is negative, indicating
 * integer overflow occurred *)

(** [first_neg func n0] return the first integer [n0] where [func n0] is
 ** negative *)
let rec first_neg func n0 =
    let res = func n0 in
        if res < 0 then n0
        else first_neg func (n0 + 1)

let _ = print_endline (string_of_int (first_neg fib_fast 1))
