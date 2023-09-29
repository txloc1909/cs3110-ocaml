(* Consult the List standard library to solve these*)

(* Write a function that take an `int list` and returns the fifth element of
 that list, if such element exists. If the list has fewer than 5 elements,
 return 0.*)
let fifth_elem (l:int list) = if List.length l >= 5 then List.nth l 4 else 0


(* Write a function that take an `int list` and returns the list sorted in
 descending order. *)
let desc_sort (l:int list) = List.rev (List.sort Stdlib.compare l)

(* Write a function that returns the last element of a list. May assume that
 the list is non-empty. Do not write any pattern matching. *)
let last_elem l = List.nth l ((List.length l) - 1)

(* Write a function `any_zeroes: int list -> bool` that return true if and
 only if the input list contains at least one zero. Use one library function.
 Do not write any pattern matching. *)
let any_zeroes (l:int list) = List.exists (fun x -> x = 0) l
