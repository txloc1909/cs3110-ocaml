(* Consult the List standard library to solve these*)

(* Write a function that take an `int list` and returns the fifth element of
 that list, if such element exists. If the list has fewer than 5 elements,
 return 0.*)
let fifth_elem (l:int list) = if List.length l >= 5 then List.nth l 4 else 0


(* Write a function that take an `int list` and returns the list sorted in
 descending order. *)
let desc_sort (l:int list) = List.rev (List.sort Stdlib.compare l)
