(* Write a function `is_unimodal : int list -> bool` that takes an integer list
   and returns whether that list is unimodal. An unimodal list is a list that
   monotonically increases to some maximum value then monotonically decreases
   after that value. Either or both segments (is_increasing or decreasing) may
   be empty. A constant list is unimodal, as is the empty list.*)


let rec unimodal (p:int) (lst:int list) (is_incr:bool) =
    match lst with
    | [] -> true
    | x :: xs ->
        if is_incr then
            if p <= x then unimodal x xs true
            else unimodal x xs false
        else
            if p >= x then unimodal x xs false
            else false


let is_unimodal (lst:int list) =
    match lst with
    | [] -> true
    | _ :: [] -> true
    | x1 :: x2 :: xs -> if x1 <= x2 then unimodal x2 xs true
                        else unimodal x2 xs false


let _ = assert (is_unimodal [])
let _ = assert (is_unimodal [1])
let _ = assert (is_unimodal [1; 1; 1])
let _ = assert (is_unimodal [1; 2; 3; 2; 1])
let _ = assert (is_unimodal [1; 2; 3; 4])
let _ = assert (is_unimodal [4; 3; 2; 1])
let _ = assert (is_unimodal [1; 2; 2; 3; 4; 4; 2; 1])
let _ = assert (not (is_unimodal [1; 2; 3; 2; 4; 5; 1]))
