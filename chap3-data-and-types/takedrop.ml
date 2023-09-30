#!/usr/bin/env ocaml

(* Write a function `take: int -> a' list -> a' list` such that `take n lst`
 returns the first `n` elements of `lst`. If `lst` has fewer than n elements,
 returns all of them. *)
let rec take n lst =
    if n = 0 then []
    else match lst with
    | [] -> []
    | x :: xs -> x :: (take (n-1) xs)

(* Make `take` a tail-recursive function.*)
let rec take_rev n lst acc =
    match lst with
    | [] -> []
    | x :: xs -> if n = 0 then acc else take_rev (n-1) xs (x :: acc)

let take_tr n lst = List.rev (take_rev n lst [])

(* Write a function `drop: int -> a' list -> a' list` such that `drop n lst`
 returns all but the first `n` elements of `lst`. If `lst` has fewer than `n`
 elements, return the empty list.*)
let rec drop n lst =
    if n = 0 then lst
    else match lst with
    | [] -> []
    | x :: xs -> drop (n-1) xs

(* `drop` is already tail-recursive*)


(* Making large lists*)
let list_1k = List.init 1000 Fun.id
let list_1M = List.init 1_000_000 Fun.id

let _ = assert ((take 5 list_1k) = [0; 1; 2; 3; 4])
let _ = assert ((drop 995 list_1k) = [995; 996; 997; 998; 999])
let _ = take_tr 1_000_000 list_1M       (* won't overflow the stack *)
