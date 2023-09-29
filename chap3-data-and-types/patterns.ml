(* Using pattern matching, write three functions, for each of the following
 * properties:
 * * The list's first element is "bigred"
 * * The list has exactly two or four elements; do not use the `length` function
 * * The first two elements of the list are equal
 *)

let first_bigred l = match l with
    | [] -> false
    | x :: _ -> (x = "bigred")


let len_2or4 l = match l with
    | _ :: _ :: [] -> true
    | _ :: _ :: _ :: _ :: [] -> true
    | _ -> false


let first_two_equal l = match l with
    | x1 :: x2 :: _ -> (x1 = x2)
    | _ -> false
