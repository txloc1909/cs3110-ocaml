(* define an infix operator +/. to compute the average of two floating-point
   numbers *)

let (+/.) a b = (a +. b) /. 2.0


let _ = assert ((1.0 +/. 2.0) = 1.5)
let _ = assert ((0.0 +/. 0.0) = 0.0)
