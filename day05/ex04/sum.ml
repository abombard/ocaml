let sum x y = x +. y

let test x y =
    Printf.printf "%f + %f = %f\n" x y (sum x y)

let () =
    test 0.01 0.02;
    test 12.1 12.148
    (* test 1 2 *)
