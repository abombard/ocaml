let rec converges f x n =
    if n < 0 then false
    else if (f x) = x then true
    else converges f (f x) (n-1)

let test f x n =
   if converges f x n
   then print_endline "True"
   else print_endline "False"

let main () =
    test (fun x -> x * x) 1 (-1);
    test (fun x -> x * x) 1 5;
    test (( * ) 2) 2 5;
    test (( * ) 2) 0 3;
    test (fun x -> x / 2) 2 3;
    test (fun x -> x / 2) 2 1;
    test (fun x -> x mod 2) 5 3

let () = main ()
