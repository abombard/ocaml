let repeat_x n =
    if n < 0 then
        "Error"
    else
        let rec loop n acc =
            if n = 0 then
                acc
            else
                loop (n-1) (acc ^ "x")
        in loop n ""

let test n =
    print_string "Test with [";
    print_int n;
    print_string "]: ";
    print_endline (repeat_x n)

let main () =
    test 0;
    test 1;
    test 2;
    test 5;
    test 42;
    test (-1);
    test (-12)

let () = main ()
