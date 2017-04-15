let fibonacci n =
    if n = -1 then -1
    else let rec tail_recursive a b n =
        if n <= 0 then a
        else tail_recursive b (a+b) (n-1)
    in tail_recursive 0 1 n

let test n =
    print_string "Test with n=";
    print_int n;
    print_string ": ";
    print_int (fibonacci n);
    print_char '\n'

let main () =
    test (-1);
    test 0;
    test 1;
    test 2;
    test 3;
    test 4;
    test 5

let () = main ()
