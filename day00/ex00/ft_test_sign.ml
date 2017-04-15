let ft_test_sign x =
    if x >= 0
    then print_endline "positive"
    else print_endline "negative"

let test x =
    print_string "Test with [";
    print_int x;
    print_string "]: ";
    ft_test_sign x

let main () =
    test 1;
    test 0;
    test (-42)

let () = main ()
