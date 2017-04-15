let rec ackermann m n =
    if m = 0 then
        n + 1
    else if m > 0 && n = 0 then
        ackermann (m-1) 1
    else if m > 0 && n > 0 then
        ackermann (m-1) (ackermann m (n-1))
    else
        -1

let test m n =
    print_string "Test with m=";
    print_int m;
    print_string " n=";
    print_int n;
    print_string ": ";
    print_int (ackermann m n);
    print_char '\n'

let main () =
    test (-1) 7;
    test 0 0;
    test 2 3;
    test 4 1

let () = main ()
