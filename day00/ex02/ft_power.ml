let rec ft_power x y =
    if y = 0
    then 1
    else x * (ft_power x (y-1))

let test x y =
    print_string "Test with [";
    print_int x;
    print_char ' ';
    print_int y;
    print_string "]: ";
    print_int (ft_power x y);
    print_char '\n'

let main () =
    test 2 2;
    test 2 3;
    test 0 2;
    test 2 0

let () = main ()
