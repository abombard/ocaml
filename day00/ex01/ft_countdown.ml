let rec ft_countdown x =
    if x <= 0
    then print_endline "0"
    else
    begin
        print_int x;
        print_char '\n';
        ft_countdown (x-1)
    end

let test x =
    print_string "test with [";
    print_int x;
    print_endline "]:";
    ft_countdown x

let main () =
    test 0;
    test 3;
    test (-1)

let () = main ()
