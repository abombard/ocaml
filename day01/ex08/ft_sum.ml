let ft_sum f start limit =
    if limit < start then nan
    else let rec loop i acc =
        if i = limit then acc
        else loop (i+1) (acc +. (f i))
    in loop start (float_of_int 0)

let main () =
    print_string "Test with (f x -> x + 1) 1 10: ";
    print_float (ft_sum (fun x -> float_of_int (x + 1)) 1 10);
    print_char '\n';

    print_string "Test with (f x -> x * x) 1 10: ";
    print_float (ft_sum (fun x -> float_of_int (x * x)) 1 10);
    print_char '\n';

    print_string "Test with (f x -> x * x) 1 0: ";
    print_float (ft_sum (fun x -> float_of_int (x * x)) 1 0);
    print_char '\n';

    print_string "Test with (f x -> x + x) 1 5: ";
    print_float (ft_sum (fun x -> float_of_int (x + x)) 1 5);
    print_char '\n'

let () = main ()
