let display x y z =
    print_int x;
    print_int y;
    print_int z;
    if not (x = 7 && y = 8 && z = 9) then
        print_string ", "

let ft_print_comb () =
    let rec loop x y z =
        if x <= 9 then begin
            if x < y && y < z then
                display x y z;
            if z = 9 then
                if y = 9
                then loop (x+1) 0 0
                else loop x (y+1) 0
            else loop x y (z+1)
        end
    in loop 0 0 0;
    print_char '\n'

let main () =
    ft_print_comb ()

let () = main ()
