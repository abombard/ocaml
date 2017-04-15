let print_int_2 x =
    if x < 10 then
        print_int 0;
    print_int x

let display x y =
    print_int_2 x;
    print_char ' ';
    print_int_2 y;
    if not (x = 98 && y = 99) then
        print_char ',';
        print_char ' '

let ft_print_comb2 () =
    let rec loop x y =
        if x < 100 then begin
            if x < y then
                display x y;
            if y = 99 then
                loop (x+1) 0
            else
                loop x (y+1)
        end
    in loop 0 0;
    print_char '\n'

let main () =
    ft_print_comb2 ()

let () = main ()
