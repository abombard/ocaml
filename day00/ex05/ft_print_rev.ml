let ft_print_rev s =
    let rec loop i =
        if i >= 0 then begin
            print_char (String.get s i);
            loop (i-1)
        end
    in loop ((String.length s)-1);
    print_char '\n'

let main () =
    ft_print_rev "Hello world !";
    ft_print_rev "toto";
    ft_print_rev ""

let () = main ()
