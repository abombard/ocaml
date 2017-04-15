let ft_print_alphabet () =
    let start = int_of_char 'a' in
    let limit = int_of_char 'z' in
    let rec loop pos =
        if pos <= limit
        then
            begin
                print_char (char_of_int pos);
                loop (pos+1)
            end
     in
     loop start;
     print_char '\n'

let main () =
    ft_print_alphabet ()

let () = main ()
