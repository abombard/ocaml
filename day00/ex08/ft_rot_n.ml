let is_alpha c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let add_char c n =
    char_of_int ((int_of_char c) + n)

let ft_rot_n n s =
    let n = n mod 26 in
    let rot_char c =
        if not (is_alpha c) then c
        else
            let c = add_char c n in
            if is_alpha c then
                c
            else
                add_char c (-26)
    in String.map rot_char s

let test n s =
    print_string "Test with [";
    print_string s;
    print_string "] rot ";
    print_int n;
    print_string ": ";
    print_endline (ft_rot_n n s)

let main () =
    test 1 "abcdefghijklmnopqrstuvwxyz";
    test 13 "abcdefghijklmnopqrstuvwxyz";
    test 42 "0123456789";
    test 2 "OI2EAS67B9";
    test 0 "Damned !";
    test 42 "";
    test 1 "NBzlk qnbjr !";
    test 42 "abcdefghijklmnopqrstuvwxyz"

let () = main ()
