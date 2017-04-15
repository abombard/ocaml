let ft_is_palindrome s =
    let len = (String.length s) - 1 in
    let rec loop i =
        if i >= len then
            true
        else
            if (String.get s i) <> (String.get s (len-i)) then
                false
            else
                loop (i+1)
    in loop 0

let test s =
    print_string "Test with [";
    print_string s;
    print_string "]: ";
    if ft_is_palindrome s
    then print_endline "True"
    else print_endline "False"

let main () =
    test "radar";
    test "madam";
    test "car";
    test ""

let () = main ()
