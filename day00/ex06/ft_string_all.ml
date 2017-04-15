let ft_string_all func s =
    let len = String.length s in
    let rec loop s i =
        if i = len then
            true
        else begin
            if func (String.get s i) then
                loop s (i+1)
            else
                false
        end
    in loop s 0

let is_digit c =
    c >= '0' && c <= '9'

let test s =
    print_string "Test with [";
    print_string s;
    print_string "]: ";
    if ft_string_all is_digit s
    then print_endline "True"
    else print_endline "False"

let main () =
    test "0123456789";
    test "0123toto";
    test ""

let () = main ()
