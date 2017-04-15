let repeat_string ?(str="x") n =
    if n < 0 then
        "Error"
    else
        let rec loop n acc =
            if n = 0 then
                acc
            else
                loop (n-1) (acc ^ str)
        in loop n ""

let test_no_string n =
    print_string "Test with n=";
    print_int n;
    print_string ": ";
    print_endline (repeat_string n)

let test str n =
    print_string "Test with str=";
    print_string str;
    print_string " n=";
    print_int n;
    print_string ": ";
    print_endline (repeat_string ~str:str n)

let main () =
    test_no_string (-1);
    test_no_string 0;
    test_no_string 1;
    test_no_string 2;
    test "Hello" 3;
    test "Hello" (-1);
    test "" 3;
    test "a" 0

let () = main ()
