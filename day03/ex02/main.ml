(* rot 42 *)

let test_rot42 s =
    print_string "rot42 ";
    print_string s;
    print_string " -> ";
    let res = Cipher.rot42 s in
    print_endline res;
    print_string "unrot42 ";
    print_string res;
    print_string " -> ";
    print_endline (Uncipher.unrot42 res)

(* caesar *)

let test_caesar s n =
    print_string "caesar ";
    print_string s;
    print_string " ";
    print_int n;
    print_string " -> ";
    let res = Cipher.caesar s n in
    print_endline res;
    print_string "uncaesar ";
    print_string res;
    print_string " ";
    print_int n;
    print_string " -> ";
    print_endline (Uncipher.uncaesar res n)

(* xor *)

let test_xor s n =
    print_string "xor ";
    print_string s;
    print_string " ";
    print_int n;
    print_string " -> ";
    let res = Cipher.xor s n in
    print_endline res;
    print_string "unxor ";
    print_string res;
    print_string " ";
    print_int n;
    print_string " -> ";
    print_endline (Uncipher.xor res n)

(* crypt *)

let test_crypt s crypts uncrypts =
    print_string "ft_crypt ";
    print_string s;
    print_string " -> ";
    let res = Cipher.ft_crypt s crypts in
    print_endline res;
    print_string "ft_uncrypt ";
    print_string res;
    print_string " -> ";
    print_endline (Uncipher.ft_uncrypt res uncrypts)

(* main *)

let () =
    test_rot42 "toto";
    test_rot42 "Salut les loulous";
    test_caesar "abcdefghijklmnopqrstuvwxyz" 1;
    test_caesar "abcdefghijklmnopqrstuvwxyz" 42;
    test_xor "Hello world!" 42;
    test_crypt "Hello world!"
        [Cipher.xor; Cipher.caesar; Cipher.caesar; Cipher.xor; Cipher.caesar]
        [Uncipher.uncaesar; Uncipher.xor; Uncipher.uncaesar; Uncipher.uncaesar; Uncipher.xor]

