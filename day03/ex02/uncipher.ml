let is_lower c = (c >= 'a' && c <= 'z')
let is_upper c = (c >= 'A' && c <= 'Z')

let rot_n s n =
    let n = n mod 26 in
    let rot_char c =
        let add_char c n = char_of_int ((int_of_char c) + n) in
        if is_lower c then
            let rotc = add_char c n in
            if is_lower rotc then rotc
            else add_char rotc (-26)
        else if is_upper c then
            let rotc = add_char c n in
            if is_upper rotc then rotc
            else add_char rotc (-26)
        else
            c
    in String.map rot_char s

(* rot 42 *)

let unrot42 s =
    rot_n s 10

(* caesar *)

let uncaesar s n =
    rot_n s (26 - (n mod 26))

(* xor *)

let xor s n =
    let xor_c c = char_of_int ((int_of_char c) lxor n) in
    String.map xor_c s

(* crypt *)

let rec ft_uncrypt s uncrypts =
    match uncrypts with
    | [] -> s
    | uncrypt :: tail -> ft_uncrypt (uncrypt s 42) tail
