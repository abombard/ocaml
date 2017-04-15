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

let rot42 s =
    rot_n s 42

(* caesar *)

let caesar s n =
    rot_n s n

(* xor *)

let xor s n =
    let xor_c c = char_of_int ((int_of_char c) lxor n) in
    String.map xor_c s

(* crypt *)

let rec ft_crypt s crypts =
    match crypts with
    | [] -> s
    | crypt :: tail -> ft_crypt (crypt s 42) tail

