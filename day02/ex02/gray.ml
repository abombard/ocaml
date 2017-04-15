let rec ft_power x y =
    if y = 0
    then 1
    else x * (ft_power x (y-1))

let gray_encode b =
    b lxor (b lsr 1)

let rec create_string ?(s="") n len =
    if len = 0 then s
    else create_string ~s:(string_of_int (n land 1) ^ s) (n lsr 1) (len-1)

let rec gray n =
    let limit = ft_power 2 n in
    let rec loop i =
        if i < limit then
            let g = gray_encode i in
            let s = create_string g n in
            print_endline s;
            loop (i+1)
    in loop 0

let test n =
    print_string "gray ";
    print_int n;
    print_char '\n';
    gray n

let () =
    test 1;
    test 2;
    test 3
