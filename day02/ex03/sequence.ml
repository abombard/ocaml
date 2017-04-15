let encode l =
    let rec loop l count res =
        match l with
        | [] -> res
        | node :: [] ->
                loop [] (count+1) (res @ [count+1; node])
        | node :: next :: tail ->
                if node = next
                then loop (next :: tail) (count+1) res
                else loop (next :: tail) 0 (res @ [count+1; node])
    in loop l 0 []

let rec string_from_list ?(s="") l =
    match l with
    | [] -> ""
    | node :: [] -> s ^ (string_of_int node)
    | node :: tail -> string_from_list ~s:(s ^ (string_of_int node)) tail

let sequence n =
    if n < 1 then ""
    else
        let rec loop l i =
            if i = n then string_from_list l
            else loop (encode l) (i+1)
        in loop [1] 1

let rec print_list print_elem l =
    match l with
    | [] -> ()
    | node :: [] ->
            print_string node
    | node :: next ->
            print_elem node;
            print_string " | ";
            print_list print_elem next

let test n =
    print_string "sequence ";
    print_int n;
    print_string ": ";
    print_endline (sequence n)

let () =
    test (-1);
    test 0;
    test 1;
    test 2;
    test 3;
    test 4;
    test 5;
    test 6;
    test 7
