let crossover l1 l2 =
    let rec find l cmp =
        match l with
        | [] -> false
        | node :: [] ->
                node = cmp
        | node :: tail ->
                node = cmp || find tail cmp
    in
    let rec loop l1 l2 res =
        match l1 with
        | [] -> res
        | node :: [] ->
                if find l2 node
                then loop [] l2 (res @ [node])
                else loop [] l2 res
        | node :: tail ->
                if find l2 node
                then loop tail l2 (res @ [node])
                else loop tail l2 res
    in loop l1 l2 []

let rec print_list print_elem l =
    match l with
    | [] -> ()
    | node :: [] ->
            print_string node
    | node :: next ->
            print_elem node;
            print_string " | ";
            print_list print_elem next

let test l1 l2 =
    print_endline "Test with";
    print_string "L1: ";
    print_list print_string l1;
    print_char '\n';
    print_string "L2: ";
    print_list print_string l2;
    print_string "\n-> ";
    print_list print_string (crossover l1 l2);
    print_char '\n'

let () =
    test ["hello"; "world"] ["world"];
    test ["hello"; "world"] [];
    test [] ["hello"; "world"];
    test ["a"; "b"; "c"; "d"; "e"] ["a"; "c"; "e"]
