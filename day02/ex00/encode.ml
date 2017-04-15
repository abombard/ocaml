let encode l =
    let rec loop l count res =
        match l with
        | [] -> res
        | node :: [] ->
                loop [] (count+1) (res @ [(count+1, node)])
        | node :: next :: tail ->
                if node = next
                then loop (next :: tail) (count+1) res
                else loop (next :: tail) 0 (res @ [(count+1, node)])
    in loop l 0 []

let rec print_tuple_string_list l =
    match l with
    | [] -> ()
    | node::next ->
            let (n, s) = node in
            print_int n;
            print_char ':';
            print_string s;
            print_string "; ";
            print_tuple_string_list next

let rec print_string_list l =
    match l with
    | [] -> ()
    | node :: [] ->
            print_string node
    | node :: next ->
            print_string node;
            print_string "|";
            print_string_list next

let rec print_tuple_int_list l =
    match l with
    | [] -> ()
    | node::next ->
            let (n, i) = node in
            print_int n;
            print_char ':';
            print_int i;
            print_string "|";
            print_tuple_int_list next

let rec print_int_list l =
    match l with
    | [] -> ()
    | node :: [] ->
            print_int node
    | node :: next ->
            print_int node;
            print_string "|";
            print_int_list next

let test_string l =
    print_endline "Test with string list: ";
    print_string_list l;
    print_string "\n-> ";
    print_tuple_string_list (encode l);
    print_char '\n'

let test_int l =
    print_endline "Test with int list: ";
    print_int_list l;
    print_string "\n-> ";
    print_tuple_int_list (encode l);
    print_char '\n'

let () =
    test_string ["a"; "a"; "a"; "b"; "b"; "b"];
    test_string [""; "a"; "a"; "b"; "b"; "b"];
    test_string ["a"; "b"; "b"; "c"; "c"; "d"];
    test_string ["a"; "b"; "b"; "c"; "c"; "d"];
    test_int [1; 1; 0; 2; 3; 3; 3; 3];
    test_string [];
    test_int []
