let test_card card =
    print_string "Card: ";
    print_string (Value.toString card);
    print_string " ";
    print_string (Value.toStringVerbose card);
    print_string " | toInt: ";
    print_int (Value.toInt card);
    print_string " next: ";
    print_string (Value.toStringVerbose (Value.next card));
    print_string " previous: ";
    print_endline (Value.toStringVerbose (Value.previous card))

let rec test card_list =
    match card_list with
    | [] -> ()
    | card :: tail -> test_card card; test tail

let () =
    match Value.all with
    | first :: cards -> test cards
    | _ -> print_endline "Value.all Invalid"
