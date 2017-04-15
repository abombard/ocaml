let rec display_cards l toString =
    match l with
    | [] -> print_endline ""
    | card :: tail ->
            print_string (toString card);
            print_string " ";
            display_cards tail toString

let () =
    display_cards Color.all Color.toString;
    display_cards Color.all Color.toStringVerbose
