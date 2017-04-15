let test_card card =
    print_endline "=== new Card ===";
    print_endline (Card.toString card);
    print_endline (Card.toStringVerbose card);
    Printf.printf
        "value %s color %s\n"
        (Card.Value.toStringVerbose (Card.getValue card))
        (Card.Color.toStringVerbose (Card.getColor card))

let test () =
    print_endline "== General tests ==";
    let card1 = Card.newCard Card.Value.T2 Card.Color.Heart in
    let card2 = Card.newCard Card.Value.As Card.Color.Club in
    let card3 = Card.newCard Card.Value.Queen Card.Color.Spade in
    Printf.printf
        "compare %s %s = %d\n"
        (Card.toStringVerbose card1)
        (Card.toStringVerbose card2)
        (Card.compare card1 card2);
    Printf.printf
        "compare %s %s = %d\n"
        (Card.toStringVerbose card3)
        (Card.toStringVerbose card1)
        (Card.compare card2 card1);
    Printf.printf
        "compare %s %s = %d\n"
        (Card.toStringVerbose card1)
        (Card.toStringVerbose card1)
        (Card.compare card1 card1);
   Printf.printf
        "min %s %s = %s\n"
        (Card.toStringVerbose card1)
        (Card.toStringVerbose card2)
        (Card.toStringVerbose (Card.min card1 card2));
   Printf.printf
        "min %s %s = %s\n"
        (Card.toStringVerbose card1)
        (Card.toStringVerbose card1)
        (Card.toStringVerbose (Card.min card1 card1));
   Printf.printf
        "max %s %s = %s\n"
        (Card.toStringVerbose card1)
        (Card.toStringVerbose card2)
        (Card.toStringVerbose (Card.max card1 card2));
   Printf.printf
        "best %s %s = %s\n"
        (Card.toStringVerbose card1)
        (Card.toStringVerbose card2)
        (Card.toStringVerbose (Card.best [card1 ;card2; card3]));
   Printf.printf
        "%s isOf Heart ? %s \n"
        (Card.toStringVerbose card1)
        (string_of_bool (Card.isOf card1 Card.Color.Heart));
   Printf.printf
        "%s isOf Heart ? %s\n"
        (Card.toStringVerbose card2)
        (string_of_bool (Card.isOf card2 Card.Color.Heart));
    Printf.printf
        "%s isSpade ? %s\n"
        (Card.toStringVerbose card3)
        (string_of_bool (Card.isSpade card3));
    Printf.printf
        "%s isSpade ? %s\n"
        (Card.toStringVerbose card2)
        (string_of_bool (Card.isSpade card2))

let main () =
    List.iter test_card Card.all;
    test ()

let () = main ()
