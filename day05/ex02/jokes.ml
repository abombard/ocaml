let () =
    Random.self_init ();
    let a = [|
        "Whenever the cashier at the grocery store asks my dad if he would like the milk in a bag he replies, 'No, just leave it in the carton!'";
        "Me: 'Dad, make me a sandwich!' Dad: 'Poof, You’re a sandwich!'";
        "How do you make holy water? You boil the hell out of it.";
        "What is Beethoven's favorite fruit? A ba-na-na-na.";
        "5/4 of people admit that they’re bad with fractions."
    |] in
    print_endline a.(Random.int 4)


