let () =
    let dalek = new People.people "dalek" in
    print_endline dalek#to_string;
    dalek#talk;
    dalek#die
