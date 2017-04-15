let () =
    let h = new Atom.hydrogen in
    print_endline h#to_string;
    let o = new Atom.oxygen in
    print_endline o#to_string;
    let c = new Atom.carbon in
    print_endline c#to_string;
    let n = new Atom.nitrogen in
    print_endline n#to_string;
    let ne = new Atom.neon in
    print_endline ne#to_string;
