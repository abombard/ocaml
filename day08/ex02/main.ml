let () =
    let methane = new Alkane.methane in
    print_endline methane#to_string;
    let ethane = new Alkane.ethane in
    print_endline ethane#to_string;
    let octane = new Alkane.octane in
    print_endline octane#to_string;
    let idk = new Alkane.alkane 9 in
    print_endline idk#to_string
