let () =
    let water = new Molecule.water in
    print_endline water#to_string;
    let carbon_dioxyde = new Molecule.carbon_dioxyde in
    print_endline carbon_dioxyde#to_string;
    let vitamin_c = new Molecule.vitamin_c in
    print_endline vitamin_c#to_string;
    let ethanol = new Molecule.ethanol in
    print_endline ethanol#to_string;
    let benzene = new Molecule.benzene in
    print_endline benzene#to_string;
    let biotin = new Molecule.biotin in
    print_endline biotin#to_string
