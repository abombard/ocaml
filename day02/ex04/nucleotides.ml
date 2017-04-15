type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None
type nucleotide = {
    phosphate : phosphate;
    deoxyribose : deoxyribose;
    nucleobase : nucleobase
}

let generate_nucleotide c =
    match c with
    | 'A' -> { phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = A }
    | 'T' -> { phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = T }
    | 'C' -> { phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = C }
    | 'G' -> { phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = G }
    | _   -> { phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = None }

let nucleobase_to_string n =
    match n with
    | A -> "A"
    | T -> "T"
    | C -> "C"
    | G -> "G"
    | None -> "None"

let test c =
    print_string "generate_nucleotide ";
    print_char c;
    print_string " -> nucleobase: ";
    let n = generate_nucleotide c in
    print_endline (nucleobase_to_string n.nucleobase)

let () =
    test 'A';
    test 'T';
    test 'C';
    test 'G';
    test 'L'
