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

type helix = nucleotide list

let random_nucleobase () =
    let n = Random.int 4 in
    match n with
    | 0 -> 'A'
    | 1 -> 'T'
    | 2 -> 'C'
    | 3 -> 'G'
    | _ -> '@'

let generate_helix n =
    if n < 0 then []
    else
        let rec create_list n (l : helix) =
            if n = 0 then l
            else create_list (n-1) (generate_nucleotide (random_nucleobase ()) :: l)
        in create_list n []

let nucleobase_to_string (n : nucleobase) =
    match n with
    | A -> "A"
    | T -> "T"
    | C -> "C"
    | G -> "G"
    | None -> "None"

let helix_to_string (l : helix) =
    let rec create_string l s =
        match l with
        | [] -> s
        | nucleotide :: tail ->
            create_string tail (s ^ (nucleobase_to_string nucleotide.nucleobase))
    in create_string l ""

let complementary_nucleotide n =
    match n.nucleobase with
    | A -> generate_nucleotide 'T'
    | T -> generate_nucleotide 'A'
    | C -> generate_nucleotide 'G'
    | G -> generate_nucleotide 'C'
    | None -> generate_nucleotide '@'

let complementary_helix (h : helix) =
    let rec iter (h : helix) (nh : helix) =
        match h with
        | [] -> nh
        | n :: tail ->
                iter tail (nh @ [complementary_nucleotide n])
    in iter h []

let test n =
    print_string "generate_helix ";
    print_int n;
    print_string "\n-> ";
    let h = generate_helix n in
    print_endline (helix_to_string h);
    print_endline "complementary_helix";
    print_string "-> ";
    let ch = complementary_helix h in
    print_endline (helix_to_string ch)

let () =
    test (-1);
    test 0;
    test 1;
    test 5;
    test 12
