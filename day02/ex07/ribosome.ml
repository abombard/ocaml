type phosphate = string;;
type deoxyribose = string;;
type nucleobase = A | T | U | C | G | None;;
type nucleotide = {
    phosphate : phosphate;
    deoxyribose : deoxyribose;
    nucleobase : nucleobase
};;

let generate_nucleotide c =
    match c with
    | 'A' -> { phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = A }
    | 'T' -> { phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = T }
    | 'U' -> { phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = U }
    | 'C' -> { phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = C }
    | 'G' -> { phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = G }
    | _   -> { phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = None };;

type helix = nucleotide list;;

let generate_helix n =
    if n < 0 then []
    else
        let random_nucleobase () =
            let n = Random.int 4 in
            match n with
            | 0 -> 'A'
            | 1 -> 'T'
            | 2 -> 'C'
            | 3 -> 'G'
            | _ -> '@'
        in
        let rec create_list n (l : helix) =
            if n = 0 then l
            else create_list (n-1) (generate_nucleotide (random_nucleobase ()) :: l)
        in create_list n [];;

let helix_to_string (l : helix) =
    let string_of_nucleobase (n : nucleobase) =
        match n with
        | A -> "A"
        | T -> "T"
        | U -> "U"
        | C -> "C"
        | G -> "G"
        | None -> "None"
    in
    let rec create_string l s =
        match l with
        | [] -> s
        | nucleotide :: tail ->
            create_string tail (s ^ (string_of_nucleobase nucleotide.nucleobase))
    in create_string l "";;

let complementary_helix (h : helix) =
    let complementary_nucleotide n =
        match n.nucleobase with
        | A -> generate_nucleotide 'T'
        | T -> generate_nucleotide 'A'
        | C -> generate_nucleotide 'G'
        | G -> generate_nucleotide 'C'
        | _ -> generate_nucleotide '@'
    in
    let rec iter (h : helix) (nh : helix) =
        match h with
        | [] -> nh
        | n :: tail ->
                iter tail (nh @ [complementary_nucleotide n])
    in iter h [];;

type rna = nucleobase list;;

let generate_rna (h : helix) =
    let rna_complementary_nucleobase n =
        match n with
        | A -> U
        | T -> A
        | C -> G
        | G -> C
        | _ -> None
    in
    let rec create_rna (h : helix) (nr : rna) =
        match h with
        | [] -> nr
        | n :: tail ->
                create_rna tail (nr @ [rna_complementary_nucleobase n.nucleobase])
    in create_rna h [];;

let generate_base_triplets (r : rna) =
    let rec create_list r nbt =
        match r with
        | n1 :: n2 :: n3 :: tail ->
            create_list tail (nbt @ [(n1, n2, n3)])
        | _ -> nbt
    in create_list r [];;

let string_of_nucleobase n =
    match n with
    | A -> "A"
    | T -> "T"
    | C -> "C"
    | G -> "G"
    | U -> "U"
    | None -> "None";;

type aminoacid = nucleobase * nucleobase * nucleobase

let string_of_aminoacid a =
    let (n1, n2, n3) = a in
    (string_of_nucleobase n1) ^ (string_of_nucleobase n2) ^ (string_of_nucleobase n3)

type protein = {
    aminoacids : aminoacid list;
    func : fun p ->


let string_of_protein (p : protein) =
    


(* Tests *)

let rna_to_string (r : rna) =
    let rec create_string r s =
        match r with
        | [] -> s
        | n :: tail ->
                create_string tail (s ^ (string_of_nucleobase n))
    in create_string r "";;

let test n =
    print_string "generate_helix ";
    print_int n;
    print_string "\n-> ";
    let h = generate_helix n in
    print_endline (helix_to_string h);
    print_endline "generate_rna";
    print_string "-> ";
    let ch = generate_rna h in
    print_endline (rna_to_string ch);;

let () =
    Random.self_init ();
    test (-1);
    test 0;
    test 5;
    test 12;;

