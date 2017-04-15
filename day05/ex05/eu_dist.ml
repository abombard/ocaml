let eu_dist a b =
    let len = Array.length a in
    let res = ref 0. in
    for i = 0 to (len - 1) do
        res := !res +. sqrt ((a.(i) -. b.(i)) ** 2.)
    done; !res

let test a b =
    print_string "eu_dist [|";
    Array.iter print_float a;
    print_string "|] [|";
    Array.iter print_float b;
    print_string "|] = ";
    print_float (eu_dist a b);
    print_endline ""

let () =
    test [||] [||];
    test [|0.1; 0.2; 1.2|] [|1.1; 1.2; 3.3|]
