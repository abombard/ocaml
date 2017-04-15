let examples_of_file filename =
    let inputs = ref [] in
    let chan = open_in filename in
    try
        while true do
            let line = input_line chan in
            let elems = String.split_on_char ',' line in
            let rec radar_of_line elems floatSet =
                match elems with
                | []                -> failwith "Empty line"
                | radarClass :: []  -> ( Array.of_list floatSet, radarClass )
                | someFloat :: tail -> radar_of_line tail ( float_of_string someFloat :: floatSet )
            in inputs := radar_of_line elems [] :: !inputs
        done; !inputs
    with
    | End_of_file -> close_in chan;
    !inputs

let print_float_array f = Printf.printf "%f; " f
let print_radar ( a, c ) =
    print_string "radar -> [|";
    Array.iter print_float_array a;
    print_string "|] ";
    print_endline c

let usage () = Printf.fprintf stderr "Usage: %s <csv file>\n" Sys.argv.(0)
let () =
    try
        List.iter print_radar (examples_of_file Sys.argv.(1))
    with e -> usage ()
