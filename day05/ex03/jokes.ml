let load_file filename =
    let lines = ref [] in
    let chan = open_in filename in
    try
        while true do
            lines := input_line chan :: !lines
        done; Array.of_list !lines
    with End_of_file -> close_in chan;
    Array.of_list !lines

let usage () = Printf.fprintf stderr "Usage:%s filename\n" Sys.argv.(0)

let () =
    if Array.length Sys.argv != 2 then usage ()
    else
        Random.self_init ();
        try
            let a = load_file Sys.argv.(1) in
            let index = Random.int ((Array.length a)) in
            print_endline a.(index)
        with e -> usage ()
