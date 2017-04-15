(* ocamlc unix.cma micronap.ml *)

let my_sleep () = Unix.sleep 1

let usage () = Printf.fprintf stderr "Usage: %s (n : int)\n" Sys.argv.(0)

let () =
    let argc = Array.length Sys.argv in
    if argc = 2 then begin
        try
            let n = int_of_string Sys.argv.(1) in
            for i = 1 to n do
                (* Printf.printf "%d\n" i; *)
                my_sleep ()
            done;
        with
        | Failure msg -> usage ()
    end
    else usage ()

