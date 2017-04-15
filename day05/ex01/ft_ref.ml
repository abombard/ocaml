type 'a ft_ref = { mutable contents: 'a }

let return t = { contents = t }

let get t = t.contents
let set t v = t.contents <- v

let bind t f = return (get (f (get t)))

let () =
    Printf.printf "x = return 42\n";
    let x = return 42 in
    Printf.printf "get x: %d\n" (get x);
    set x 21;
    Printf.printf "set x 21 -> get x: %d\n" (get x);
    let y = bind x (fun x -> return (x + 100)) in
    Printf.printf "y = bind x (fun x -> return (x + 100)) -> get x: %d get y: %d\n" (get x) (get y)
