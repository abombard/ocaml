type 'a tree = Nil | Node of 'a * 'a tree * 'a tree;;

type window = {
    width : int;
    height: int;
    square_size: int;
}

let win = {
    width = 800;
    height = 600;
    square_size = 30;
}

let rec size bt =
    match bt with
    | Nil -> 1
    | Node (v, n1, n2) ->
            1 + (size n1 + size n2)

let rec height bt =
    let rec compute bt n =
        match bt with
        | Nil -> n + 1
        | Node (v, n1, n2) ->
                let s1 = compute n1 (n+1)
                and s2 = compute n2 (n+1) in
                if s1 > s2 then s1 else s2
    in compute bt 0

(* ex00 *)

let draw_square x y size =
    let half_size = size / 2 in
    let x_top_left  = x - half_size and
        y_top_left  = y - half_size and
        x_top_right = x + half_size and
        y_top_right = y - half_size and
        x_bot_left  = x - half_size and
        y_bot_left  = y + half_size and
        x_bot_right = x + half_size and
        y_bot_right = y + half_size in
    Graphics.moveto x_top_left y_top_left;
    Graphics.lineto x_top_right y_top_right;
    Graphics.lineto x_bot_right y_bot_right;
    Graphics.lineto x_bot_left y_bot_left;
    Graphics.lineto x_top_left y_top_left;;

let draw_square_text x y s =
    draw_square x y win.square_size;
    Graphics.moveto x y;
    Graphics.draw_string s

let link_points x1 y1 x2 y2 =
    Graphics.moveto x1 y1;
    Graphics.lineto x2 y2

let link_squares x1 y1 x2 y2 =
    let x1 = x1 + win.square_size / 2 in
    let x2 = x2 - win.square_size / 2 in
    link_points x1 y1 x2 y2

(* draw tree *)

let rec draw_tree ?(x=win.square_size / 2) ?(y=win.height / 2) n =
    match n with
    | Node (v, n1, n2) ->
            draw_square_text x y v;
            let x1 = x + 2 * win.square_size in
            let y1 = y + (height n1) * win.square_size in
            let x2 = x + 2 * win.square_size in
            let y2 = y - (height n2) * win.square_size in
            draw_tree ~x:x1 ~y:y1 n1;
            draw_tree ~x:x2 ~y:y2 n2;
            link_squares x y x1 y1;
            link_squares x y x2 y2
    | Nil ->
        draw_square_text x y "Nil"

(* tests *)

let test btree =
    Graphics.open_graph (" " ^ (string_of_int win.width) ^ "x" ^ (string_of_int win.height));
    print_string "Size: ";
    print_int (size btree);
    print_char '\n';
    print_string "Height: ";
    print_int (height btree);
    print_char '\n';
    draw_tree btree;
    ignore(Graphics.read_key ());
    Graphics.clear_graph ()

let main () =
    test Nil;
    test (Node ("Hello",
                Node ("toto", Nil, Nil),
                Nil
         )
    );
    test (Node ("Salut",
                Node ("Hello", Nil, Nil),
                Node ("World", Nil, Nil)
          )
    );
    test (Node ("salut",
                Node ("les",
                      Node ("loulou", Nil, Nil),
                      Nil
                ),
                Node ("aaah", Nil, Nil)
          )
    )

let _ = main ()
