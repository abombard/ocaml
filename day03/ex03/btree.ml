type 'a tree = Nil | Node of 'a * 'a tree * 'a tree;;

let is_bst tree =
    let rec eval_node node =
        match node with
        | Nil -> true
        | Node ( v, left, right ) ->
                match (left, right) with
                | ( Nil, Nil ) -> true
                | ( Node ( lv, _, _ ), Nil ) -> lv < v && eval_node left
                | ( Nil, Node ( lr, _, _ ) ) -> v < lr && eval_node right
                | ( Node ( lv, _, _ ), Node ( lr, _, _ ) ) ->
                    lv < v && v < lr && eval_node left && eval_node right
    in eval_node tree

let rec is_perfect tree =
    match tree with
    | Node ( v, n1, n2 ) ->
            ((n1 = Nil && n2 = Nil) || (n1 != Nil && n2 != Nil))
            && is_bst n1 && is_bst n2
    | Nil -> true

let rec height bt =
    let rec compute bt n =
        match bt with
        | Nil -> n + 1
        | Node (v, n1, n2) ->
                let s1 = compute n1 (n+1)
                and s2 = compute n2 (n+1) in
                if s1 > s2 then s1 else s2
    in compute bt 0

let rec is_balanced bst =
    match bst with
    | Nil -> true
    | Node ( _, n1, n2 ) ->
            abs ( ( height n1 ) - ( height n2 ) ) <= 1
            && is_balanced n1
            && is_balanced n2

let rec search_bst value bst =
    match bst with
    | Nil -> false
    | Node ( v, n1, n2 ) ->
            value = v || search_bst value n1 || search_bst value n2

let rec add_bst value bst =
    match bst with
    | Nil -> Node ( value, Nil, Nil )
    | Node ( v, n1, n2 ) when v > value -> Node ( v, add_bst value n1, n2 )
    | Node ( v, n1, n2 ) -> Node ( v, n1, add_bst value n2 )

let rec add_bst_to_bst src dst =
    match src with
    | Nil -> dst
    | Node ( v, n1, n2 ) ->
            add_bst_to_bst n1 (add_bst_to_bst n2 (add_bst v dst))

let rec delete_bst value bst =
    match bst with
    | Nil -> Nil
    | Node ( v, n1, n2 ) when v = value -> add_bst_to_bst n1 n2
    | Node ( v, n1, n2 ) -> Node ( v, delete_bst value n1, delete_bst value n2 )

(* display *)

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

let rec draw_tree ?(x=win.square_size / 2) ?(y=win.height / 2) n =
    match n with
    | Node (v, n1, n2) ->
            draw_square_text x y (string_of_int v);
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

let display btree =
    Graphics.open_graph (" " ^ (string_of_int win.width) ^ "x" ^ (string_of_int win.height));
    draw_tree btree;
    ignore(Graphics.read_key ());
    Graphics.clear_graph ()

(* tests *)

let test tree =
    print_endline "=== new test ===";
    print_string "is_bst: ";
    print_endline (string_of_bool (is_bst tree));
    print_string "is_perfect: ";
    print_endline (string_of_bool (is_perfect tree));
    print_string "is_balanced: ";
    print_endline (string_of_bool (is_balanced tree));
    print_string "search_bst ";
    let add_value = 3 in
    print_int add_value;
    print_string ": ";
    print_endline (string_of_bool (search_bst add_value tree));
    display tree;
    print_string "add_bst ";
    print_int add_value;
    print_endline "";
    let tree = add_bst add_value tree in
    display tree;
    let del_value = 4 in
    print_string "delete_bst ";
    print_int del_value;
    print_endline "";
    display (delete_bst del_value tree)

let () =
    test ( Nil );
    test ( Node ( 5, Nil, Nil ) );
    test (
        Node ( 5,
            Node ( 3, Nil, Nil ),
            Node ( 2, Nil, Nil )
       )
    );
    test (
        Node ( 5,
            Node ( 6, Nil, Nil ),
            Node ( 4, Nil, Nil )
        )
    );
    test (
        Node ( 5,
            Node ( 4, Nil, Nil ),
            Nil
        )
    );
    test (
        Node ( 5,
            Node ( 4,
                Node ( 3, Nil, Nil ),
                Nil
            ),
            Nil
        )
    )
