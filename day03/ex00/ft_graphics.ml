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

let square_size = 60

let draw_square_text x y s =
    draw_square x y square_size;
    Graphics.moveto x y;
    Graphics.draw_string s

let link_points x1 y1 x2 y2 =
    Graphics.moveto x1 y1;
    Graphics.lineto x2 y2

let link_squares x1 y1 x2 y2 =
    let x1 = x1 + square_size / 2 in
    let x2 = x2 - square_size / 2 in
    link_points x1 y1 x2 y2

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree;;

let rec draw_tree_node n =
    match n with
    | Node (v, n1, n2) ->
            draw_square_text 200 300 "value";
            draw_square_text 600 200 "Nil";
            link_squares 200 300 600 200;
            draw_square_text 600 400 "Nil";
            link_squares 200 300 600 400
    | Nil ->
        draw_square_text 400 300 "Nil"

let main () =
    Graphics.open_graph " 800x600";
    let n = Nil in
    (* let n = Node ("Hello", Nil, Nil) in *)
    draw_tree_node n;
    Graphics.read_key ()

let _ = main ()
