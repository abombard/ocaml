let leibniz_pi () =
    let pi = 4.0 *. atan 1.0 in
    let fabs x = if x < 0.0 then -.x else x in
    let rec ft_sum f x i =
        if fabs (x -. pi) < 0.001
        then x
        else begin
            print_float x;
            print_char '\r';
            ft_sum f (x +. 4.0 *. (f i)) (i+1)
        end in
    let rec ft_power ?(acc=1) x y =
        if y = 0 then acc
        else ft_power ~acc:(acc * x) x (y-1) in
    let stepi i =
        float_of_int (ft_power (-1) i) /. float_of_int (2 * i + 1)
    in ft_sum stepi 0.0 0

let () =
    print_float (leibniz_pi ());
    print_char '\n'
