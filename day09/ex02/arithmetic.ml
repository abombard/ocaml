module type MONOID =
    sig
        type element
        val zero1 : element
        val zero2 : element
        val add : element -> element -> element
        val sub : element -> element -> element
        val mul : element -> element -> element
        val div : element -> element -> element
    end

module INT =
    struct
        type element = int
        let zero1 = 0
        let zero2 = 1
        let add e1 e2 = e1 + e2
        let sub e1 e2 = e1 - e2
        let mul e1 e2 = e1 * e2
        let div e1 e2 = e1 / e2
    end

module FLOAT =
    struct
        type element = float
        let zero1 = 0.
        let zero2 = 1.
        let add e1 e2 = e1 +. e2
        let sub e1 e2 = e1 -. e2
        let mul e1 e2 = e1 *. e2
        let div e1 e2 = e1 /. e2
    end

module Calc =
    functor (Monoid : MONOID) ->
        struct
            let add = Monoid.add
            let sub = Monoid.sub
            let mul = Monoid.mul
            let div = Monoid.div
            let power x y =
                let rec aux acc i =
                    if i = y then acc
                    else aux (mul acc x) (i+1)
                in aux Monoid.zero2 0
            let fact x =
                let rec aux acc x =
                    if x = 0 then acc
                    else aux (acc + x) (x-1)
                in aux 0 x
         end

module Calc_int = Calc(INT)
module Calc_float = Calc(FLOAT)

let () =
    print_endline (string_of_int (Calc_int.power 3 3));
    print_endline (string_of_float (Calc_float.power 3.0 3));
    print_endline (string_of_int (Calc_int.mul (Calc_int.add 20 1) 2));
    print_endline (string_of_float (Calc_float.mul (Calc_float.add 20.0 1.0) 2.0))
