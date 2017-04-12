module type FIXED = sig
    type t
    val of_float : float -> t
    val of_int : int -> t
    val to_float : t -> float
    val to_int : t -> int
    val to_string : t -> string
    val zero : t
    val one : t
    val succ : t -> t
    val pred : t -> t
    val min : t -> t -> t
    val max : t -> t -> t
    val gth : t -> t -> bool
    val lth : t -> t -> bool
    val gte : t -> t -> bool
    val lte : t -> t -> bool
    val eqp : t -> t -> bool (** physical equality *)
    val eqs : t -> t -> bool (** structural equality *)
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t
    val foreach : t -> t -> (t -> unit) -> unit
end

module type BITS = sig val bits : int end

module type MAKE =
    functor (Bits : BITS) -> FIXED

module Make : MAKE =
    functor (Bits : BITS) ->
        struct
            type t = int
            let zero = 0
            let one = 1 lsl Bits.bits
            let of_float x = int_of_float (x *. float_of_int one)
            let of_int x = x lsl Bits.bits
            let to_float x = float_of_int x /. float_of_int one
            let to_int x = x lsr Bits.bits
            let to_string x = string_of_float (to_float x)
            let succ x = x + one
            let pred x = x - one
            let min x y = if x <= y then x else y
            let max x y = if x >= y then x else y
            let gth x y = x > y
            let lth x y = x < y
            let gte x y = x >= y
            let lte x y = x <= y
            let eqp x y = x == y
            let eqs x y = x = y
            let add x y = x + y
            let sub x y = x - y
            let mul x y = x * y lsr Bits.bits
            let div x y = x lsl Bits.bits / y
            let foreach x y f =
                let rec aux i =
                    if gth i y then ()
                    else f i
                in aux x
        end

module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)

let () =
    let i = Fixed8.of_int 5 in
    Printf.printf "i: %s\n" (Fixed8.to_string i);
    let x8 = Fixed8.of_float 21.10 in
    let y8 = Fixed8.of_float 21.32 in
    let r8 = Fixed8.add x8 y8 in
    print_endline (Fixed8.to_string r8);
    (*
    Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f))
    *)
