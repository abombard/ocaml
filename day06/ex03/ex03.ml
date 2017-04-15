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

module type FRACTIONNAL_BITS = sig val bits : int end

module type MAKE =
    functor (Bits : FRACTIONNAL_BITS) -> FIXED

module Make : MAKE =
    functor (Bits : FRACTIONNAL_BITS) ->
        struct
            type t = int
            let zero = 0
            let one = 1 lsl Bits.bits
            let of_float x = int_of_float (x *. float_of_int one +. 1.)
            let of_int x = x lsl Bits.bits
            let to_float x = float_of_int x /. float_of_int one
            let to_int x = x lsr Bits.bits
            let to_string x = string_of_float (to_float x)
            let succ x = x + 1
            let pred x = x - 1
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
            let mul x y = (x * y) lsr Bits.bits
            let div x y = x lsl Bits.bits / y
            let foreach x y f =
                let rec aux i =
                    if gth i y then ()
                    else begin f i; aux (succ i) end
                in aux x
        end

module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)

let () =
    let x8 = Fixed8.of_float 21.10 in
    let y8 = Fixed8.of_float 21.32 in
    let r8 = Fixed8.add x8 y8 in
    print_endline (Fixed8.to_string r8);
    Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f));
    Printf.printf "to_float %s -> %s\n" (Fixed8.to_string x8) (string_of_float (Fixed8.to_float x8));
    Printf.printf "to_int %s -> %s\n" (Fixed8.to_string x8) (string_of_int (Fixed8.to_int x8));
    let x = Fixed4.of_float 2.1 in
    let y = Fixed4.of_int 2 in
    let z = Fixed4.of_int 3 in
    let ra = Fixed4.add x y in
    let rs = Fixed4.sub x z in
    let rm = Fixed4.mul x y in
    let rd = Fixed4.div x y in
    Printf.printf "%s + %s = %s\n" (Fixed4.to_string x) (Fixed4.to_string y) (Fixed4.to_string ra);
    Printf.printf "%s - %s = %s\n" (Fixed4.to_string x) (Fixed4.to_string z) (Fixed4.to_string rs);
    Printf.printf "%s * %s = %s\n" (Fixed4.to_string x) (Fixed4.to_string y) (Fixed4.to_string rm);
    Printf.printf "%s / %s = %s\n" (Fixed4.to_string x) (Fixed4.to_string y) (Fixed4.to_string rd);
    let pred = Fixed4.pred Fixed4.one in
    let succ = Fixed4.succ Fixed4.one in
    Printf.printf "one : %s ; succ one : %s ; prev one : %s\n"
        (Fixed4.to_string Fixed4.one) (Fixed4.to_string succ) (Fixed4.to_string pred);
    let max = Fixed4.max pred succ in
    Printf.printf "max %s %s -> %s\n" (Fixed4.to_string pred) (Fixed4.to_string succ) (Fixed4.to_string max);
    let min = Fixed4.min pred succ in
    Printf.printf "min %s %s -> %s\n" (Fixed4.to_string pred) (Fixed4.to_string succ) (Fixed4.to_string min);
    let gth = Fixed4.gth pred succ in
    Printf.printf "gth %s %s -> %s\n" (Fixed4.to_string pred) (Fixed4.to_string succ) (string_of_bool gth);
    let lth = Fixed4.lth pred succ in
    Printf.printf "lth %s %s -> %s\n" (Fixed4.to_string pred) (Fixed4.to_string succ) (string_of_bool lth);
    let gte = Fixed4.gte pred succ in
    Printf.printf "gte %s %s -> %s\n" (Fixed4.to_string pred) (Fixed4.to_string succ) (string_of_bool gte);
    let lte = Fixed4.lte pred succ in
    Printf.printf "lte %s %s -> %s\n" (Fixed4.to_string pred) (Fixed4.to_string succ) (string_of_bool lte);
    let x1 = Fixed4.of_int 42 in
    let x2 = Fixed4.of_int 42 in
    let x3 = Fixed4.of_int 21 in
    let eqp = Fixed4.eqp x1 x2 in
    Printf.printf "eqp x1: %s == x2: %s -> %s\n" (Fixed4.to_string x1) (Fixed4.to_string x2) (string_of_bool eqp);
    let eqp = Fixed4.eqp x1 x1 in
    Printf.printf "eqp x1 : %s == x1 : %s -> %s\n" (Fixed4.to_string x1) (Fixed4.to_string x1) (string_of_bool eqp);
    let eqs = Fixed4.eqs x1 x1 in
    Printf.printf "eqs x1 : %s = x1 : %s -> %s\n" (Fixed4.to_string x1) (Fixed4.to_string x1) (string_of_bool eqs);
    let eqs = Fixed4.eqs x1 x3 in
    Printf.printf "eqs x1 : %s = x3 : %s -> %s\n" (Fixed4.to_string x1) (Fixed4.to_string x3) (string_of_bool eqs)

