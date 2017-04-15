module Watchtower =
    struct
        type hour = int
        let zero = 0
        let add x y = (x + y) mod 12
        let sub x y = if x - y < 0 then 12 - (-(x - y) mod 12) else x - y
    end
