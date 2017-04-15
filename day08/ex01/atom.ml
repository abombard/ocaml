class virtual atom =
    object (self)
        method virtual name : string
        method virtual symbol : string
        method virtual atomic_number : int

        method to_string = self#name ^ " " ^ self#symbol ^ " " ^ string_of_int self#atomic_number

        method equals (rhs : atom) = self#name = rhs#name && self#symbol = rhs#symbol && self#atomic_number = rhs#atomic_number
        method compare (rhs : atom) =
            match (self#name, rhs#name) with
            | (n1, n2) when n1 = n2 -> 0
            | ("carbon", _) -> -1
            | (_, "carbon") -> 1
            | ("hydrogen", _) -> -1
            | (_, "hydrogen") -> 1
            | (n1, n2) -> compare n1 n2
    end

class hydrogen =
    object
        inherit atom
        method name = "hydrogen"
        method symbol = "H"
        method atomic_number = 1
    end

class helium =
    object
        inherit atom
        method name = "helium"
        method symbol = "He"
        method atomic_number = 2
    end

class lithium =
    object
        inherit atom
        method name = "lithium"
        method symbol = "Li"
        method atomic_number = 3
    end

class boron =
    object
        inherit atom
        method name = "boron"
        method symbol = "B"
        method atomic_number = 5
    end

class carbon =
    object
        inherit atom
        method name = "carbon"
        method symbol = "C"
        method atomic_number = 6
    end

class nitrogen =
    object
        inherit atom
        method name = "nitrogen"
        method symbol = "N"
        method atomic_number = 7
    end

class oxygen =
    object
        inherit atom
        method name = "oxygen"
        method symbol = "O"
        method atomic_number = 8
    end

class fluorine =
    object
        inherit atom
        method name = "fluorine"
        method symbol = "F"
        method atomic_number = 9
    end

class neon =
    object
        inherit atom
        method name = "neon"
        method symbol = "Ne"
        method atomic_number = 10
    end

class sulfur =
    object
        inherit atom
        method name = "sulfur"
        method symbol = "S"
        method atomic_number = 16
    end
