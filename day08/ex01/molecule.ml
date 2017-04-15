class virtual molecule atoms =
    object (self)
       method virtual name : string
       method virtual formula : string

       method private atoms : (Atom.atom * int) list = List.sort (fun (x, _) (y, _) -> x#compare y) atoms
       method genFormula =
           List.fold_left
                (fun acc (a, n) -> acc ^ a#symbol ^ begin if n = 1 then "" else string_of_int n end)
                ""
                self#atoms

       method to_string = self#name ^ " " ^ self#formula
       method equals (rhs : molecule) = self#name = rhs#name && self#formula = rhs#formula
    end

class water =
    object
        inherit molecule ((new Atom.hydrogen, 2) :: (new Atom.oxygen, 1) :: []) as mother
        method name = "Water"
        method formula = mother#genFormula
    end

class carbon_dioxyde =
    object
        inherit molecule ((new Atom.carbon, 1) :: (new Atom.oxygen, 2) :: []) as mother
        method name = "Carbon dioxyde"
        method formula = mother#genFormula
    end

class vitamin_c =
    object
        inherit molecule ((new Atom.carbon, 6) :: (new Atom.hydrogen, 8) :: (new Atom.oxygen, 6) :: []) as mother
        method name = "Vitamin C"
        method formula = mother#genFormula
    end

class ethanol =
    object
        inherit molecule ((new Atom.carbon, 2) :: (new Atom.hydrogen, 6) :: (new Atom.oxygen, 1) :: []) as mother
        method name = "Ethanol"
        method formula = mother#genFormula
    end

class benzene =
    object
        inherit molecule ((new Atom.carbon, 6) :: (new Atom.hydrogen, 6) :: []) as mother
        method name = "Benzene"
        method formula = mother#genFormula
    end

class biotin =
    object
        inherit molecule ((new Atom.carbon, 10) :: (new Atom.hydrogen, 16) :: (new Atom.nitrogen, 2) :: (new Atom.oxygen, 3) :: (new Atom.sulfur, 1) :: []) as mother
        method name = "biotin"
        method formula = mother#genFormula
    end
