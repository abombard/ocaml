class alkane n =
    object
        inherit Molecule.molecule ((new Atom.carbon, n) :: (new Atom.hydrogen, 2 * n + 2) :: []) as mother
        method name =
            if n < 1 || n > 12 then invalid_arg (string_of_int n) else
            let a = [| "methane"; "ethane"; "propane"; "n-butane"; "n-pentane"; "n-hexane"; "n-heptane"; "n-octane"; "n-nonane"; "n-decane"; "n-undecane"; "n-dodecane" |] in
            a.(n - 1)
        method formula = mother#genFormula
    end

class methane =
    object
        inherit alkane 1
    end

class ethane =
    object
        inherit alkane 2
    end

class octane =
    object
        inherit alkane 8
    end
