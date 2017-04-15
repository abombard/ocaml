class virtual reaction m1 m2 =
    object (self)
        val _m1 : (Molecule.molecule * int) list = m1
        val _m2 : (Molecule.molecule * int) list = m2
        method virtual get_start : (Molecule.molecule * int) list
        method virtual get_result : (Molecule.molecule * int) list
        method virtual balance : reaction
        method virtual is_balanced : bool
    end
