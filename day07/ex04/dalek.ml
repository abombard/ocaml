class dalek =
    object (self)

        val _name : string =
            let generateName () =
                let genRandomCharacter () =
                    match Random.int (26+26) with
                    | n when n < 26  -> char_of_int (int_of_char 'a' + n)
                    | n              -> char_of_int (int_of_char 'A' + n - 26)
                in
                let rec genName i s =
                    if i = 3 then s
                    else genName (i+1) (s ^ (String.make 1 (genRandomCharacter ())))
                in genName 0 ""
             in "Dalek" ^ generateName ()

        val _hp : int = 100
        val mutable _shield : bool = true

        method getName = _name
        method getHp = _hp
        method getShield = _shield

        method takeDamage pv = {< _hp = _hp - pv >}

        method to_string = _name ^ " -> hp " ^ string_of_int self#getHp ^ " shield " ^ string_of_bool self#getShield
        method talk =
            let a = [
                "∗ Explain! Explain!";
                "∗ Exterminate! Exterminate!";
                "∗ I obey!";
                "∗ You are the Doctor! You are the enemy of the Daleks!"
            ] in print_endline (List.nth a (Random.int (List.length a)))
       method exterminate (people : People.people) = people#die; _shield <- not _shield
       method die = print_endline "Emergency Temporal Shift!"

    end
