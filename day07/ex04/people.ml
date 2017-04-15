class people name =
    object (self)
        initializer print_endline (name ^ " is born")

        val _name : string = name
        val _hp : int = 100

        method getName = _name
        method getHp = _hp

        method takeDamage pv = {< _hp = _hp - pv >}

        method to_string = self#getName ^ " |hp " ^ string_of_int self#getHp ^ "|"

        method talk = print_endline ("I’m " ^ self#getName ^ "! Do you know the Doctor?")
        method die = print_endline "Aaaarghh!"

    end
