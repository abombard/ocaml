class people name =
    object
        initializer print_endline (name ^ " is born")

        val _attribute : string = name
        val _hp : int = 100

        method to_string = _attribute ^ " -> hp " ^ string_of_int _hp

        method talk = print_endline ("I’m " ^ _attribute ^ "! Do you know the Doctor?")
        method die = print_endline "Aaaarghh!"
    end
