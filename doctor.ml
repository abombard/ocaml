class doctor name age sidekick =
    object (self)

        initializer print_endline ("The doctor " ^ name ^ " is born")

        val _name : string = name
        val _age : int = age
        val _sidekick : People.people = sidekick
        val _hp : int = 100

        method to_string = "doctor " ^ _name ^ " -> age " ^ string_of_int _age ^ " hp " ^ string_of_int _hp ^ " sidekick " ^ _sidekick#to_string

        method private drawTardis = print_string
"
        ___
_______(_@_)_______
| POLICE      BOX |
|_________________|
 | _____ | _____ |
 | |###| | |###| |
 | |###| | |###| |
 | _____ | _____ |
 | || || | || || |
 | ||_|| | ||_|| |
 | _____ |$_____ |
 | || || | || || |
 | ||_|| | ||_|| |
 | _____ | _____ |
 | || || | || || |
 | ||_|| | ||_|| |
 |       |       |
 *****************
 "

        method talk = print_endline "Hi! I’m the Doctor!"
        method travel_in_time start arrival = self#drawTardis; {< _age = _age + (arrival - start); >}
        method use_sonic_screwdriver = print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"

        method private regenerate = {< _hp = 100 >}
        method bornAgain = self#regenerate

        method gotHit pv = {< _hp = _hp - pv >}

    end

