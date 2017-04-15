let print_army name a =
    Printf.printf "army of %s: size %d\n" name a#size

let () =
    Random.self_init ();
    (* people *)
    let peopleArmy = new Army.army in
    print_army "people" peopleArmy;
    let peopleArmy = peopleArmy#add (new People.people "Clara oswald") in
    print_army "people" peopleArmy;
    let peopleArmy = peopleArmy#add (new People.people "Rory Williams") in
    print_army "people" peopleArmy;
    let peopleArmy = peopleArmy#delete in
    print_army "people" peopleArmy;
    let peopleArmy = peopleArmy#delete in
    print_army "people" peopleArmy;
    (* doctor *)
    let doctorArmy = new Army.army in
    print_army "doctor" doctorArmy;
    let doctorArmy = doctorArmy#add (new Doctor.doctor "Who") in
    print_army "doctor" doctorArmy;
    let doctorArmy = doctorArmy#add (new Doctor.doctor "Who") in
    print_army "doctor" doctorArmy;
    let doctorArmy = doctorArmy#delete in
    print_army "doctor" doctorArmy;
    let doctorArmy = doctorArmy#delete in
    print_army "doctor" doctorArmy;
    (* dalek *)
    let dalekArmy = new Army.army in
    print_army "dalek" dalekArmy;
    let dalekArmy = dalekArmy#add (new Dalek.dalek) in
    print_army "dalek" dalekArmy;
    let dalekArmy = dalekArmy#add (new Dalek.dalek) in
    print_army "dalek" dalekArmy;
    let dalekArmy = dalekArmy#delete in
    print_army "dalek" dalekArmy;
    let dalekArmy = dalekArmy#delete in
    print_army "dalek" dalekArmy;
