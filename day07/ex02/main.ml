let () =
    let clara = new People.people "Clara Oswald" in
    let doctor = new Doctor.doctor "Who" 4000 clara in
    let dalek = new Dalek.dalek in
    Printf.printf "clara#to_string: %s\n" clara#to_string;
    Printf.printf "doctor#to_string: %s\n" doctor#to_string;
    Printf.printf "dalek#to_string: %s\n" dalek#to_string;
    print_endline "=== The battle begin ===";
    let doctor = doctor#travel_in_time 2000 1990 in
    doctor#talk;
    dalek#talk;
    doctor#use_sonic_screwdriver;
    let dalek = dalek#takeDamage 20 in
    clara#talk;
    dalek#exterminate clara;
    print_endline dalek#to_string;
    doctor#use_sonic_screwdriver;
    let dalek = dalek#takeDamage 20 in
    dalek#talk;
    let doctor = doctor#takeDamage 50 in
    print_endline doctor#to_string;
    dalek#talk;
    let doctor = doctor#restoreHp in
    doctor#use_sonic_screwdriver;
    dalek#die;
    let doctor = doctor#travel_in_time 1990 200 in
    print_endline doctor#to_string;
