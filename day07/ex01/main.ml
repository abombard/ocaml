let () =
    let clara = new People.people "Clara Oswald" in
    let doctor = new Doctor.doctor "Who" 4000 clara in
    print_string "doctor#to_string: ";
    print_endline doctor#to_string;
    print_string "doctor#travel_in_time 2000 1600: ";
    let doctor = doctor#travel_in_time 2000 1600 in
    print_string "doctor#to_string: ";
    print_endline doctor#to_string;
    print_string "doctor#talk: ";
    doctor#talk;
    print_string "doctor#use_sonic_screwdriver: ";
    doctor#use_sonic_screwdriver;
    print_endline "doctor#takeDamage 20: ";
    let doctor = doctor#takeDamage 20 in
    print_string "doctor#to_string: ";
    print_endline doctor#to_string;
    print_endline "doctor#restoreHp";
    let doctor = doctor#restoreHp in
    print_string "doctor#to_string: ";
    print_endline doctor#to_string;
