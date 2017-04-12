let () =
    let clara = new People.people "Clara Oswald" in
    let doctor = new Doctor.doctor "Who" 4000 clara in
    print_endline doctor#to_string;
    print_endline "travel_in_time 2000 1600";
    let doctor = doctor#travel_in_time 2000 1600 in
    print_endline doctor#to_string;
    print_endline "talk";
    doctor#talk;
    print_endline "use_sonic_screwdriver";
    doctor#use_sonic_screwdriver;
    print_endline "Loose 20 hp";
    let doctor = doctor#gotHit 20 in
    print_endline doctor#to_string;
    print_endline "regenerate";
    let doctor = doctor#bornAgain in
    print_endline doctor#to_string;
