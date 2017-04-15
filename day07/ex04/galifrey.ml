class galifrey =
    object (self)

        val _daleks : Dalek.dalek Army.army =
            let count = Random.int 10 in
            let rec aux i l =
                if i = count then l
                else aux (i+1) (l#add (new Dalek.dalek))
            in aux 0 (new Army.army)

        val _doctors : Doctor.doctor Army.army =
            List.fold_left
                (fun acc n -> acc#add (new Doctor.doctor n (Random.int 1000) (new People.people "Sidekick")))
                (new Army.army)
                ["Who"; "Tom Baker"; "Peter Davison"; "Paul McGann"; "Matt Smith"; "Peter Capaldi"; "John Hurt"]

        val _peoples : People.people Army.army =
            List.fold_left
                (fun acc n -> acc#add (new People.people n))
                (new Army.army)
                ["Clara Oswald"; "Rory Williams"; "Amelia Pond"; "Rose Tyler"; "Martha Jones"]

        method getDaleks = _daleks
        method getDoctors = _doctors
        method getPeoples = _peoples

        method 

        method do_time_war =
            let rec aux doctors peoples daleks =
                if doctors#size = 0 then print_endline "Daleks Win"
                else if daleks#size = 0 then print_endline "Doctors Win"
                else begin
                    match Random.int 3 with
                    | 0 -> doctors#add doctors#delete#attack daleks#getRandom
                    | 1 -> peoples#getRandom#attack daleks#getRandom
                    | 2 -> begin
                        match Random.int 2 with
                        | 0 -> daleks#getRandom#attack doctors#getRandom
                        | 1 -> daleks#getRandom#attack peoples#getRandom
                        | _ -> failwith "Out of range value"
                    end
                    | _ -> failwith "Out of range value"
                end
            in aux _doctors _peoples _daleks

    end
