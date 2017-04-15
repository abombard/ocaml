let eu_dist a b =
    let len = Array.length a in
    let res = ref 0. in
    for i = 0 to (len - 1) do
        res := !res +. sqrt ((a.(i) -. b.(i)) ** 2.)
    done; !res

let examples_of_file filename =
    let inputs = ref [] in
    let chan = open_in filename in
    try
        while true do
            let line = input_line chan in
            let elems = String.split_on_char ',' line in
            let rec radar_of_line elems floatSet =
                match elems with
                | []                -> failwith "Empty line"
                | radarClass :: []  -> ( Array.of_list floatSet, radarClass )
                | someFloat :: tail -> radar_of_line tail ( float_of_string someFloat :: floatSet )
            in inputs := radar_of_line elems [] :: !inputs
        done; !inputs
    with
    | End_of_file -> close_in chan;
    !inputs

type radar = float array * string

let print_float_array f = Printf.printf "%f; " f
let print_radar ( a, c ) =
    print_string "radar -> [|";
    Array.iter print_float_array a;
    print_string "|] ";
    print_endline c

let one_nn (radars : radar list) (radarCmp : radar) =
    let ( cmpFloatSet, cmpName ) = radarCmp
    and nearest = ref ( [||], "" )
    and nearestScore = ref 100000000. in
    for i = 0 to (List.length radars) - 1 do
        (* print_endline "Compute currentRadar"; *)
        let currentRadar = List.nth radars i in
        (* print_endline "Compute currentScore"; *)
        let currentScore = eu_dist (fst currentRadar) cmpFloatSet in
        (* Printf.printf "nearestScore %f currentScore %f\n" !nearestScore currentScore; *)
        if currentScore < !nearestScore then
            nearest := currentRadar;
            nearestScore := currentScore;
    done;
    !nearest

let k_nn (radars : radar list) n (radarCmp : radar) =
    if n <= 0 then "Error" else
    let rec get_radars radars kRadars count =
        if count = n then kRadars
        else
            let nearestRadar = one_nn radars radarCmp in
            let radars = List.filter (fun e -> e <> nearestRadar) radars in
            get_radars radars (nearestRadar :: kRadars) (count+1)
    in
    let rec addOneOccurence oldRadarTypes newRadarTypes filterRadarType =
        match oldRadarTypes with
        | [] -> ( filterRadarType, 1 ) :: newRadarTypes
        | ( radarType, count ) :: tail when radarType = filterRadarType ->
                List.append ( ( radarType, count+1 ) :: newRadarTypes ) tail
        | ( radarType, count ) as elem :: tail ->
                addOneOccurence tail ( elem :: newRadarTypes ) filterRadarType
    in
    let rec countOccurences kRadars kRadarTypes =
        match kRadars with
        | [] -> kRadarTypes
        | ( _, filterRadarType ) :: tail ->
           countOccurences tail ( addOneOccurence kRadarTypes [] filterRadarType )
    in
    let rec findMostOccurence radarTypes ( bestRadarType, maxOccurence ) =
        match radarTypes with
        | [] -> bestRadarType
        | ( radarType, occurences ) as elem :: tail when occurences > maxOccurence ->
                findMostOccurence tail elem
        | elem :: tail ->
                findMostOccurence tail ( bestRadarType, maxOccurence )
    in
    let kRadars = get_radars radars [] 0 in
    let occurencesList = countOccurences kRadars [] in
    findMostOccurence occurencesList ( "", 0 )

let () =
    let radarCmp = ([|1.;0.;0.94331;0.19959;0.96132;0.40803;0.80514;0.56569;0.56687;0.70830;0.41836;0.83230;0.14939;0.89489;0.05167;0.93682;-0.24742;0.83939;-0.42811;0.75554;-0.50251;0.62563;-0.65515;0.50428;-0.68851;0.30912;-0.77097;0.15619;-0.75406;-0.04399;-0.75199;-0.17921;-0.66932;-0.34367|], "g")
    and radars = examples_of_file "resources/ionosphere.test.csv" in
    print_endline (k_nn radars 5 radarCmp)

