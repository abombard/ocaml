let print_proj (s, status, grade) =
    Printf.printf "Project \"%s\" status: %s grade: %d\n" s status grade

let () =
    let project1 = App.App.zero in
    let project2 = ("Testing", "", 0) in
    let result = App.App.combine project1 project2 in
    print_proj result
