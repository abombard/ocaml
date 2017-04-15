module StringHashtbl = Hashtbl.Make (
    struct
        type t = String.t
        let equal t1 t2 = String.equal t1 t2
        let hash t =
            (* FVN-1a hash *)
            let fnv_offset_basis = 2166136261
            and fnv_prime = 16777619 in
            let hashValue = ref fnv_offset_basis in
            let hashCharacter c =
                hashValue := !hashValue lxor int_of_char c;
                hashValue := !hashValue * fnv_prime
            in
            String.iter hashCharacter t;
            !hashValue
    end
)

let () =
    let ht = StringHashtbl.create 5 in
    let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
    let pairs = List.map (fun s -> (s, String.length s)) values in
    List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
    StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht
