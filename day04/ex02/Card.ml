module Color =
struct

    type t = Spade | Heart | Diamond | Club

    let all = [ Spade; Heart; Diamond; Club ]

    let toString color =
        match color with
        | Spade   -> "S"
        | Heart   -> "H"
        | Diamond -> "D"
        | Club    -> "C"

    let toStringVerbose color =
        match color with
        | Spade   -> "Spade"
        | Heart   -> "Heart"
        | Diamond -> "Diamond"
        | Club    -> "Club"

end

module Value =
struct

    type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

    let all = [ T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As ]

    let toInt value =
        match value with
	    | T2 -> 1
	    | T3 -> 2
	    | T4 -> 3
	    | T5 -> 4
	    | T6 -> 5
	    | T7 -> 6
	    | T8 -> 7
	    | T9 -> 8
	    | T10 -> 9
	    | Jack -> 10
	    | Queen -> 11
	    | King -> 12
	    | As -> 13

    let toString value =
        match value with
	    | T2 -> "2"
	    | T3 -> "3"
	    | T4 -> "4"
	    | T5 -> "5"
	    | T6 -> "6"
	    | T7 -> "7"
	    | T8 -> "8"
	    | T9 -> "9"
	    | T10 -> "10"
	    | Jack -> "J"
	    | Queen -> "Q"
	    | King -> "K"
	    | As -> "A"

    let toStringVerbose value =
        match value with
	    | T2 -> "2"
	    | T3 -> "3"
	    | T4 -> "4"
	    | T5 -> "5"
	    | T6 -> "6"
	    | T7 -> "7"
	    | T8 -> "8"
	    | T9 -> "9"
	    | T10 -> "10"
	    | Jack -> "Jack"
	    | Queen -> "Queen"
	    | King -> "King"
	    | As -> "As"

    let next value =
        match value with
	    | T2 -> T3
	    | T3 -> T4
	    | T4 -> T5
	    | T5 -> T6
	    | T6 -> T7
	    | T7 -> T8
	    | T8 -> T9
	    | T9 -> T10
	    | T10 -> Jack
	    | Jack -> Queen
	    | Queen -> King
	    | King -> As
	    | As -> invalid_arg (toStringVerbose value)

    let previous value =
        match value with
	    | T2 -> invalid_arg (toStringVerbose value)
	    | T3 -> T2
	    | T4 -> T3
	    | T5 -> T4
	    | T6 -> T5
	    | T7 -> T6
	    | T8 -> T7
	    | T9 -> T8
	    | T10 -> T9
	    | Jack -> T10
	    | Queen -> Jack
	    | King -> Queen
	    | As -> King

end

type t = { value : Value.t; color: Color.t }

let newCard value color = { value = value; color = color }

let allCardsFromColor color =
    let rec aux values cards =
        match values with
        | [] -> cards
        | value :: tail -> aux tail (cards @ [newCard value color])
    in aux Value.all []

let rec concat_list src dst =
    match src with
    | [] -> dst
    | cards :: tail -> concat_list tail (List.append cards dst)

let allSpades   = allCardsFromColor Color.Spade
let allHearts   = allCardsFromColor Color.Heart
let allDiamonds = allCardsFromColor Color.Diamond
let allClubs    = allCardsFromColor Color.Club
let all = concat_list [allSpades; allHearts; allDiamonds; allClubs] []

let getValue card = card.value
let getColor card = card.color

let toString card = (Value.toString card.value) ^ (Color.toString card.color)
let toStringVerbose card =
    "Card(" ^
    (Value.toStringVerbose card.value) ^
    ", " ^
    (Color.toStringVerbose card.color) ^
    ")"

let compare x y = Value.toInt x.value - Value.toInt y.value
let max x y = if Value.toInt x.value >= Value.toInt y.value then x else y
let min x y = if Value.toInt x.value <= Value.toInt y.value then x else y
let best card_list =
    match card_list with
    | [] -> invalid_arg "best: Empty list"
    | first :: tail ->
            let rec aux card_list b =
                match card_list with
                | [] -> b
                | card :: tail ->
                        aux tail (max b card)
            in aux tail first

let isOf card color = card.color = color
let isSpade card    = card.color = Color.Spade
let isHeart card    = card.color = Color.Heart
let isDiamond card  = card.color = Color.Diamond
let isClub card     = card.color = Color.Club

