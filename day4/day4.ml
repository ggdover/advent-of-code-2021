
(*
DESCRIPTION

Get all content of the file 'file' as one whole string.

PARAMETERS

file = Name of file to read from
*)
let input_string file =
    let ic = open_in file in
    let str = really_input_string ic (in_channel_length ic) in
    close_in ic;
    str

module String = struct
    include String
    let split_on_str delim str = Str.split (Str.regexp delim) str
        (*
        This doesn't work for what I'm trying to do here.
        "split_on_chars" doesn't split when those chars appears in sequence
        in a string, the way that works is that it will split the string if
        any of those chars appear at any point in the string it will split.
        So for example: "split_on_chars '\n','\r'.." means that it will split
        whenever either '\n' or '\r' if found, not only if "\n\r" is found.

        let chars = Base.String.to_list delim in
        Base.String.split_on_chars ~on:chars str
        *)

    (*
    NOTE with this implementation, the order of your delimiter strings might matter!

    So for example, if you have the two delimiters "\n" and "\n\n" and they are
    placed in that order so the call to this function looks like 
    " String.split_on_strs ["\n"; "\n\n"] "1\n\n2\n3" ", then the result will be:
    ["1"; ""; "2"; "3"], but if you switch order on those delimiters so the function
    call becomes " String.split_on_strs ["\n\n"; "\n"] "1\n\n2\n3" " you'll get
    the result: ["1"; "2"; "3"].
    *)
    let split_on_strs delims str = 
        let rec get_regexp delims regexp =
            match delims with
            | [] -> regexp ^ "\\)"
            | h::t -> get_regexp t (regexp ^ "\\|" ^ h)
        in
        let re = get_regexp (List.tl delims) ("\\(" ^ List.hd delims) in
        Str.split (Str.regexp re) str

    let split_on_chars = Base.String.split_on_chars
end

module List = struct
    include List
    let transpose_exn = Base.List.transpose_exn

    (* Pop the item from list if it exists.
       If duplicates of that same item exists,
       it will only pop the first one it finds *)
    let pop item list = 
        let rec inner list new_list =
            match list with
            | [] -> new_list
            | h::t ->
                let next_list = 
                    if h = item then new_list else (h :: new_list)
                in
                inner t next_list
        in
        inner list []

    let split_n = Base.List.split_n
end

(***************************************)

let parse_card card_str = 
    let horz_rows_str = String.split_on_char '\n' card_str in
    let parse_row row =
        (* Solution 1 *)
        (*let nums_str = String.split_on_strs ["  "; " "] row in*)
        (* Solution 2 *)
        let nums_str = Str.split (Str.regexp "\\(  \\| \\)") row in

        List.map int_of_string nums_str
    in
    let horz_rows = List.map parse_row horz_rows_str in
    let vert_rows = List.transpose_exn horz_rows in
    let bingo_rows = horz_rows @ vert_rows in
    bingo_rows

let rec parse_cards cards parsed_cards =
    match cards with
    | [] -> List.rev parsed_cards
    | card_str::rem_cards_str ->
        let card = parse_card card_str in
        parse_cards rem_cards_str (card :: parsed_cards)

let parse_input input =
    match String.split_on_str "\n\n" input with
    | [] -> raise (Failure ("Could not parse input:\n" ^ input))
    | nums_str::cards_str -> 
        let nums_str = String.split_on_char ',' nums_str in
        let nums = List.map int_of_string nums_str in
        let cards = parse_cards cards_str [] in
        (nums, cards)

(* 
DESCRIPTION:

Marks number on the bingo cards passed in by removing
that number from the sublist of every bingo card that
it exists on.

PARAMETERS:

num = Number to be marked (removed) from every bingo card
cards = The list of bingo cards to be marked

RETURNS:

Returns a tuple that consists of two things:

1. The updated list of bingo cards where the number 
'num' has been removed. If bingo was reached, this
will only include all of the cards upto and including 
the card that got bingo (the winning card, the card 
that has one or more empty sublist.)

EDIT:

1. The updated list of bingo cards where the number 
'num' has been removed. If bingo was reached, this
will only include the card that got bingo (the winning card, 
the card that has one or more empty sublists.)

2. Bool that is TRUE if bingo was reached and FALSE otherwise.
*)
let mark_num_on_cards num cards =
    let rec next_card cards new_cards =
        match cards with
        | [] -> (List.rev new_cards, false)
        | card::rem_cards -> 
            (* Solution 1 *)
            (*let next_line bingo line = 
                let new_line = List.pop num line in
                (bingo || new_line = [], new_line)
            in
            let (marked_card, is_bingo) = List.fold_left_map False next_line card in*)

            (* Solution 2 *)
            let marked_card = List.map (fun line -> List.pop num line) card in
            let is_bingo = not (List.for_all (fun line -> line != []) marked_card) in

            (* Solution 3 (work in progress) *)
            (*
            (*let mark_card num card =
                let rec next_card_line card new_card =
                    match card with
                    | [] -> (List.rev new_card, False)
                    | line::rem_lines ->
                        let marked_line = List.pop num line in
                        let is_bingo = is_bingo || marked_line = [] in
                        if marked_line = [] then
                             (* BINGO! *)
                        else
                            next_line rem_lines (marked_line :: new_card)
                next_line card []
            *)

            let marked_card = mark_card num card in
            ...
            ...
            *)

            if is_bingo then
                ([marked_card], true)
            else
                next_card rem_cards (marked_card :: new_cards)
    in
    next_card cards []

(*
DESCRIPTION:

find_winning_card, pops one number at a time
from 'nums' and pops that number from every
sublist on the 'cards' if it exists.
This means that "marking" a number on a bingo
card is equivalent here to removing that
number from a sublist of a bingo card.

Function stops when it has run out of numbers
in 'nums' or when one sublist in any of the
cards have been emptied.

PARAMETERS:

nums = List of bingo numbers drawn and
       marked on all the bingo cards that
       number exists, one by one

cards = Bingo cards. It's a list of all rows
        and columns of the bingo card, meaning
        all sublists are a list consisting of 5
        decimal numbers and there should be
        in total 10 sublists, since the bingo
        card is a 5x5 grid (5 rows and 5 columns)

RETURNS:

If a sublist (line) in any of the cards in 'cards' was 
emptied before we've run out of numbers from 'nums', 
it will return that card, otherwise it returns 'None'.

EDIT:

Returns a tuple that consists of two things:

1. The winning bingo card or 'None' if no card got bingo.
   (Bingo is reached when one of the sublists in a card
    is empty.)

2. The last number from 'nums' that was drawn/marked before
   a card got "bingo". If no card got bingo before the numbers
   in 'nums' ran out, it's set to default value 0.

!! NOTE !! 
If it returns a card, the numbers that remains
in that card are all of the unmarked numbers of 
the winning bingo card.
*)

let rec find_winning_card nums cards =
    match nums with
    | [] -> None, 0
    | num::rem_nums ->
        let (new_cards, bingo) = mark_num_on_cards num cards in
        if bingo then
            (Some (List.hd new_cards), num) (* Here, "new_cards" is a list with only the winning card *)
        else
            find_winning_card rem_nums new_cards

(*
DESCRIPTION:

Prints the values/numbers of a bingo card that have 5x5 dimensions.
*)
let print_card card =
    let print_line line =
        List.iter (fun num -> print_string ("Num: " ^ (string_of_int num) ^ "\n")) line
    in
    List.iter print_line card

(* 
DESCRIPTION:

Calculates the sum of all the numbers in the bingo card passed in.

!! NOTE !!
This can be used whether the bingo card is a 5x5 list of list of integers
or if some of the numbers have been removed, as long as it's of the
type "int list list" it works.
 *)
let sum_bingo_numbers card =
    (* Use either only horizontal or vertical rows/lines when calculating sum, not both *)
    let horz_rows, _ = List.split_n card 5 in
    let sum_line accu_sum line = List.fold_left (+) accu_sum line in
    List.fold_left sum_line 0 horz_rows

let part1 input =
    let (nums, cards) = parse_input input in
    let (winning_card, winning_num) = find_winning_card nums cards in
    let unmarked_numbers_sum = sum_bingo_numbers (Option.get winning_card) in
    print_string ("Unmarked numbers sum = " ^ string_of_int unmarked_numbers_sum ^ "\n");
    print_string ("Winning number = " ^ string_of_int winning_num ^ "\n");
    print_string ("Part1 answer = " ^ string_of_int (unmarked_numbers_sum * winning_num) ^ "\n\n")

let () = 
    let input = input_string "input.txt" in
    part1 input
