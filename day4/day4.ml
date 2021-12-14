
(* Get all content of the file 'file' as one whole string. *)
let input_string file =
    let ic = open_in file in
    let str = really_input_string ic (in_channel_length ic) in
    close_in ic;
    str

module String = struct
    include String
    let split_on_str delim str = Str.split (Str.regexp delim) str
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

(********************************************************)
(* Printing functions for bingo cards. Works on cards of any dimensions.
   Can be used for debugging purposes. *)

let print_card card =
    let print_line line =
        print_string "Line: ";
        List.iter (fun num -> print_string (string_of_int num ^ " ")) line;
        print_string "\n";
    in
    List.iter print_line card;
    print_string "\n"

let rec print_cards cards =
    match cards with
    | [] -> ()
    | c::rem_cards -> 
        print_card c;
        print_cards rem_cards

let rec print_nums nums =
    match nums with
    | [] -> ()
    | n::rem_nums ->
        if rem_nums = [] then
            print_string ((string_of_int n) ^ "\n")
        else
            print_string ((string_of_int n) ^ ", ");
        print_nums rem_nums

(********************************************************)

(*  
Goes through cards in 'cards' and removes the number 'num'
from any line that it appears on. Stop if bingo is reached
before all cards have been checked (Find an empty line in
a card).

RETURNS

Tuple with 3 things

1. Cards that were not checked or updated. Guaranteed 
to be empty if bingo wasn't reached and might hold
some cards if bingo was reached.

2. The cards that have been checked and updated
(updated means that 'num' has been removed from
the lines it appears on in that card).

3. Bool that is 'true' if bingo was reached and 
'false otherwise.
*)
let rec stop_on_bingo_num num cards new_cards =
    match cards with
    | [] -> ([], new_cards, false)
    | card::rem_cards ->
        let marked_card = List.map (fun line -> List.pop num line) card in
        let is_bingo = not (List.for_all (fun line -> line != []) marked_card) in

        if is_bingo then
            (rem_cards, (marked_card :: new_cards), true)
        else
            stop_on_bingo_num num rem_cards (marked_card :: new_cards)

(* 
Takes one number from 'nums' at a time, starting with the
first element, and calls 'stop_on_bingo_num' on it.
Stops when we run out of numbers from 'nums' or when
'stop_on_bingo_num' returns that bingo was reached.

RETURNS

Tuple with 5 things

1. Numbers that weren't called with 'stop_on_bingo_num'.

2. Numbers that were called with 'stop_on_bingo_num'.
The number that was in the middle of being processed
when bingo was reched by 'stop_on_bingo_num' (assuming
that bingo was ever reached.) is included in this list
of numbers as the first element.

3. The list of cards that hadn't been checked yet that
round of calling 'stop_on_bingo_num' when it suddenly
finds a card with bingo.

4. The list of cards that had been checked by 'stop_on_bingo_num'.
The card that got 'bingo' is included in this list as the
first element.

5. Bool that is 'true' if bingo was reached and 
'false otherwise.
*)
let rec stop_on_bingo_nums nums new_nums cards new_cards  =
    match nums with
    | [] -> ([], new_nums, [], new_cards, false)
    | num::rem_nums ->
        let (rem_cards, new_cards, bingo) = stop_on_bingo_num num cards [] in

        if bingo then
            (rem_nums, (num :: new_nums), rem_cards, new_cards, true)
        else
            stop_on_bingo_nums rem_nums (num :: new_nums) new_cards []

(* The card that gets "Bingo" first *)
let find_winning_card nums cards =
    let (_, checked_nums, _, checked_cards, is_bingo) = stop_on_bingo_nums nums [] cards [] in

    if is_bingo then
        let winning_num = List.hd checked_nums in
        let winning_card = List.hd checked_cards in
        (Some winning_card, winning_num)
    else
        let _ = print_string "find_winning_card: Never found a winning card! wut?" in
        (None, 0)

(* The card that gets "Bingo" last *)
let rec find_losing_card nums cards =
    let (rem_nums, checked_nums, rem_cards, checked_cards, is_bingo) = stop_on_bingo_nums nums [] cards [] in

    if is_bingo then
        (* Include the num that got bingo (List.hd checked_nums) in the new list of nums
            If there are more cards that could get bingo with that same card. *)
        let new_nums = (List.hd checked_nums) :: rem_nums in

        (* The new list of cards includes all bingo cards but excludes
            the card that got bingo (the first element in 'checked_cards') *)
        let new_cards = (List.tl checked_cards) @ rem_cards in

        match new_cards with
        | [] ->
            (* No cards that haven't gotten bingo remaining. Return the last card that got bingo. *)
            (* We have found the last winning card *)
            (Some (List.hd checked_cards), (List.hd checked_nums))
        | _ ->
            (* More than one card left, loop again *)
            find_losing_card new_nums new_cards
    else
        let _ = print_string "find_losing_card: ERROR Ran out of numbers and never found a card with bingo! wut?" in
        (None, 0)

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

let part2 input =
    let (nums, cards) = parse_input input in
    let (last_winning_card, last_winning_num) = find_losing_card nums cards in
    let unmarked_numbers_sum = sum_bingo_numbers (Option.get last_winning_card) in
    print_string ("Unmarked numbers sum = " ^ string_of_int unmarked_numbers_sum ^ "\n");
    print_string ("Winning number = " ^ string_of_int last_winning_num ^ "\n");
    print_string ("Part2 answer = " ^ string_of_int (unmarked_numbers_sum * last_winning_num) ^ "\n\n")

let () =
    let input = input_string "input.txt" in
    part1 input;
    part2 input
