
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
(*
OLD VERSION OF mark_num_on_cards_old
---------- replaced by "mark_num_on_cards" ----------

let mark_num_on_cards_old num cards =
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
*)

(*
DESCRIPTION:

Runs through all bingo cards in 'cards'
and remove any number on any bingo card that
matches 'num'.

The function stops when it has run through all
cards in 'cards' or if it finds a "bingo" on one
of the cards before that (one of the sublists in
a card is empty after removing the number.)
it will stop and return.

RETURN

Returns a tuple including three things:

1. The remaining cards that didn't get processed
will only be non-empty if "bingo" was reached before
all the cards have been processed.

2. The list of updated cards that was accumulated
up to the point when function returned.
Will include all of the cards in 'cards' but with
the number 'num' removed from them (if it existed)
if no bingo was reached. If no bingo was reached it
will only hold all of the cards that have been
updated up to that point including the card that
received the bingo.
Since it's of the type "Stdlib.List", the elements
are added at the front, therefore, if bingo was reached,
the first element in this list will be the card that have
gotten "bingo".

3. Bool that is 'true' if bingo was reached and 'false' otherwise.
 *)
(*

---------- Replaced by "stop_at_bingo_num" ----------

let rec mark_num_on_cards num cards new_cards =
    match cards with
    | [] -> ([], List.rev new_cards, false)
    | card::rem_cards ->
        let marked_card = List.map (fun line -> List.pop num line) card in
        let is_bingo = not (List.for_all (fun line -> line != []) marked_card) in

        if is_bingo then
            (rem_cards, (marked_card :: new_cards, true)
        else
            mark_num_on_cards num rem_cards (marked_card :: new_cards)
*)
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
(*
OLD VERSION OF THIS FUNCTION

let rec find_winning_card nums cards =
    match nums with
    | [] -> None, 0
    | num::rem_nums ->
        let (_, new_cards, bingo) = mark_num_on_cards num cards [] in
        if bingo then
            (Some (List.hd new_cards), num) (* Here, "new_cards" is a list with only the winning card *)
        else
            find_winning_card rem_nums new_cards
*)

(********************************************************)

(*
DESCRIPTION:

Prints the values/numbers of a bingo card that have 5x5 dimensions.
*)
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

let find_winning_card nums cards =
    let (_, checked_nums, _, checked_cards, is_bingo) = stop_on_bingo_nums nums [] cards [] in

    if is_bingo then
        let winning_num = List.hd checked_nums in
        let winning_card = List.hd checked_cards in
        (Some winning_card, winning_num)
    else
        let _ = print_string "find_winning_card: Never found a winning card! wut?" in
        (None, 0)

(* Card that is guaranteed to lose *)
let rec find_last_winning_card nums cards =
(*
    DEBUG CODE

    print_cards cards;
    print_nums nums;
    print_string "-----------------\n";
*)

    let (rem_nums, checked_nums, rem_cards, checked_cards, is_bingo) = stop_on_bingo_nums nums [] cards [] in

    if is_bingo then

(*      
        DEBUG CODE

        let _ = print_string "Bingo card:\n" in
        let _ = print_card (List.hd checked_cards) in
        let _ = print_string ("Bingo num = " ^ string_of_int (List.hd checked_nums) ^ "\n") in
        let _ = print_nums rem_nums in
        let _ = print_string "#############################\n" in
*)
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
            find_last_winning_card new_nums new_cards
    else
        let _ = print_string "find_last_winning_card: ERROR Ran out of numbers and never found a card with bingo! wut?" in
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
    let (last_winning_card, last_winning_num) = find_last_winning_card nums cards in
    let unmarked_numbers_sum = sum_bingo_numbers (Option.get last_winning_card) in
    print_string ("Unmarked numbers sum = " ^ string_of_int unmarked_numbers_sum ^ "\n");
    print_string ("Winning number = " ^ string_of_int last_winning_num ^ "\n");
    print_string ("Part2 answer = " ^ string_of_int (unmarked_numbers_sum * last_winning_num) ^ "\n\n")

let () =
    let input = input_string "input.txt" in
    part1 input;
    part2 input
