
module List = struct
    include List
    let count = Base.List.count
end

(* modulus that handles both positive and negative numbers *)
let modulo a b =
    let res = a mod b in
    if res >= 0 then res else res + b

(* Increment table (Hashtbl) 
   PARAMETERS:
   - table = Hashtbl where value is of type "int"
   - key = Key in Hashtbl
   - step = integer representing the value of which the value
            with key "key" should increment with.
            If a value with key "key" doesn't exist in table
            "table", then the value of "step" will be added
            with key "key" to the table. *)
let increment_table table key step =
    match Hashtbl.find table key with
    | exception Not_found ->
        Hashtbl.add table key step
    | value ->
        Hashtbl.replace table key (value+step)

(* Convert fishes from existing in a list of integers
   to a Hashtbl where the 'key' is the fish age/generation number
   and the 'value' is the number of fishes with that fish age/generation number*)
let fishes_to_table fishes =
    let table = Hashtbl.create 8 in
    let rec inner fish_gen =
        if fish_gen > 8 then
            (* Stop condition, there is no fishes with generation value greater than 8 *)
            table
        else
            let fish_count = List.count fishes ~f:( fun f -> f = fish_gen ) in
            Hashtbl.add table fish_gen fish_count;
            inner (fish_gen+1)
    in
    inner 0

(* Used for debugging purposes.
   Prints all the fishes that exist as a list of integers. *)
let print_fishes fishes =
    print_string "Fishes: ";
    List.iter (fun f -> print_string ((string_of_int f) ^ ",")) fishes;
    print_endline "\n"

(**************************************************************)
(******* Functions for reading and parsing puzzle input *******)

(* Get all content of the file 'file' as one whole string. *)
let input_string file =
    let ic = open_in file in
    let str = really_input_string ic (in_channel_length ic) in
    close_in ic;
    str

(* Given the whole puzzle input as one long string.
   Parse it by returning a list of the numbers/fishes
   as a list of integers. *)
let parse_input str =
    let numbers_str = String.split_on_char ',' str in
    List.map (fun num_str -> int_of_string num_str) numbers_str

(********************************************************)
(* Update the fishes for what they 
   will look like the next day *)

(* First version, uses a simple approach of adding and updating
   The fish numbers (age/generation number) to a list.
   This approach works for Part1 but for part2 it's way too inefficient,
   and takes way too long to finish. *)
let shift_day fishes =
    let rec inner fishes new_fishes =
        match fishes with
        | [] -> List.rev new_fishes
        | curr_fish::rem_fishes ->
            if curr_fish = 0 then
                inner rem_fishes (8 :: 6 :: new_fishes)
            else
                inner rem_fishes ( (curr_fish-1) :: new_fishes )
    in
    inner fishes []

(* Second version, uses a hashmap instead and stores a key/value
   pair where key is a fish generation/age number (from 0 to 8),
   and value is the number of fishes that have that generation/age
   number currently.
   Alot more efficient than the first version of this function, at least
   when it grows into very large number of fishes. *)
let shift_day_2 table =
    let old_table = Hashtbl.copy table in
    Hashtbl.clear table;

    let rec inner fish_gen =
        if fish_gen > 8 then
            (* no fish generation greater than 8, so lets quit *)
            ()
        else
            match Hashtbl.find old_table fish_gen with
            | exception Not_found ->
                (* There exists no fishes with age/generation number 'fish_gen'.
                   Just skip and go to the next fish age/generation number *)
                inner (fish_gen+1)
            | fish_count -> 
                if fish_gen = 0 then
                    (* All fishes that are 0, turn to 6
                       and then aditionally, for each, 
                       add a new/produced fish with a value of 8 *)
                    let _ = increment_table table 6 fish_count in
                    increment_table table 8 fish_count
                else begin
                    (* For any other fish generation, add
                       those number of fishes to the decremented
                       level of fish generation. *)
                    increment_table table (fish_gen-1) fish_count
                end;
                inner (fish_gen+1)
    in
    inner 0

(***************************************************************** *)

let part1a fishes =
    (* Using mutable variable with keyword 'ref' for the first time ever! :D *)
    let fish_accu = ref fishes in
    for _ = 1 to 80 do
        fish_accu := shift_day !fish_accu
        (*print_fishes !fish_accu*)
    done;
    let sum_fishes = List.length (!fish_accu) in
    print_endline ("Part1a = " ^ (string_of_int sum_fishes))

let part1b fishes =
    (* Note! Hashtbl is a mutable datastructure in Ocaml,
       so when we pass it into "shift_day_2" it will modify
       the existing varible "fish_table", so we don't have to
       care about some return value from "shift_day_2". *)
    let fish_table = fishes_to_table fishes in
    for _ = 1 to 80 do
        shift_day_2 fish_table
    done;
    let sum_fishes = Hashtbl.fold (fun _ fish_count sum -> fish_count + sum) fish_table 0 in
    print_endline ("part1b = " ^ (string_of_int sum_fishes))

let part2 fishes =
    let fish_table = fishes_to_table fishes in
    for _ = 1 to 256 do
        shift_day_2 fish_table
    done;
    let sum_fishes = Hashtbl.fold (fun _ fish_count sum -> fish_count + sum) fish_table 0 in
    print_endline ("part2 = " ^ (string_of_int sum_fishes))

let () = 
    let content = input_string "input.txt" in
    let fishes = parse_input content in
    part1a fishes;
    part1b fishes;
    part2 fishes
