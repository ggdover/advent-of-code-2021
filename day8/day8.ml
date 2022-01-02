
module String = struct
    include String
    let lsplit2_exn  = Base.String.lsplit2_exn 
end

module List = struct
    include List
    let count = Base.List.count
end

(************************************************************)

let input_all_lines file =
    let rec build_list ic lines =
        match input_line ic with
        | exception End_of_file -> lines
        | line -> build_list ic (line::lines)
    in
    let ic = open_in file in
    let result = build_list ic [] in
    List.rev result

let parse_line line accu_patterns accu_digit_outputs =
    let pattern_str, digit_output_str = 
        String.lsplit2_exn  ~on:'|' line in
    let new_pattern = 
        String.split_on_char ' ' pattern_str in
    let new_digit_output = 
        String.split_on_char ' ' digit_output_str in
    (new_pattern :: accu_patterns), (new_digit_output :: accu_digit_outputs)

let parse_input_lines lines =
    let rec inner lines accu_patterns accu_digit_outputs =
        match lines with 
        | [] -> (List.rev accu_patterns, List.rev accu_digit_outputs)
        | line::rem_lines ->
            let new_patterns, new_digit_outputs = 
                parse_line line accu_patterns accu_digit_outputs in
            inner rem_lines new_patterns new_digit_outputs
    in
    inner lines [] []

(*
Version of "parse_input_lines" function
that uses mutable variables ("ref" and Array)
and for-loop, a more imperative style.

let parse_input_lines_2 lines =
    let lines_arr = Array.of_list lines in
    let patterns = ref []
    let digit_output = ref []
    for i = 0 to (Array.length lines_arr) do
        let line = Array.get lines_arr i in
        let pattern_str, digit_output_str = 
            String.lsplit2_exn  ~on:'|' line in
        let new_pattern = 
            String.split_on_char ' ' pattern_str in
        let new_digit_output = 
            String.split_on_char ' ' digit_output_str in
        patterns := (new_pattern :: patterns)
        digit_output := (new_digit_output :: digit_output)
    done;
    patterns, digit_output
*)

(************************************************************)

let decode_seven_seg seven_seg =
    (*print_endline ("    seven_seg = " ^ seven_seg ^ ", len = " ^ (string_of_int (String.length seven_seg)));*)
    match String.length seven_seg with
    | 2 -> 1 (* 1 on seven segment display uses 2 segments *)
    | 3 -> 7 (* 7 on seven segment display uses 3 segments *)
    | 4 -> 4 (* 4 on seven segment display uses 4 segments *)
    | 7 -> 8 (* 8 on seven segment display uses 7 segments *)
    | _ ->
        0
        (*raise (Failure ("Cannot parse input seven_seg value: " ^ seven_seg))*)

(* "Unique digit" means a seven segment digit with a unique number of segments.
   Those digits happens to be: 1, 4, 7 and 8. *)
let is_unique_digit digit =
    List.mem (decode_seven_seg digit) [1;4;7;8]

let part1 lines =
    let _, digit_outputs = parse_input_lines lines in
    let count_unique_digits count digit_output = 
        let inner accu digit =
            (*print_endline ("count_instances, count = " ^ (string_of_int count) ^ ", digit = " ^ digit);*)
            if is_unique_digit digit then accu + 1 else accu
        in
        (*print_endline ("count_instances 2, count = " ^ (string_of_int count));*)
        List.fold_left inner count digit_output
    in
    let answer = List.fold_left count_unique_digits 0 digit_outputs in
    print_endline ("Part 1 answer = " ^ (string_of_int answer))

let () = 
    let lines = input_all_lines "input.txt" in
    part1 lines
