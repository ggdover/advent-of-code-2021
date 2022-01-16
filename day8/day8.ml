
module String = struct
    include String
    let lsplit2_exn  = Base.String.lsplit2_exn 
    let to_list = Base.String.to_list
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
        String.split_on_char ' ' (String.trim pattern_str) in
    let new_digit_output = 
        String.split_on_char ' ' (String.trim digit_output_str) in
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

(*
1 -> 2 segments <---- unique

2 -> 5 segments
3 -> 5 segments

4 -> 4 segments <---- unique

5 -> 5 segments
6 -> 6 segments

7 -> 3 segments <---- unique
8 -> 7 segments <---- unique

9 -> 6 segments
*)


(* Digit is either 2, 5 or 3, figure out which with the
   help of parameter "pattern" *)
let decode_five_seg_digit digit pattern =
    let digit_one = List.find (fun p -> String.length p = 2) pattern in

    (*print_endline ("decode_five_seg_digit: digit_one = " ^ digit_one);*)

    (* Out of the digits 2, 5 and 3 we know that only the digit 3 contains
       both segments that makes up the digit 1. So if the following expression
       evaluates to true we know the digit is a three. *)
    let is_three = List.for_all (fun seg -> String.contains digit seg) (String.to_list digit_one) in
    if is_three then
        3
    else
        (* Check how many segments overlap *)
        let digit_four = List.find (fun p -> String.length p = 4) pattern in
        let overlapping_segs = List.count ~f:(fun seg -> String.contains digit seg) (String.to_list digit_four) in
        match overlapping_segs with
        | 3 -> 5 (* Digit 5 overlaps with 3 of the segments of digit 4 *)
        | 2 -> 2 (* Digit 2 overlaps with 2 of the segments of digit 4 *)
        | _ ->
            (* Wait a second. This isn't right!! *)
            raise (Failure ("Failed decode five seg. overlapping_segs = " ^ (string_of_int overlapping_segs)))

(* Digit is either 0, 6 or 9, figure out which with the
   help of parameter "pattern" *)
let decode_six_seg_digit digit pattern =
    let digit_four = List.find (fun p -> String.length p = 4) pattern in

    (* Out of the digits 0, 6 and 9 we know that only the digit 9 contains
       all segments that makes up the digit 4. So if the following expression
       evaluates to true we know the digit is a nine. *)
    let is_nine = List.for_all (fun seg -> String.contains digit seg) (String.to_list digit_four) in
    if is_nine then
        9
    else
        (* Check how many segments overlap *)
        let digit_one = List.find (fun p -> String.length p = 2) pattern in

        (* Between the digits 0 and 6, we know that only the digit 0 overlaps
           with all the segments of digit 1. So if the following expressions
           evaluates to true we know the digit is a zero. *)
        let is_zero = List.for_all (fun seg -> String.contains digit seg) (String.to_list digit_one) in
        if is_zero then
            0
        else
            6

(* Decodes the seven segment digit 'digit' from a
   string letter-pattern to a integer value.

   For any digit that doesn't have a unique number
   of segments, it will use the parameter 'pattern'
   which is a list of seven segment letter-patterns
   to figure out which seven segment digit the
   parameter 'digit' should be decoded to.
   
   Will raise exception when it finds a digit that it fails to decode. *)
let decode_seven_seg digit pattern =
    (*print_endline ("    seven_seg = " ^ digit ^ ", len = " ^ (string_of_int (String.length digit)));*)
    match String.length digit with
    | 2 -> 1 (* 1 on seven segment display uses 2 segments *)
    | 3 -> 7 (* 7 on seven segment display uses 3 segments *)
    | 4 -> 4 (* 4 on seven segment display uses 4 segments *)
    | 5 ->
        (* The digits 2, 3 and 5 all use 5 segments *)
        decode_five_seg_digit digit pattern
    | 6 ->
        (* The digits 0, 6 and 9 all use 6 segments *)
        decode_six_seg_digit digit pattern
    | 7 -> 8 (* 8 on seven segment display uses 7 segments *)
    | _ ->
        raise (Failure ("Cannot parse input digit value: " ^ digit))

let decode_unique_seven_seg digit =
    (*print_endline ("    seven_seg = " ^ digit ^ ", len = " ^ (string_of_int (String.length digit)));*)
    match String.length digit with
    | 2 -> 1 (* 1 on seven segment display uses 2 segments *)
    | 3 -> 7 (* 7 on seven segment display uses 3 segments *)
    | 4 -> 4 (* 4 on seven segment display uses 4 segments *)
    | 7 -> 8 (* 8 on seven segment display uses 7 segments *)
    | _ -> -1

(* "Unique digit" means a seven segment digit with a unique number of segments.
   Those digits happens to be: 1, 4, 7 and 8. *)
let is_unique_digit digit =
    List.mem (decode_unique_seven_seg digit) [1;4;7;8]

(************************************************************)

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

let part2 lines =
    let patterns, digit_outputs = parse_input_lines lines in
    let sum_digit_outputs accu_sum pattern digit_output =
        (*let inner (accu_output, pattern) digit =
            let output_value = decode_seven_seg digit pattern in
            accu_output ^ (string_of_int output_value)
        in
        let output_str = List.fold_left inner ("", pattern) digit_output in
        accu_sum + (int_of_string output_str)*)
        let inner accu_output digit =
            (*print_string ("GGDGGD digit = " ^ digit ^ " pattern = ");*)
            (*List.iter (fun p -> print_string (p ^ " ")) pattern;*)
            (*print_endline "";*)
            let output_value = decode_seven_seg digit pattern in
            accu_output ^ (string_of_int output_value)
        in
        let output_str = List.fold_left inner "" digit_output in
        (*print_endline ("output_str = " ^ output_str ^ "\n");*)
        accu_sum + (int_of_string output_str)
    in
    let answer = List.fold_left2 sum_digit_outputs 0 patterns digit_outputs in
    print_endline ("Part 2 answer = " ^ (string_of_int answer))

let () = 
    let lines = input_all_lines "input.txt" in
    part1 lines;
    part2 lines
