
let input_all_lines file =
    let rec build_list ic lines =
        match input_line ic with
        | exception End_of_file -> lines
        | line -> build_list ic (line::lines)
    in
    let ic = open_in file in
    let result = build_list ic [] in
    List.rev result

module String = struct
    include String
    let to_list = Base.String.to_list
end

module List = struct
    include List
    let map = Base.List.map
    let transpose_exn = Base.List.transpose_exn
    let count = Base.List.count

    (* Attempt at creating a function that, given a list
       creates a hash table with a key for each unique element
       that appears in the input list and the value holding
       how many times that element/key appeared in the list.
    
    let to_freq_map list =
        let next_el list hashtbl =
            match list with
            | [] -> hashtbl
            | h::t -> 

        in
        next_el list (Hashtbl.create (List.length list))*)
end

module Array = struct
    include Array
    let transpose_exn = Base.Array.transpose_exn
end

module Bool = struct
    include Bool
    let to_int_char boolean = 
        Base.Char.of_string (Int.to_string (Bool.to_int boolean))
end

(**********************************************)

(* Returns TRUE if there are more '1's than '0's
   and also TRUE if there are equal number of '1's and '0's *)
let is_one_most_common bits = 
    let num_of_zeroes = List.count ~f:(fun a -> a = '0') bits in
    let num_of_ones = (List.length bits) - num_of_zeroes in
    let res =  num_of_ones >= num_of_zeroes  in
    print_string ("  num zeroes = " ^ (string_of_int num_of_zeroes) ^ " num ones = " ^ (string_of_int num_of_ones) ^ "\n");
    print_string ("    is_one = " ^ (Bool.to_string res) ^ "\n");
    res

let part1 lines =
    let list_of_rows = List.map ~f:(fun str -> String.to_list str) lines in
    let list_of_columns = List.transpose_exn list_of_rows in
    let num_of_rows = List.length lines in

    let get_rates (accu_gamma, accu_epsilon) col = 
        let gamma_bit_int = Bool.to_int (is_one_most_common col) in
        let gamma_bit_str = string_of_int gamma_bit_int in

        let epsilon_bit_str = if gamma_bit_str = "0" then "1" else "0" in
        (accu_gamma ^ gamma_bit_str, accu_epsilon ^ epsilon_bit_str)
    in
    let (gamma_rate_str, epsilon_rate_str) = 
        List.fold_left get_rates ("0b", "0b") list_of_columns in
    print_string ("Gamma rate = " ^ gamma_rate_str ^ "\n");
    print_string ("Epsilon rate = " ^ epsilon_rate_str ^ "\n");

    let gamma_rate = int_of_string gamma_rate_str in
    let epsilon_rate = int_of_string epsilon_rate_str in
    print_string ("Gamme rate (decimal) = " ^ string_of_int gamma_rate ^ "\n");
    print_string ("Epsilon rate (decimal) = " ^ string_of_int epsilon_rate ^ "\n");

    print_string ("Part1 answer (power consump) = " ^ string_of_int (gamma_rate * epsilon_rate) ^ "\n\n")

let print_str_list str_list = 
    let rec next_str l i =
        match l with
        | [] -> ()
        | h::t -> 
            print_string ("s" ^ (string_of_int i) ^ " = " ^ h ^ "\n");
            next_str t (i+1)
    in
    next_str str_list 0

(* This part2a version doesn't work.
   The problem with this one is that when it calculates the
   "most_common_bit" it does so from the original list of binary sequences
   instead of progressively calculate the next "most_common_bit" based
   on the current/remaining binary sequences for o2 and co2 ratings respectively.
   Because of this we get an incorrect final answer *)
let part2a lines =
    let list_of_rows = List.map ~f:(fun str -> String.to_list str) lines in
    let list_of_columns = List.transpose_exn list_of_rows in

    let parse_column (o2_list, co2_list, index) col =
        let most_common_bit_char =  Bool.to_int_char (is_one_most_common col) in
        print_string ("------### " ^ (string_of_int index) ^ " ###------\n");
        print_str_list o2_list; 
        print_string "\n";
        print_str_list co2_list;
        print_string "\n---------------------------\n";

        let o2_list = 
            if List.length o2_list > 1 then
                List.filter (fun row -> (String.get row index) = most_common_bit_char) o2_list
            else
                o2_list
        in

        let co2_list = 
            if List.length co2_list > 1 then
                List.filter (fun row -> (String.get row index) != most_common_bit_char) co2_list
            else
                co2_list
        in

        (*print_str_list o2_list; 
        print_string "\n";
        print_str_list co2_list;
        print_string "\n";*)
        (o2_list, co2_list, index + 1)
    in
    let (o2_list, co2_list, _) = 
        List.fold_left parse_column (lines, lines, 0) list_of_columns in

    print_string ("o2_list  result len = " ^ (string_of_int (List.length o2_list)) ^ "\n");
    print_string ("co2_list result len = " ^ (string_of_int (List.length co2_list)) ^ "\n");

    let o2_rating_str = List.hd o2_list in
    let co2_rating_str = List.hd co2_list in
    print_string ("o2  rating binary = " ^ o2_rating_str ^ "\n");
    print_string ("co2 rating binary = " ^ co2_rating_str ^ "\n");

    let o2_rating = int_of_string ("0b" ^ o2_rating_str) in
    let co2_rating = int_of_string ("0b" ^ co2_rating_str) in
    print_string ("o2 rating decimal = " ^ (string_of_int o2_rating) ^ "\n");
    print_string ("co2 rating decimal = " ^ (string_of_int co2_rating) ^ "\n");

    print_string ("Part 2 answer (life support rating) = " ^ string_of_int (o2_rating * co2_rating) ^ "\n\n")

let part2b lines =
    let next_filtering binseq_list bit_index is_o2_filtering =
        if (List.length binseq_list) <= 1 then
            binseq_list
        else
            let list_of_rows = List.map ~f:(fun str -> String.to_list str) binseq_list in
            let list_of_columns = List.transpose_exn list_of_rows in
            let col = List.nth list_of_columns bit_index in

            let most_common_bit = Bool.to_int_char (is_one_most_common col) in

            if is_o2_filtering then
                List.filter (fun binseq -> (String.get binseq bit_index) = most_common_bit) binseq_list
            else
                List.filter (fun binseq -> (String.get binseq bit_index) != most_common_bit) binseq_list
    in

    let rec next_bit o2_list co2_list i num_of_cols =
        if ( (List.length o2_list)  <= 1 && (List.length co2_list) <= 1 ) || 
             i = num_of_cols 
        then
            (List.hd o2_list, List.hd co2_list)
        else
            let o2_list = next_filtering o2_list i true in
            let co2_list = next_filtering co2_list i false in
            next_bit o2_list co2_list (i+1) num_of_cols
    in
    let (o2_rating_str, co2_rating_str) = next_bit lines lines 0 (String.length (List.hd lines)) in
    print_string ("o2 rating (binary) = " ^ o2_rating_str ^ "\n");
    print_string ("co2 rating (binary) = " ^ co2_rating_str ^ "\n");

    let o2_rating = int_of_string ("0b" ^ o2_rating_str) in
    let co2_rating = int_of_string ("0b" ^ co2_rating_str) in
    print_string ("o2 rating (decimal) = " ^ (string_of_int o2_rating) ^ "\n");
    print_string ("co2 rating (decimal) = " ^ (string_of_int co2_rating) ^ "\n");

    print_string ("Part 2 answer (life support rating) = " ^ string_of_int (o2_rating * co2_rating) ^ "\n")

let () = 
    let lines = input_all_lines "input.txt" in
    part1 lines;
    part2a lines;
    part2b lines
