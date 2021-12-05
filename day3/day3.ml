
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

(**********************************************)

let part1 lines =
    let list_of_rows = List.map ~f:(fun str -> String.to_list str) lines in
    let list_of_columns = List.transpose_exn list_of_rows in
    let num_of_rows = List.length lines in

    let get_gamma_bit column = 
        let num_of_zeroes = List.count ~f:(fun a -> a = '0') column in
        let num_of_ones = num_of_rows - num_of_zeroes in
        Bool.to_int(num_of_ones > num_of_zeroes)
    in

    let get_rates (accu_gamma, accu_epsilon) col = 
        let gamma_bit_str = string_of_int (get_gamma_bit col) in
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

    print_string ("Part1 answer (power consump) = " ^ string_of_int (gamma_rate * epsilon_rate) ^ "\n")

let () = 
    let lines = input_all_lines "input.txt" in
    part1 lines
