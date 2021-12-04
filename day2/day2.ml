
let input_all_lines file =
    let rec build_list ic lines =
        match input_line ic with
        | exception End_of_file -> lines
        | line -> build_list ic (line::lines)
    in
    let ic = open_in file in
    let result = build_list ic [] in
    List.rev result

module List = struct
    include List
    let lsplit2_exn = Base.String.lsplit2_exn
end

(*********************************************************)

let part1a lines =
    let fold_horizontals accu str = 
        let (dir, num) = List.lsplit2_exn ~on:' ' str in
        if dir = "forward" then 
            (accu + int_of_string(num)) 
        else 
            accu
    in
    let horiz_pos = List.fold_left fold_horizontals 0 lines in
    print_string ("part1a - result horizontal pos: " ^ (string_of_int horiz_pos) ^ "\n");

    let fold_depth accu str =
        let (dir, num) = List.lsplit2_exn ~on:' ' str in
        if dir = "up" then 
            (accu - int_of_string(num))
        else if dir = "down" then
            (accu + int_of_string(num))
        else
            accu
    in
    let depth = List.fold_left fold_depth 0 lines in
    print_string ("part1a - result depth: " ^ (string_of_int depth) ^ "\n");

    print_string ("part1a - answer = " ^ (string_of_int (horiz_pos * depth)) ^ "\n\n")

let part1b lines =
    let parse_line (horz_accu, depth_accu) str = 
        let (dir, num_str) =  List.lsplit2_exn ~on:' ' str in
        let num = int_of_string num_str in
        match dir with
        | "forward" -> (horz_accu + num, depth_accu)
        | "up" -> (horz_accu, depth_accu - num)
        | "down" -> (horz_accu, depth_accu + num)
        | _ -> raise (Failure ("Unknown direction/command = " ^ dir))
    in
    let (horz, depth) = List.fold_left parse_line (0, 0) lines in

    print_string ("part1b - result horizontal pos: " ^ (string_of_int horz) ^ "\n");
    print_string ("part1b - result depth: " ^ (string_of_int depth) ^ "\n");
    print_string ("part1b - answer = " ^ (string_of_int (horz * depth)) ^ "\n\n")

let part2 lines =
    let parse_line (horz_accu, depth_accu, aim_accu) str =
        let (dir, num_str) = List.lsplit2_exn ~on:' ' str in
        let num = int_of_string num_str in
        match dir with
        | "forward" -> (horz_accu + num, depth_accu + num * aim_accu, aim_accu)
        | "up" -> (horz_accu, depth_accu, aim_accu - num)
        | "down" -> (horz_accu, depth_accu, aim_accu + num)
        | _ -> raise (Failure ("Unknown direction/command = " ^ dir))
    in
    let (horz, depth, _) = List.fold_left parse_line (0, 0, 0) lines in

    print_string ("part2 - result horizontal pos: " ^ (string_of_int horz) ^ "\n");
    print_string ("part2 - result depth: " ^ (string_of_int depth) ^ "\n");
    print_string ("part2 - answer = " ^ (string_of_int (horz * depth)) ^ "\n\n")

let () = 
    let lines = input_all_lines "input.txt" in
    part1a lines;
    part1b lines;
    part2 lines
