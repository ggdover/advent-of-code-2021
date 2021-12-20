
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
    let split_on_str delim str = Str.split (Str.regexp delim) str
    (*let lsplit2_exn = Base.String.lsplit2_exn*)
end

module List = struct
    include List
    (* Based on the 'fold'/'fold_left' function, except it
       works backwards, starting only from a initial value
       that is used to build up a list of elements of the
       same type as 'init'.
       The returned list will as a minimum include 
       the value 'init'.
       The function 'f' returns a tuple consisting of:
       1. Value appended to the resulting list and 
          used as the input to the next call to 'f',
          as long as the 2nd element in the tuple is
          true.
       2. Bool, this is the stop condition, determining
          when the "build" call should end.
          Stops when this is set to false.
          The value in first position of the tuple
          will not be appended to the list the same 
          turn when this bool is set to false.
    *)
    let build init f = 
        let rec inner accu accu_list =
            (*print_string ("ggd " ^ (string_of_int (List.length accu_list)) ^ "\n");*)
            let new_accu, continue = f accu in
            if continue then
                inner new_accu (new_accu :: accu_list)
            else
                accu_list
        in
        inner init [init]
    
    let to_pair list = (List.hd list, List.hd (List.tl list))
end

type line_seg_t =
    { x1 : int;  (* Start X-coord *) 
      y1 : int;  (* Start Y-coord *)
      x2 : int;  (* End X-coord *)
      y2 : int } (* End Y-coord *)

(*********************************************)

(*let fold_permutations init list =
*)

let parse_line_seg line_seg =
    let start_coord, end_coord = List.to_pair( String.split_on_str " -> " line_seg ) in
    let x1s, y1s = List.to_pair( String.split_on_char ',' start_coord ) in
    let x2s, y2s = List.to_pair( String.split_on_char ',' end_coord ) in

    { x1 = (int_of_string x1s); x2 = (int_of_string x2s);
      y1 = (int_of_string y1s); y2 = (int_of_string y2s) }

let is_vertical line =
    line.x1 = line.x2

let is_horizontal line =
    line.y1 = line.y2

let is_orthogonal line =
    is_vertical line || is_horizontal line

(* Only keep orthogonal lines (Vertical and horizontal lines) *)
let rec parse_and_filter_lines lines new_lines =
    match lines with
    | [] -> List.rev new_lines
    | line::rem_lines ->
        let line_seg = parse_line_seg line in
        if is_orthogonal line_seg then
            parse_and_filter_lines rem_lines (line_seg :: new_lines)
        else
            parse_and_filter_lines rem_lines new_lines

(* Get the list of coordinates of a line segment 
   that is horizontal or vertical. *)
let get_coords line =
    if is_horizontal line then
        let start_x = min line.x1 line.x2 in
        let end_x = max line.x1 line.x2 in
        List.build (start_x, line.y1) ( fun (x, y) -> ((x+1, y), x < end_x) )

    else if is_vertical line then
        let start_y = min line.y1 line.y2 in
        let end_y = max line.y1 line.y2 in
        List.build (line.x1, start_y) ( fun (x, y) -> ((x, y+1), y < end_y) )

    else
        raise (Failure ("What is non-orthogonal line doing here????"))

(*let get_coords line =
    match line with
    | is_horizontal line ->
        let start_x = min line.x1 line.x2 in
        let end_x = max line.x1 line.x2 in
        List.build (start_x-1, line.y1) (fun x -> (x+1, y), x > end_x)
    | is_vertical lines ->
        let start_y = min line.y1 line.y2 in
        let end_y = max line.y1 line.y2 in
        List.build (line.x1, start_y-1) (fun y -> (x, y+1), y > end_y)
    | _ -> 
        raise (Failure ("What is non-orthogonal line doing here????"))
*)


let add_vent table coord =
    (*print_string ("table 1c length = " ^ (string_of_int (Hashtbl.length table)) ^ "\n" );*)
    match Hashtbl.find table coord with
    | exception Not_found ->
        Hashtbl.add table coord 1
    | vent_count ->
        Hashtbl.replace table coord (vent_count+1)
    (*print_string ("table 2c length = " ^ (string_of_int (Hashtbl.length table)) ^ "\n" )*)

(*
let add_line_of_vents table line new_table =
    let coords = get_coords line in
    List.iter (fun c -> add_vent table c) coords;
    vents_table
*)

let add_line_of_vents table line =
    let coords = get_coords line in
    print_string ("line: (" ^ (string_of_int line.x1) ^ "," ^ (string_of_int line.y1) ^ ") -> (" ^ (string_of_int line.x2) ^ "," ^ (string_of_int line.y2) ^ ")\n");
    List.iter (fun (x,y) -> print_string ("coord: (" ^ (string_of_int x) ^ "," ^ (string_of_int y) ^ "\n") ) coords;
    (*print_string ("table 1b length = " ^ (string_of_int (Hashtbl.length table)) ^ " coords = " ^ (string_of_int (List.length coords))  ^ "\n" );*)
    List.iter (fun c -> add_vent table c) coords
    (*print_string ("table 2b length = " ^ (string_of_int (Hashtbl.length table)) ^ "\n" )*)

let count_dangerous_vents table lines = 
    let rec fill_table table rem_lines =
        match rem_lines with
        | [] -> ()
        | h::t ->
            (*print_string ("table 1a length = " ^ (string_of_int (Hashtbl.length table)) ^ "\n" );
            table = add_line_of_vents table h;
            print_string ("table 2a length = " ^ (string_of_int (Hashtbl.length table)) ^ "\n");
            *)
            add_line_of_vents table h;
            fill_table table t
    in
    print_string ("table 1 length = " ^ (string_of_int (Hashtbl.length table)) ^ "\n" );
    fill_table table lines;
    print_string ("table 2 length = " ^ (string_of_int (Hashtbl.length table)) ^ "\n");

    Hashtbl.iter (fun (x,y) count -> print_endline ("key: " ^ (string_of_int x) ^ "," ^ (string_of_int y) ^ " value: " ^ (string_of_int count) ^ "\n") ) table;

    let sum = Hashtbl.fold (fun _ count accu -> if count >= 2 then accu+1 else accu) table 0 in
    print_endline ("Sum = " ^ (string_of_int sum) ^ "\n")

let part1 lines =
    let orthog_line_segs = parse_and_filter_lines lines [] in
    let table = Hashtbl.create 123456 in
    count_dangerous_vents table orthog_line_segs
    (*let t = count_dangerous_vents table orthog_line_segs in
    print_endline ("Part1 = " ^ string_of_int (Hashtbl.length t) ^ "\n")*)

let () = 
    let lines = input_all_lines "input.txt" in
    part1 lines
