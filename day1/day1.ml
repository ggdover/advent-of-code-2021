
let input_all_lines file =
    let rec build_list ic lines =
        match input_line ic with
        | exception End_of_file -> lines
        | line -> build_list ic (line::lines)
    in
    let ic = open_in file in
    let result = build_list ic [] in
    List.rev result

(* Works like "count" from Jane Street's Base library
   except the function takes in both the current and the
   next element. Stops when "next element" is the last
   element in the list, and just like "count" it returns
   the number of times that the function returned TRUE.
*)
let count_in_pairs list f =
    let rec inner hd tl accum =
        match tl with
        | [] -> accum
        | [last] -> Bool.to_int(f hd last) + accum
        | next_hd::next_tl -> 
            inner next_hd next_tl (Bool.to_int(f hd next_hd) + accum)
    in
    inner (List.hd list) (List.tl list) 0

module List = struct
    include List

    let groupi = Base.List.groupi

    let sum list = 
        match list with
        | [] -> 0
        | hd::tl -> List.fold_left (fun a b -> a + b) hd tl
    
    let pop_last list = List.rev (List.tl (List.rev list))

    let sub = Base.List.sub

    (* Works like Jane Steet Base's "chunks_of" function, except
       each chunk will have elements that overlaps with the next
       chunk.
       You can see it as a "sliding window" that is the size of "chunk_size"
       and starts from the beginning of the list and slides one element
       between each chunk and each time it generates a chunk including the
       elements that the "sliding window" at that moment includes.
       Example:
       "overlapping_chunks_of [1; 2; 3; 4; 5] 3"
       would give the result
       [[1; 2; 3]; [2; 3; 4]; [3; 4; 5]]

       TODO:
       A potential addition to this function would be to allow user to supply
       how much the window should slide in bewteen capturing each chunk,
       if the user would want it to be more than just one step at a time.
       *)
    let overlapping_chunks_of list chunk_size =
        let rec next_chunk pos accu =
            if ( pos + chunk_size ) > ( List.length list ) then
                accu
            else
                next_chunk (pos + 1) ((sub list pos chunk_size) :: accu)
        in
        let result = next_chunk 0 [] in
        List.rev result
end

(***********************************************************************)

(* Difference between part1a and part1b (just two different solutions)

    part1a = Converts from input line from string to int at each
            comparison.

    part1b = Converts all input lines before starting the counting/comparison
             into integers, generating a list of all the input as a "int list"
             before starting the comparison.

    Conclusion = I like part1b better, because the comparison function we supply
                 when calling "count_in_pairs" goes from being:
                 - (fun a b -> int_of_string(a) < int_of_string(b))
                 to simply
                 - (<)
                 in ocaml fashion!
*)
let part1a lines =
    let check_increase a b = int_of_string(a) < int_of_string(b) in
    let res = count_in_pairs lines check_increase in
    print_endline ("part1a result = " ^ (string_of_int res))

let part1b lines =
    let numbers = List.map int_of_string lines in
    let res = count_in_pairs numbers (<) in
    print_endline ("part1b result = " ^ (string_of_int res))

let part2 lines =
    (* A "number" here refers to a single sonar measure,
       in the type 'int'.
       See AdventOfCode assignment instructions for clarification. *)
    let numbers = List.map int_of_string lines in

    (* A "triplet" here means a "three-measurement sliding window".
       See AdventOfCode assignment instructions for clarification. *)
    let triplets = List.overlapping_chunks_of numbers 3 in
    let triplet_sums = List.map (fun a -> List.sum a) triplets in

    let res = count_in_pairs triplet_sums (<) in
    print_endline ("part2 result = " ^ (string_of_int res))

let () = 
    let lines = input_all_lines "input.txt" in
    part1a lines;
    part1b lines;
    part2 lines
