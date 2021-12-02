
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
        | [last] -> 
            print_string ("hd = " ^ (string_of_int hd) ^ " last = " ^ (string_of_int last) ^ " accum = " ^ (string_of_int accum) ^ "\n");
            Bool.to_int(f hd last) + accum
        | next_hd::next_tl -> 
            print_string ("hd = " ^ (string_of_int hd) ^ " next_hd = " ^ (string_of_int next_hd) ^ " accum = " ^ (string_of_int accum) ^ "\n");
            inner next_hd next_tl (Bool.to_int(f hd next_hd) + accum)
    in
    inner (List.hd list) (List.tl list) 0

module List = struct
    include List

    let groupi = Base.List.groupi

    (* Need to use this instead of "stdlib's" version of "map"
       inorder to make sure we get back a list that is compatible
       with the type "Summable" from "Container_intf" so that we
       can use the list with the function "List.sum" from "Base" library *)
    (*let map = Base.List.map*)

    let sum list = 
        match list with
        | [] -> 0
        | hd::tl -> List.fold_left (fun a b -> a + b) hd tl
    
    let pop_last list = List.rev (List.tl (List.rev list))

    let sub = Base.List.sub

    let overlapping_chunks_of list chunk_size =
        let rec next_chunk pos accu =
            if ( pos + chunk_size ) > ( List.length list ) then
                (*
                This is what you would do if you want to include 
                the last group if it's smaller than "chunk_size".

                (* rem_len = remaining_length *)
                let rem_len = (List.length list) - pos in
                (sub list pos rem_len) :: accu

                But for the convenience of this assignment, where
                we don't care about this last group, we'll return
                "accu" directly
                *)
                accu
            else
                next_chunk (pos + 1) ((sub list pos chunk_size) :: accu)
        in
        let result = next_chunk 0 [] in
        List.rev result
end

(***********************************************************************)
(*
let part1a lines =
    let check_increase a b = int_of_string(a) < int_of_string(b) in
    let res = count_in_pairs lines check_increase in
    print_endline ("part1a result = " ^ (string_of_int res))
*)
let part1b lines =
    let numbers = List.map int_of_string lines in
    let res = count_in_pairs numbers (<) in
    print_endline ("part1b result = " ^ (string_of_int res))

(*
    OLD, first version that's incorrect, because I misinterpreted
    the assignment and though that measurements that would get group
    in triplets shouldn't be overlapping, so in a list like:
    1, 2, 3, 4, 5, 6, ...
    I thought the group should be (1, 2, 3), (4, 5, 6)
    but it should actually be (1, 2, 3), (2, 3, 4). (3, 4, 5), ....

let part2 lines =
    (* A "number" here refers to a single sonar measure,
       in the type 'int'.
       See AdventOfCode assignment instructions for clarification. *)
    let numbers = List.map ~f:int_of_string lines in

    (* A "triplet" here means a "three-measurement sliding window".
       See AdventOfCode assignment instructions for clarification. *)
    let triplets = List.groupi numbers ~break:(fun i _ _ -> i mod 3 = 0) in
    let fixed_triplets = 
        if (List.length numbers) mod 3 = 0 then 
            triplets
        else
            List.pop_last triplets
    in

    let triplet_sums = List.map fixed_triplets ~f:(fun a -> List.sum a) in

    let _ = List.iter (fun a -> print_int a;print_string "\n") triplet_sums  in

    let res = count_in_pairs triplet_sums (<) in
    print_endline ("part2 result = " ^ (string_of_int res)) *)

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
    part1b lines;
    part2 lines
