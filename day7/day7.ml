
let median arr =
    (* Can only calculate median on arrays with atleast 2 integers *)
    assert ((Array.length arr) >= 2);

    (* First sort the array in increasing order*)
    Array.sort (-) arr;

    if ((Array.length arr) mod 2) = 0 then
        (* There is an even number of elements *)
        (* Take the two middle numbers and return 
           the average of them *)
        let upper_mid_ix = (Array.length arr) / 2 in
        let lower_mid_ix = upper_mid_ix - 1 in

        let upper_mid_val = Array.get arr upper_mid_ix in
        let lower_mid_val = Array.get arr lower_mid_ix in 
        (upper_mid_val + lower_mid_val) / 2
    else
        (* There is an odd number of elements.
           Return the middle most value *)
        let mid_ix = ((Array.length arr) / 2) + 1 in
        Array.get arr mid_ix

(*
let average arr =
    (* Can only calculate average on arrays with atleast 2 integers *)
    assert ((Array.length arr) >= 2);

    let sum = Array.fold_left (+) 0 arr in
    sum / (Array.length arr)

let average_float arr =
    (* Can only calculate average on arrays with atleast 2 integers *)
    assert ((Array.length arr) >= 2);

    let sum = Array.fold_left (+) 0 arr in
    (float_of_int sum) /. (float_of_int (Array.length arr))
*)

(* Gauss formula for calculating the sum of number series
   that goes 1+2+3+4+5+....+n, where 'n' is the input number*)
let rec gauss_sum n =
    if n mod 2 = 0 then
        (* Number is even *)
        (n+1) * (n / 2)
    else
        (* Number is odd *)
        (gauss_sum (n-1)) + n

module Array = struct
    include Array
    let min arr =
        Array.fold_left (fun curr_min a -> if a < curr_min then a else curr_min) Int.max_int arr
    let max arr =
        Array.fold_left (fun curr_max a -> if a > curr_max then a else curr_max) 0 arr
end

(********************************************************************)

(* Get all content of the file 'file' as one whole string. *)
let input_string file =
    let ic = open_in file in
    let str = really_input_string ic (in_channel_length ic) in
    close_in ic;
    str

let parse_input str =
    let numbers_str = String.split_on_char ',' str in
    List.map (fun num_str -> int_of_string num_str) numbers_str

(********************************************************************)

(* Returns as an 'int' the total fuel required to move
   all the horizontal crab positions in "positions" to
   the horizontal position "dest".

   For Part 1 *)
let calc_fuel dest positions_arr =
    (* Includes debug printing

       Array.fold_left (fun sum a -> 
                            print_endline ("a: " ^ (string_of_int a) ^ " sum: " ^ (string_of_int sum) ^ " abs: " ^ (string_of_int (abs (dest-a))) ^ " new_sum: " ^ (string_of_int ((abs (dest-a)) + sum)) );
                            (abs (dest-a)) + sum ) 0 positions_arr*)
    Array.fold_left (fun sum a -> (abs (dest-a)) + sum ) 0 positions_arr

(* Returns as an 'int' the total fuel required to move
   all the horizontal crab positions in "positions" to
   the horizontal position "dest".

   For Part 2 *)
let calc_fuel_2 dest positions_arr =
    let fuel_for_dist pos_a pos_b =
        gauss_sum (abs (pos_a - pos_b))
    in
    Array.fold_left (fun sum a -> (fuel_for_dist a dest) + sum ) 0 positions_arr

(********************************************************************)

(* The correct position to get the lowest fuel cost for
   this assignment being the same as the "median" might
   at first glance seem like a pure guess.
   But if you look up the definition of "Geometric median"
   you'll realize that the position you're looking to find
   in this assignment is the exact same as "Geometric median".
   You can find this information on for example this wikipedia
   page: https://en.wikipedia.org/wiki/Geometric_median 

   In fact, the first sentence on this wikipedia page, at least
   as I write this, reads as follows: 
   "The geometric median of a discrete set of sample points in a 
    Euclidean space is the point minimizing the sum of distances 
    to the sample points" 

    It even goes on to say:
    For the 1-dimensional case, the geometric median coincides with the median.
    This is because the univariate median also minimizes the sum of distances 
    from the points. (More precisely, if the points are p1, …, pn, in that order, 
    the geometric median is the middle point "p {( n + 1 ) / 2}" if n is odd, but 
    is not uniquely determined if n is even, when it can be any point in the line 
    segment between the two middling points p {n / 2} and p {( n / 2 ) + 1} . *)
let part1 positions =
    let pos_arr = Array.of_list positions in
    (*print_endline ("Avg = " ^ (string_of_int (average pos_arr)));*)
    print_endline ("Part1 position (median) = " ^ (string_of_int (median pos_arr)));
    (*print_endline ("Calc fuel avg = " ^ (string_of_int (calc_fuel (average pos_arr) pos_arr)));*)
    let fuel_cost = calc_fuel (median pos_arr) pos_arr in
    print_endline ("Part1 (min fuel cost) = " ^ (string_of_int fuel_cost))

let part2 positions =
    let pos_arr = Array.of_list positions in
    let min_fuel_cost = ref Int.max_int in
    print_endline ("Part2 Min crab position = " ^ (string_of_int (Array.min pos_arr)));
    print_endline ("Part2 Max crab position = " ^ (string_of_int (Array.max pos_arr)));
    (* Common sense tells us that at the very least, the position to produce the least
       fuel cost doesn't lie anywhere outside the minimum or maximum crab position, as
       any position beyond any of those points is guaranteed to create a greater total 
       fuel cost. *)
    for pos = (Array.min pos_arr) to (Array.max pos_arr) do
        let fuel_cost = calc_fuel_2 pos pos_arr in
        (*print_endline ("Calc fuel (" ^ (string_of_int pos) ^ ") = " ^ (string_of_int fuel_cost))*)
        if fuel_cost < !min_fuel_cost then begin
            min_fuel_cost := fuel_cost;
            (*print_endline ("Min fuel updated (pos = " ^ (string_of_int pos) ^ ", new min fuel = " ^ (string_of_int !min_fuel_cost) ^ ")")*)
        end
    done;
    print_endline ("Part2 (min_fuel_cost) = " ^ (string_of_int !min_fuel_cost))

let () = 
    let input_str = input_string "input.txt" in
    let positions = parse_input input_str in
    part1 positions;
    part2 positions
    (* Test the gauss_sum function.
    assert ((gauss_sum 7) = 28);
    assert ((gauss_sum 16) = 136);
    assert ((gauss_sum 3) = 6)*)
