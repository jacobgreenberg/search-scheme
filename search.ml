let unsorted_list = [6; 7; 4; 45; 7; 76; 3; 67; 7; 63; 19;]
let sorted_list = [2; 6; 10; 14; 55; 65; 78; 99; 102;]


(*  purpose:    check if second param is nil/empty
 *  input:      two params (likely two data structures)
 *  output:     true if second param is nil/empty, false otherwise
 *)
let second_empty first second = second == []


(*  purpose:    get tail of second param
 *  input:      two params (likely two data structures)
 *  output:     tail of second param
 *)
let tail_second first second = match second with 
    | [] -> raise (Failure "tail_second applied to []")
    | _ :: t -> t


(*  purpose:    get the head of an a' list
 *  input:      an a' list
 *  output:     the head of an a' list
 *)
let current_item ds = match ds with 
    | [] -> raise (Failure "current_item applied to []")
    | h :: _ -> h


(*  purpose:    compare two integers
 *  input:      two integers
 *  output:     true if equal, false otherwise
 *)
let int_equal first second = first == second


(*  purpose:    check if current element of sorted list is greater than needle
 *  input:      current element and a needle
 *  output:     true if needle is less than current element, false otherwise
 *)
let done_sorted e ds = match ds with
    | [] -> true
    | h :: _ -> e < h


(*  purpose:    search a data structure for an element
 *  input:      a data structure, a needle, function to get current item of data structure,
 *              function to check if search is complete, function to check if needle found,
 *              function to get the next element of the data structure
 *  output:     empty list if not found, a singleton lsit containg needle otherwise 
 *)
let rec search ds e current finished found next =
    if finished e ds then 
        [] 
    else if found e (current ds) then 
        [current ds] 
    else 
        search (next e ds) e current finished found next


(* call to search an unsorted list *)
let search_u ds e = search ds e current_item second_empty int_equal tail_second

(* call to search a sorted list *)
let search_s ds e = search ds e current_item done_sorted int_equal tail_second
