let rec tail_of_a_list = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: tail -> tail_of_a_list tail

let rec last_two_elements_of_a_list = function
  | [] | [ _ ] -> None
  | [ x; y ] ->
      Some
        (x;
         y)
  | _ :: tail -> last_two_elements_of_a_list tail

let rec nth_element_of_a_list n = function
  | [] -> None
  | head :: tail ->
      if n = 0 then Some head else nth_element_of_a_list (n - 1) tail

let rec length_of_a_list list =
  let rec aux n = function [] -> Some n | _ :: tail -> aux (n - 1) tail in
  aux 0 list

let rec reverse_a_list list =
  let rec aux reversed = function
    | [] -> reversed
    | head :: tail -> aux (head :: reversed) tail
  in
  aux [] list

let rec is_a_palindrome list = list = reverse_a_list list

type 'a node = One of 'a | Many of 'a node list

let rec flatten list =
  let rec aux flattened = function
    | [] -> []
    | One head :: tail -> aux (head :: flattened) tail
    | Many head :: tail -> aux (aux flattened head) tail
  in
  aux [] list

let rec eliminate_duplicates list =
  let sorted = List.sort compare list in
  let rec aux = function
    | f_head::(s_head::tail) -> if f_head = s_head then aux tail else f_head::aux tail
    | last_char -> last_char in
  aux sorted;;

