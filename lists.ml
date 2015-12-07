open Core.Std

let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: t -> last t

let last_two = function
  | [] | [_] -> None
  | a :: b :: t -> Some (a, b)

let rec at k = function
  | [] -> None
  | h :: t when k = 0 -> Some h
  | h :: t -> at (k - 1) t

let rec length = function
  | [] -> 0
  | h :: t -> 1 + length t

let rec reverse = function
  | [] -> []
  | h :: t -> List.concat [ reverse t; [h] ]

let is_palindrome lst = List.rev lst = lst

type 'a node =
  | One of 'a
  | Many of 'a node list

let rec flatten = function
  | [] -> []
  | One x :: t -> x :: flatten t
  | Many x :: t -> flatten x @ flatten t

let rec compress = function
  | [] | [_] -> []
  | a :: (b :: _ as t) when a = b -> compress t
  | a :: (b :: _ as t) -> a :: compress t

let pack lst =
  let rec loop current acc = function
    | [] -> acc
    | [x] -> (x :: current) :: acc
    | a :: (b :: _ as t) -> if a = b then
                              loop (a :: current) acc t
                            else
                              loop [] ((a :: current) :: acc) t
  in loop [] [] (List.rev lst)

let encode lst =
  let rec loop count acc = function
    | [] -> []
    | [x] -> (x, count + 1) :: acc
    | a :: (b :: _ as t) -> if a = b then
                              loop (count + 1) acc t
                            else
                              loop 0 ((a, count + 1) :: acc) t
  in loop 0 [] (List.rev lst)

type 'a rle =
  | One of 'a
  | Many of 'a * int;;

let non_dup_encode lst =
  let create_tuple a = function
    | 1 -> One a
    | x -> Many (a, x) in
  let rec loop count acc = function
    | [] -> []
    | [x] -> create_tuple x (count + 1) :: acc
    | a :: (b :: _ as t) -> if a = b then
                              loop (count + 1) acc t
                            else
                              loop 0 (create_tuple a (count + 1) :: acc) t
  in loop 0 [] (List.rev lst)

let rec decode = function
  | [] -> []
  | [(a, n)] -> [String.make n (Char.of_string a)]
  | (a, n) :: t -> String.make n (Char.of_string a) :: decode t

let rec duplicate = function
  | [] -> []
  | [x] -> [ x; x ]
  | h :: t -> h :: h :: duplicate t
