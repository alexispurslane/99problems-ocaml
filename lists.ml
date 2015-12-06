let rec last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: t -> last t

let last_two = function
  | [] -> None
  | [ _ ] -> None
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
  | h :: t -> List.concat [ reverse t; [ h ] ]

let is_palindrome lst = List.rev lst = lst

type 'a node =
  | One of 'a
  | Many of 'a node list

let rec flatten = function
  | [] -> []
  | One x :: t -> x :: flatten t
  | Many x :: t -> flatten x @ flatten t
