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
  | h :: t -> if k = 0 then Some h else at (k - 1) t

let rec length ?(n = 0) = function
  | [] -> n
  | h :: t -> length ~n:(n + 1) t

let rec reverse = function
  | [] -> []
  | h :: t -> List.concat [ reverse t; [ h ] ]

let is_palindrome lst = List.rev lst = lst

type 'a node =
  | One of 'a
  | Many of 'a node list

let rec flatten ?(acc = []) = function
  | [] -> acc
  | [ One x ] -> [ x ]
  | One x :: t -> x :: flatten t
  | Many x :: t -> List.append (flatten x) (flatten t)
