open Sexplib.Conv

type t = int with sexp

let compare (a : t) (b : t) : int = a - b
let hash : t -> int = CorePervasives.identity
let equal : t -> t -> bool = (==)

external to_int : t -> int = "%identity"
external of_int : int -> t = "%identity"


let rec find f i e =
  if i <= e then
    if f i then
      i
    else
      find f (i + 1) e
  else
    raise Not_found

let find f e =
  find f 0 e


let rec fold_left f x i e =
  if i <= e then
    fold_left f (f x i) (i + 1) e
  else
    x

let fold_left f x e =
  fold_left f x 0 e


let iter f e =
  for i = 0 to e do
    f i
  done


let to_string (id : t) = string_of_int id
let print out (id : t) = output_string out (to_string id)
let sprint () = to_string

let default = 0
