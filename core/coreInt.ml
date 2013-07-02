include Deriving_Show.Show_int

type t = int

let compare a b = a - b

let t_of_sexp = Sexplib.Conv.int_of_sexp
let sexp_of_t = Sexplib.Conv.sexp_of_int

let rec fold_left f x l h =
  if l == h + 1 then
    x
  else
    fold_left f (f x l) (l + 1) h
