open CorePervasives
open Sexplib.Conv

type ('a, 'mutability, 'integer) repr = 'a array with sexp

let append = Array.append

module Make(T : Sig.IntegralType) = struct
  type integer = T.t
  type ('a, 'm) t = ('a, 'm, T.t) repr

  (* private functions *)
  let mapi_fun f i e = f (T.of_int i) e

  (* public functions *)
  let t_of_sexp of_sexp _ sx = repr_of_sexp of_sexp identity identity sx
  let sexp_of_t sexp_of _ ar = sexp_of_repr sexp_of identity identity ar

  let readonly = identity

  let to_array = identity
  let to_list = Array.to_list

  let get array index =
    array.(T.to_int index)

  let set array index value =
    array.(T.to_int index) <- value

  let length = Array.length
  let last_index array = T.of_int (length array - 1)

  let empty = [||]
  let make = Array.make
  let init l f = Array.init l (fun i -> f (T.of_int i))

  let fold_left = Array.fold_left
  let fold_right = Array.fold_right
  let foldl_untili f = CoreArray.foldl_untili (mapi_fun f)
  let foldl_until = CoreArray.foldl_until
  let iteri f = Array.iteri (mapi_fun f)
  let iter = Array.iter
  let map = Array.map
  let mapi f = Array.mapi (mapi_fun f)
  let find = BatArray.find
  let exists = BatArray.exists
  let mem = BatArray.mem
  let memq = BatArray.memq

  let sum = CoreArray.sum
  let count = CoreArray.count
end

include Make(struct
  type t = int
  external to_int : t -> int = "%identity"
  external of_int : int -> t = "%identity"
end)
