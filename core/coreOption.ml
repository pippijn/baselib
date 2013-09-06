let return x = Some x
let bind m f =
  match m with
  | None -> None
  | Some x -> f x

let get = function
  | None -> raise Not_found
  | Some x -> x

let map f = function
  | None -> None
  | Some v -> Some (f v)

let default x = function
  | None -> x
  | Some v -> v
