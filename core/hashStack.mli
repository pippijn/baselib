module Make : functor (T : Hashtbl.S) -> sig
  type 'a t

  val create : int -> 'a t
  val is_empty : 'a t -> bool
  val push : T.key -> 'a -> 'a t -> unit
  val pop : 'a t -> 'a
  val mem : 'a t -> T.key -> bool
  val find : 'a t -> T.key -> 'a
end
