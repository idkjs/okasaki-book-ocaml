module type S = sig
  exception Empty

  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool

  val snoc : 'a t -> value:'a -> 'a t
  val head : 'a t -> 'a
  val tail : 'a t -> 'a t
end
