type 'a node
type 'a t
val is_empty : 'a t -> bool
val push : 'a t -> 'a -> unit
val pop : 'a t -> 'a option
val make : unit -> 'a t
