(** [Util] contains helper functions used in other modules. *)

val explode : string -> char list
(** [explode] converts a string into a char list in the same order as
    the string.*)

val implode : char list -> bytes
(** [implode] converts and char list into a string by concatinating them
    together.*)

val build_list : int -> 'a -> 'a list
(** [build_list] creates a list containing [length] instances of [elm]*)