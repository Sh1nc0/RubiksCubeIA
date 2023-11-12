(** Types that support comparison *)

(**
Example of use of the OrderedType module signature defined in the Set module of the OCaml standard library.
 *)
module Int : Set.OrderedType with type t = int = struct
  type t = int

  let compare = (-)
end 


(** [nubOrd] with use of polymorphic comparison. *)
let nubOrd l = BatSet.elements (BatSet.of_list l)

