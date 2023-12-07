(** Types that support addition. *) 


(** Module signature for datatypes that support addition. *)
module type ADDITIVE =
  sig
    type c
    val zero : c
    val add : c -> c -> c
    val strictly_positive : c -> bool (* ou comparaison stricte *)
    val string_of_c : c -> string
  end


(** Example of module with module signature [ADDITIVE] for integers. *)
module IntC : ADDITIVE
	 with type c = int
  =
  struct
    type c = int
    let zero = 0
    let add = (+)
    let strictly_positive = (<) 0
    let string_of_c = string_of_int
  end
