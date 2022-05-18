(** The unspecified types *)
module type TYPES = sig
  type literal = Literal of int
  type value = Value of int
end

module Types: TYPES = struct
  type literal = Literal of int
  type value = Value of int
end
