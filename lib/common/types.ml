module type TYPES = sig
  type bool_t =
  | True
  | False

  type int_t
  type string_t
  type _ dict_t
  type _ queue_t
end

module Types = struct
  open Base

  type bool_t =
  | True
  | False
  
  type int_t = Int.t
  type string_t = String.t
  type 'v dict_t = 'v Map.M(String).t
  type 'v queue_t = 'v Queue.t
end
