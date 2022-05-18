(** The interpretation monad *)
module type MONAD = sig
  type 'a t
  val ret: 'a -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val branch: (unit -> 'a t) list -> 'a t
  val fail: string -> 'a t
  val apply: ('a -> 'b t) -> 'a -> 'b t
  val extract: 'a t -> 'a
end


module Monad: MONAD = struct
	exception Branch_fail of string

	type 'a t = 'a

	let ret x = x

	let rec branch l =
		begin match l with
		| [] -> raise (Branch_fail "No branch matches")
		| b1 :: bq ->
				try b1 () with Branch_fail _ -> branch bq
		end

	let fail s = raise (Branch_fail s)

	let bind x f = f x

  let apply f x = f x

  let extract x = x
end
