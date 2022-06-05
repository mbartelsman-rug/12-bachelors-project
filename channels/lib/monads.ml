module type MONAD = sig
  type 'a t

  module Infixes : sig
    val (>>=): 'a t -> ('a -> 'b t) -> 'b t
    val (>>|): 'a t -> ('a -> 'b) -> 'b t
  end

  val ret: 'a -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val map: 'a t -> ('a -> 'b) -> 'b t
  val branch: (unit -> 'a t) list -> 'a t
  val fail: string -> 'a t
  val apply: ('a -> 'b t) -> 'a -> 'b t
  val extract: 'a t -> 'a
end

(** Identity monad *)
module Identity : MONAD = struct
  exception BranchFail of string
  
  type 'a t = 'a

  let ret x = x
  let rec branch l =
    begin match l with
    | [] -> raise (BranchFail "No branch matches")
    | b1 :: bq ->
        try b1 () with BranchFail _ -> branch bq
    end
  let fail s = raise (BranchFail s)
  let bind x f = f x
  let map x f = f x
  let apply f x = f x
  let extract x = x

  module Infixes = struct
    let (>>=) = bind
    let (>>|) = map
  end

end
