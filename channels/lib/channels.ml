(* The map is passed under a ref
   It holds the channel names or IDs mapped to the channel queues
   The channel queues are asynchronous, dequeuing returns deferreds
   If the map is empty, the deferred is unfilled until something is enqueuea
*)

module type INTERPRETER = sig
  type value
end

(** Models an async channel or queue. Requests
    are satisfied in the order in which they are
    made *)
module Channel (Interpreter: INTERPRETER) = struct
  open Base
  open Async

  type v = Interpreter.value
  type t = {
    ready:   v Ivar.t Queue.t ;
    pending: v Ivar.t Queue.t
  }

  (* Returns an empty channel *)
  let empty ((): unit): t = {
    pending = Queue.of_list [] ;
    ready   = Queue.of_list []
  }

  (** Enqueues an item into the channel. If
      there are processes already waiting for
      items, it fills their values without
      enqueuing them *)
  let enqueue (queue: t) (value: v): unit =
    match Queue.is_empty queue.pending with
    (* Pending dequeues, fulfill them by filling
       the ivar and dequeuing the request *)
    | false -> (
      let iv = Queue.dequeue_exn queue.pending in
      Ivar.fill iv value
    )
    (* No pending requests, simply enqueue the
       value in an ivar *)
    | true -> (
      let iv = Ivar.create_full value in
      Queue.enqueue queue.ready iv
    )

  (** Dequeues an item from the channel. If the
      channel is empty, a pending request is
      made that will be filled next time a value
      is enqueued *)
  let dequeue (queue: t): (v Deferred.t) =
    match Queue.is_empty queue.ready with
    (* There's ready items in the queue; dequeue
       and extract them from the ivar *)
    | false -> (
      let iv = Queue.dequeue_exn queue.ready in
      Ivar.read iv
    )
    (* There are no items ready; put in a
       pending request in the form of an ivar *)
    | true -> (
      let iv = Ivar.create () in
      Queue.enqueue queue.pending iv ;
      Ivar.read iv
    )

end

(** Models a set of channels in a concurrent
    program. Each channel is uniquely identified
    by a key *)
module Dispatcher (Interpreter: INTERPRETER) = struct
  open Base

  module V = Channel(Interpreter)
  module K = Int

  type v = V.t
  type k = K.t
  type t = v Map.M(K).t

  let next_id: int ref = ref 0

  let empty ((): unit): t = Map.empty (module Int)

  let new_channel (map: t): (t * k) =
    let ch = V.empty () in
    let id = !next_id in
    next_id := (!next_id + 1) ;
    (Map.add_exn map ~key:(id) ~data:(ch), id)

  let get_channel (map: t) (key: k): v =
    Map.find_exn map key
end

module Concurrent (Interpreter: INTERPRETER) = struct
  open Base

  module Channel = Channel (Interpreter)

  type channel_map = Channel.t Map.M(Int).t
end

