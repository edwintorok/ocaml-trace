open Trace_core
open Trace_private_util
open Event
module A = Trace_core.Internal_.Atomic_

let ( let@ ) = ( @@ )

module Mock_ = struct
  let enabled = ref false
  let now = ref 0

  let[@inline never] now_us () : float =
    let x = !now in
    incr now;
    float_of_int x
end

(** Now, in microseconds *)
let[@inline] now_us () : float =
  if !Mock_.enabled then
    Mock_.now_us ()
  else (
    let t = Mtime_clock.now () in
    Int64.to_float (Mtime.to_uint64_ns t) /. 1e3
  )

let on_tracing_error = ref (fun s -> Printf.eprintf "trace-tef error: %s\n%!" s)

module Span_tbl = Hashtbl.Make (struct
  include Int64

  let hash : t -> int = Hashtbl.hash
end)

type span_info = {
  tid: int;
  name: string;
  start_us: float;
  mutable data: (string * user_data) list;
}

(** key used to carry a unique "id" for all spans in an async context *)
let key_async_id : int Meta_map.key = Meta_map.Key.create ()

let key_async_data : (string * [ `Sync | `Async ] option) Meta_map.key =
  Meta_map.Key.create ()

let key_data : (string * user_data) list ref Meta_map.key =
  Meta_map.Key.create ()

(** Writer: knows how to write entries to a file in TEF format *)
module Writer = struct
  type t = {
    oc: out_channel;
    jsonl: bool;  (** JSONL mode, one json event per line *)
    mutable first: bool;  (** first event? useful in json mode *)
    buf: Buffer.t;  (** Buffer to write into *)
    must_close: bool;  (** Do we have to close the underlying channel [oc]? *)
    pid: int;
  }
  (** A writer to a [out_channel]. It writes JSON entries in an array
      and closes the array at the end. *)

  let create ~(mode : [ `Single | `Jsonl ]) ~out () : t =
    let jsonl = mode = `Jsonl in
    let oc, must_close =
      match out with
      | `Stdout -> stdout, false
      | `Stderr -> stderr, false
      | `File path -> open_out path, true
      | `File_append path ->
        open_out_gen [ Open_creat; Open_wronly; Open_append ] 0o644 path, true
      | `Output oc -> oc, false
    in
    let pid =
      if !Mock_.enabled then
        2
      else
        Unix.getpid ()
    in
    if not jsonl then output_char oc '[';
    { oc; jsonl; first = true; pid; must_close; buf = Buffer.create 2_048 }

  let close (self : t) : unit =
    if self.jsonl then
      output_char self.oc '\n'
    else
      output_char self.oc ']';
    flush self.oc;
    if self.must_close then close_out self.oc

  let with_ ~mode ~out f =
    let writer = create ~mode ~out () in
    Fun.protect ~finally:(fun () -> close writer) (fun () -> f writer)

  let[@inline] flush (self : t) : unit = flush self.oc

  (** Emit "," if we need, and get the buffer ready *)
  let emit_sep_and_start_ (self : t) =
    Buffer.reset self.buf;
    if self.jsonl then
      Buffer.add_char self.buf '\n'
    else if self.first then
      self.first <- false
    else
      Buffer.add_string self.buf ",\n"

  let char = Buffer.add_char
  let raw_string = Buffer.add_string

  let str_val (buf : Buffer.t) (s : string) =
    char buf '"';
    let encode_char c =
      match c with
      | '"' -> raw_string buf {|\"|}
      | '\\' -> raw_string buf {|\\|}
      | '\n' -> raw_string buf {|\n|}
      | '\b' -> raw_string buf {|\b|}
      | '\r' -> raw_string buf {|\r|}
      | '\t' -> raw_string buf {|\t|}
      | _ when Char.code c <= 0x1f ->
        raw_string buf {|\u00|};
        Printf.bprintf buf "%02x" (Char.code c)
      | c -> char buf c
    in
    String.iter encode_char s;
    char buf '"'

  let pp_user_data_ (out : Buffer.t) : [< user_data ] -> unit = function
    | `None -> raw_string out "null"
    | `Int i -> Printf.bprintf out "%d" i
    | `Bool b -> Printf.bprintf out "%b" b
    | `String s -> str_val out s
    | `Float f -> Printf.bprintf out "%g" f

  (* emit args, if not empty. [ppv] is used to print values. *)
  let emit_args_o_ ppv (out : Buffer.t) args : unit =
    if args <> [] then (
      Printf.bprintf out {json|,"args": {|json};
      List.iteri
        (fun i (n, value) ->
          if i > 0 then raw_string out ",";
          Printf.bprintf out {json|"%s":%a|json} n ppv value)
        args;
      char out '}'
    )

  let emit_duration_event ~tid ~name ~start ~end_ ~args (self : t) : unit =
    let dur = end_ -. start in
    let ts = start in

    emit_sep_and_start_ self;

    Printf.bprintf self.buf
      {json|{"pid":%d,"cat":"","tid": %d,"dur": %.2f,"ts": %.2f,"name":%a,"ph":"X"%a}|json}
      self.pid tid dur ts str_val name
      (emit_args_o_ pp_user_data_)
      args;
    Buffer.output_buffer self.oc self.buf

  let emit_manual_begin ~tid ~name ~id ~ts ~args ~flavor (self : t) : unit =
    emit_sep_and_start_ self;
    Printf.bprintf self.buf
      {json|{"pid":%d,"cat":"trace","id":%d,"tid": %d,"ts": %.2f,"name":%a,"ph":"%c"%a}|json}
      self.pid id tid ts str_val name
      (match flavor with
      | None | Some `Async -> 'b'
      | Some `Sync -> 'B')
      (emit_args_o_ pp_user_data_)
      args;
    Buffer.output_buffer self.oc self.buf

  let emit_manual_end ~tid ~name ~id ~ts ~flavor ~args (self : t) : unit =
    emit_sep_and_start_ self;
    Printf.bprintf self.buf
      {json|{"pid":%d,"cat":"trace","id":%d,"tid": %d,"ts": %.2f,"name":%a,"ph":"%c"%a}|json}
      self.pid id tid ts str_val name
      (match flavor with
      | None | Some `Async -> 'e'
      | Some `Sync -> 'E')
      (emit_args_o_ pp_user_data_)
      args;
    Buffer.output_buffer self.oc self.buf

  let emit_instant_event ~tid ~name ~ts ~args (self : t) : unit =
    emit_sep_and_start_ self;
    Printf.bprintf self.buf
      {json|{"pid":%d,"cat":"","tid": %d,"ts": %.2f,"name":%a,"ph":"I"%a}|json}
      self.pid tid ts str_val name
      (emit_args_o_ pp_user_data_)
      args;
    Buffer.output_buffer self.oc self.buf

  let emit_name_thread ~tid ~name (self : t) : unit =
    emit_sep_and_start_ self;
    Printf.bprintf self.buf
      {json|{"pid":%d,"tid": %d,"name":"thread_name","ph":"M"%a}|json} self.pid
      tid
      (emit_args_o_ pp_user_data_)
      [ "name", `String name ];
    Buffer.output_buffer self.oc self.buf

  let emit_name_process ~name (self : t) : unit =
    emit_sep_and_start_ self;
    Printf.bprintf self.buf
      {json|{"pid":%d,"name":"process_name","ph":"M"%a}|json} self.pid
      (emit_args_o_ pp_user_data_)
      [ "name", `String name ];
    Buffer.output_buffer self.oc self.buf

  let emit_counter ~name ~tid ~ts (self : t) f : unit =
    emit_sep_and_start_ self;
    Printf.bprintf self.buf
      {json|{"pid":%d,"tid":%d,"ts":%.2f,"name":"c","ph":"C"%a}|json} self.pid
      tid ts
      (emit_args_o_ pp_user_data_)
      [ name, `Float f ];
    Buffer.output_buffer self.oc self.buf
end

(** Background thread, takes events from the queue, puts them
    in context using local state, and writes fully resolved
    TEF events to [out]. *)
let bg_thread ~mode ~out (events : Event.t B_queue.t) : unit =
  (* open a writer to [out] *)
  Writer.with_ ~mode ~out @@ fun writer ->
  (* local state, to keep track of span information and implicit stack context *)
  let spans : span_info Span_tbl.t = Span_tbl.create 32 in

  (* add function name, if provided, to the metadata *)
  let add_fun_name_ fun_name data : _ list =
    match fun_name with
    | None -> data
    | Some f -> ("function", `String f) :: data
  in

  (* how to deal with an event *)
  let handle_ev (ev : Event.t) : unit =
    match ev with
    | E_tick -> Writer.flush writer
    | E_message { tid; msg; time_us; data } ->
      Writer.emit_instant_event ~tid ~name:msg ~ts:time_us ~args:data writer
    | E_define_span { tid; name; id; time_us; fun_name; data } ->
      let data = add_fun_name_ fun_name data in
      let info = { tid; name; start_us = time_us; data } in
      (* save the span so we find it at exit *)
      Span_tbl.add spans id info
    | E_exit_span { id; time_us = stop_us } ->
      (match Span_tbl.find_opt spans id with
      | None -> !on_tracing_error (Printf.sprintf "cannot find span %Ld" id)
      | Some { tid; name; start_us; data } ->
        Span_tbl.remove spans id;
        Writer.emit_duration_event ~tid ~name ~start:start_us ~end_:stop_us
          ~args:data writer)
    | E_add_data { id; data } ->
      (match Span_tbl.find_opt spans id with
      | None -> !on_tracing_error (Printf.sprintf "cannot find span %Ld" id)
      | Some info -> info.data <- List.rev_append data info.data)
    | E_enter_manual_span { tid; time_us; name; id; data; fun_name; flavor } ->
      let data = add_fun_name_ fun_name data in
      Writer.emit_manual_begin ~tid ~name ~id ~ts:time_us ~args:data ~flavor
        writer
    | E_exit_manual_span { tid; time_us; name; id; flavor; data } ->
      Writer.emit_manual_end ~tid ~name ~id ~ts:time_us ~flavor ~args:data
        writer
    | E_counter { tid; name; time_us; n } ->
      Writer.emit_counter ~name ~tid ~ts:time_us writer n
    | E_name_process { name } -> Writer.emit_name_process ~name writer
    | E_name_thread { tid; name } -> Writer.emit_name_thread ~tid ~name writer
  in

  try
    while true do
      (* get all the events in the incoming blocking queue, in
         one single critical section. *)
      let local = B_queue.pop_all events in
      List.iter handle_ev local
    done
  with B_queue.Closed ->
    (* write a message about us closing *)
    Writer.emit_instant_event ~name:"tef-worker.exit"
      ~tid:(Thread.id @@ Thread.self ())
      ~ts:(now_us ()) ~args:[] writer;

    (* warn if app didn't close all spans *)
    if Span_tbl.length spans > 0 then
      Printf.eprintf "trace-tef: warning: %d spans were not closed\n%!"
        (Span_tbl.length spans);
    ()

(** Thread that simply regularly "ticks", sending events to
    the background thread so it has a chance to write to the file *)
let tick_thread events : unit =
  try
    while true do
      Thread.delay 0.5;
      B_queue.push events E_tick
    done
  with B_queue.Closed -> ()

type output =
  [ `Stdout
  | `Stderr
  | `File of string
  ]

let collector_ ~(finally : unit -> unit) ~(mode : [ `Single | `Jsonl ]) ~out ()
    : collector =
  let module M = struct
    let active = A.make true

    (** generator for span ids *)
    let span_id_gen_ = A.make 0

    (* queue of messages to write *)
    let events : Event.t B_queue.t = B_queue.create ()

    (** writer thread. It receives events and writes them to [oc]. *)
    let t_write : Thread.t =
      Thread.create
        (fun () ->
          let@ () = Fun.protect ~finally in
          bg_thread ~mode ~out events)
        ()

    (** ticker thread, regularly sends a message to the writer thread.
         no need to join it. *)
    let _t_tick : Thread.t = Thread.create (fun () -> tick_thread events) ()

    let shutdown () =
      if A.exchange active false then (
        B_queue.close events;
        (* wait for writer thread to be done. The writer thread will exit
           after processing remaining events because the queue is now closed *)
        Thread.join t_write
      )

    let get_tid_ () : int =
      if !Mock_.enabled then
        3
      else
        Thread.id (Thread.self ())

    let[@inline] enter_span_ ~fun_name ~data name : span =
      let span = Int64.of_int (A.fetch_and_add span_id_gen_ 1) in
      let tid = get_tid_ () in
      let time_us = now_us () in
      B_queue.push events
        (E_define_span { tid; name; time_us; id = span; fun_name; data });
      span

    let enter_span ~__FUNCTION__:fun_name ~__FILE__:_ ~__LINE__:_ ~data name :
        span =
      enter_span_ ~fun_name ~data name

    let exit_span span : unit =
      let time_us = now_us () in
      B_queue.push events (E_exit_span { id = span; time_us })

    (* re-raise exception with its backtrace *)
    external reraise : exn -> 'a = "%reraise"

    let with_span ~__FUNCTION__:fun_name ~__FILE__:_ ~__LINE__:_ ~data name f =
      let span = enter_span_ ~fun_name ~data name in
      try
        let x = f span in
        exit_span span;
        x
      with exn ->
        exit_span span;
        reraise exn

    let add_data_to_span span data =
      if data <> [] then B_queue.push events (E_add_data { id = span; data })

    let enter_manual_span ~(parent : explicit_span option) ~flavor
        ~__FUNCTION__:fun_name ~__FILE__:_ ~__LINE__:_ ~data name :
        explicit_span =
      (* get the id, or make a new one *)
      let id =
        match parent with
        | Some m -> Meta_map.find_exn key_async_id m.meta
        | None -> A.fetch_and_add span_id_gen_ 1
      in
      let time_us = now_us () in
      B_queue.push events
        (E_enter_manual_span
           { id; time_us; tid = get_tid_ (); data; name; fun_name; flavor });
      {
        span = 0L;
        meta =
          Meta_map.(
            empty |> add key_async_id id |> add key_async_data (name, flavor));
      }

    let exit_manual_span (es : explicit_span) : unit =
      let id = Meta_map.find_exn key_async_id es.meta in
      let name, flavor = Meta_map.find_exn key_async_data es.meta in
      let data =
        match Meta_map.find key_data es.meta with
        | None -> []
        | Some r -> !r
      in
      let time_us = now_us () in
      let tid = get_tid_ () in
      B_queue.push events
        (E_exit_manual_span { tid; id; name; time_us; data; flavor })

    let add_data_to_manual_span (es : explicit_span) data =
      if data <> [] then (
        let data_ref, add =
          match Meta_map.find key_data es.meta with
          | Some r -> r, false
          | None -> ref [], true
        in
        let new_data = List.rev_append data !data_ref in
        data_ref := new_data;
        if add then es.meta <- Meta_map.add key_data data_ref es.meta
      )

    let message ?span:_ ~data msg : unit =
      let time_us = now_us () in
      let tid = get_tid_ () in
      B_queue.push events (E_message { tid; time_us; msg; data })

    let counter_float ~data:_ name f =
      let time_us = now_us () in
      let tid = get_tid_ () in
      B_queue.push events (E_counter { name; n = f; time_us; tid })

    let counter_int ~data name i = counter_float ~data name (float_of_int i)
    let name_process name : unit = B_queue.push events (E_name_process { name })

    let name_thread name : unit =
      let tid = get_tid_ () in
      B_queue.push events (E_name_thread { tid; name })
  end in
  (module M)

let[@inline] collector ~out () : collector =
  collector_ ~finally:ignore ~mode:`Single ~out ()

let setup ?(out = `Env) () =
  match out with
  | `Stderr -> Trace_core.setup_collector @@ collector ~out:`Stderr ()
  | `Stdout -> Trace_core.setup_collector @@ collector ~out:`Stdout ()
  | `File path -> Trace_core.setup_collector @@ collector ~out:(`File path) ()
  | `Env ->
    (match Sys.getenv_opt "TRACE" with
    | Some ("1" | "true") ->
      let path = "trace.json" in
      let c = collector ~out:(`File path) () in
      Trace_core.setup_collector c
    | Some "stdout" -> Trace_core.setup_collector @@ collector ~out:`Stdout ()
    | Some "stderr" -> Trace_core.setup_collector @@ collector ~out:`Stderr ()
    | Some path ->
      let c = collector ~out:(`File path) () in
      Trace_core.setup_collector c
    | None -> ())

let with_setup ?out () f =
  setup ?out ();
  Fun.protect ~finally:Trace_core.shutdown f

module Internal_ = struct
  let mock_all_ () = Mock_.enabled := true
  let on_tracing_error = on_tracing_error

  let collector_jsonl ~finally ~out () : collector =
    collector_ ~finally ~mode:`Jsonl ~out ()

  module Event = Event
end
