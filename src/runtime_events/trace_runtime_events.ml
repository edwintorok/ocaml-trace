open Trace_core 
open Runtime_events

module Collector = struct

  let string =
    let encode dest str = 
      let n = String.length str in
      Bytes.blit_string str 0 dest 0 n;
      n
    and decode src length =
      Bytes.sub_string src 0 length
    in
    Type.register ~encode ~decode

  type value = 
      [`Int of int | `String of string | `Bool of bool | `Float of float | `None]

  type span =
  { span: int
  ; name: string
  ;  __FUNCTION__: string option
  ; __FILE__: string
  ; __LINE__: int
  ; data: (string * value) list
  }

  let[@inline always] encode_string dest str pos =
    let n = String.length str in
    Bytes.set_uint16_ne dest pos n;
    let pos = pos + 2 in
    Bytes.blit_string str 0 dest pos n;
    pos + n

  let[@inline always] write_kind dest pos k =
    Bytes.set_int8 dest pos k;
    pos + 1

  let[@inline always] encode_int64 dest i64 pos =
    Bytes.set_int64_ne dest pos i64;
    pos + 8

  let[@inline always] encode_float dest f pos =
    encode_int64 dest (Int64.bits_of_float f) pos

  let[@inline always] encode_int dest i pos =
    encode_int64 dest (Int64.of_int i) pos

  let[@inline always] encode_value dest value pos =
    match value with
    | `None ->
      write_kind dest pos 0
    | `Int i ->
      let pos = write_kind dest pos 1 in
      encode_int dest i pos
    | `String str ->
      let pos = write_kind dest pos 2 in
      encode_string dest str pos
    | `Bool b ->
      let pos = write_kind dest pos 3 in
      Bytes.set_uint8 dest pos (Bool.to_int b);
      pos + 1
    | `Float f ->
      let pos = write_kind dest pos 4 in
      encode_float dest f pos

  let[@inline always] rec encode_data dest lst pos =
    match lst with
    | [] -> write_kind dest pos (-1)
    | (key, value) :: tl ->
      pos
      |> encode_string dest key
      |> encode_value dest value
      |> encode_data dest tl

  let span_data =
    let encode dest (span, data) =
      0
      |> encode_int dest (Int64.to_int span)
      |> encode_data dest data
    and decode _ = failwith "TODO"
    in
    Type.register ~encode ~decode

  let message =
    let encode dest (span, data, msg) =
      0
      |> encode_int dest (span |> Option.value ~default:(-1L) |> Int64.to_int)
      |> encode_data dest data
      |> encode_string dest msg
    and decode _ = failwith "TODO"
    in
    Type.register ~encode ~decode

  let counter_int =
    let encode dest (name, value, data) =
      0
      |> encode_string dest name
      |> encode_int dest value
      |> encode_data dest data
    and decode _ = failwith "TODO"
    in
    Type.register ~encode ~decode

  let counter_float =
    let encode dest (name, value, data) =
      0
      |> encode_string dest name
      |> encode_float dest value
      |> encode_data dest data
    and decode _ = failwith "TODO"
    in
    Type.register ~encode ~decode

  let encode_span dest t =
    0
    |> encode_int dest t.span
    |> encode_string dest t.name
    |> encode_string dest (Option.value ~default:"" t.__FUNCTION__)
    |> encode_string dest t.__FILE__
    |> encode_int dest t.__LINE__
    |> encode_data dest t.data

  let decode_span _ _ = failwith "TODO"

  let span =
    Type.register ~encode:encode_span ~decode:decode_span

  let span_id = Type.int

  module Tags = struct
    type User.tag += Name_process

    let name_process = User.register "name_process" Name_process string

    type User.tag += Name_thread
    let name_thread = User.register "name_process" Name_thread string

    type User.tag += Enter_span
    let enter_span = User.register "enter_span" Enter_span span
  
    type User.tag += Exit_span
    let exit_span = User.register "exit_span" Exit_span span_id

    type User.tag += Span_data
    let span_data = User.register "span_data" Span_data span_data

    type User.tag += Message
    let message = User.register "message" Message message
  
    type User.tag += Counter_int
    let counter_int = User.register "counter" Counter_int counter_int
  
    type User.tag += Counter_float
    let counter_float = User.register "counter" Counter_float counter_float
  end

  let name_process name = User.write Tags.name_process name
  
  let name_thread name = User.write Tags.name_thread name

  let enter_manual_span ~parent:_ ~flavor:_ ~__FUNCTION__:_ ~__FILE__:_ ~__LINE__:_ ~data:_ _name =
    (* TODO *)
    { span = 0L
    ; meta = Meta_map.empty
    }

  let span_id = Atomic.make 0

  let enter_span ~__FUNCTION__ ~__FILE__ ~__LINE__ ~data name =
    let span = Atomic.fetch_and_add span_id 1 in
    (* makes a single call, avoids repeated timestamp acquisition calls,
       and various other overhead.
       TODO: test that this is actually faster *)
    User.write Tags.enter_span {span; name; __FUNCTION__; __FILE__; __LINE__; data};
    Int64.of_int span

  let exit_manual_span _span =
    (* TODO *)
    ()

  let exit_span span =
    User.write Tags.exit_span (Int64.to_int span)

  let add_data_to_span span data =
    User.write Tags.span_data (span, data) 

  let add_data_to_manual_span _span _data =
    (* TODO *)
    ()

  let message ?span ~data msg =
    User.write Tags.message (span, data, msg)

  let with_span ~__FUNCTION__ ~__FILE__ ~__LINE__ ~data name f =
    let span = enter_span ~__FUNCTION__ ~__FILE__ ~__LINE__ ~data name in
    try
     let r = f span in
     exit_span span;
     r
    with e ->
      exit_span span;
      raise e

  let counter_int ~data name value =
    User.write Tags.counter_int (name, value, data)

  let counter_float ~data name value =
    User.write Tags.counter_float (name, value, data)

  let shutdown () = ()

end


let setup () =
  Trace_core.setup_collector (module Collector)

let with_setup () f =
  setup ();
  Fun.protect ~finally:Trace_core.shutdown f

