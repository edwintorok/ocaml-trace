module Trace = Trace_core

let ( let@ ) = ( @@ )

let work ~n () : unit =
  for _i = 1 to n do
    let@ _sp =
      Trace.with_span ~__FILE__ ~__LINE__ "outer" ~data:(fun () ->
          [ "i", `Int _i ])
    in
    for _k = 1 to 10 do
      let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "inner" in
      ()
    done
    (* Thread.delay 1e-6 *)
  done

let main ~n ~j () : unit =
  let domains = Array.init j (fun _ -> Domain.spawn (fun () -> work ~n ())) in
  Array.iter Domain.join domains


module Otel = Opentelemetry

let () =
  Otel.Globals.service_name := __MODULE__;
  Ambient_context.set_storage_provider (Ambient_context_tls.storage ());
  let config = Opentelemetry_client_ocurl.Config.make  () in
  Opentelemetry_client_ocurl.with_setup ~config () @@ fun () ->
  Opentelemetry_trace.setup ();

  let n = ref 10_000 in
  let j = ref 4 in

  let args =
    [
      "-n", Arg.Set_int n, " number of iterations";
      "-j", Arg.Set_int j, " set number of workers";
    ]
    |> Arg.align
  in
  Arg.parse args ignore "bench1";
  let t0 = Unix.gettimeofday () in
  main ~n:!n ~j:!j ();
  let t1 = Unix.gettimeofday () in
  Printf.printf "%fns/span\n" (1e9 *. (t1 -. t0) /. (float !n *. 11.))
