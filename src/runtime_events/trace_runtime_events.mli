val with_setup : unit -> (unit -> 'a) -> 'a
(** [with_setup f] (optionally) sets a collector up, calls [f()],
    and makes sure to shutdown before exiting.
*)
