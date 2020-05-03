open Lwt.Infix
module P = CalendarLib.Calendar.Precise

let wait (n : Notif.t) =
  let duration = Delay.duration n.delay in
  let s = P.Time.Period.to_seconds @@ P.Period.safe_to_time duration in
  Fmt.pr "Alert will trigger in %a.@." Delay.pp_duration duration;
  Lwt_unix.sleep (float s) >>= fun () ->
  Notif.notif n >>= fun _ ->
  Lwt.return_unit

let launch n =
  match Lwt_unix.fork () with
  | 0 ->
    if Lwt_unix.fork () = 0 then Lwt_main.run @@ wait n ;
    exit 0
  | _ -> Lwt.return_unit
