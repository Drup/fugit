open Lwt.Infix
module P = CalendarLib.Calendar.Precise

let wait (n : Notif.t) =
  let length = P.sub n.orig.stop n.orig.start in
  let s = P.Time.Period.to_seconds @@ P.Period.to_time length in
  Fmt.pr "Alert will trigger in %a.@." Delay.pp_period length;
  Lwt_unix.sleep (float s) >>= fun () ->
  Notif.notif n >>= fun _ ->
  Lwt.return_unit

let launch n =
  match Lwt_unix.fork () with
  | 0 ->
    if Lwt_unix.fork () = 0 then Lwt_main.run @@ wait n ;
    exit 0
  | _ -> Lwt.return_unit
