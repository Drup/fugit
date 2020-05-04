open Lwt.Infix
module P = CalendarLib.Calendar.Precise

let wait ~no_action ~verbose (n : Notif.t) =
  let duration = Delay.duration n.delay in
  let s = P.Time.Period.to_seconds @@ P.Period.safe_to_time duration in
  if verbose then Fmt.pr "Sleeping for %i secondes.@." s;
  if no_action then Lwt.return_ok ()
  else begin
    Lwt_unix.sleep (float s) >>= fun () ->
    Notif.notif n >>= fun _ ->
    Lwt.return_ok ()
  end

let launch ~no_action ~verbose n =
  match Lwt_unix.fork () with
  | 0 ->
    if Lwt_unix.fork () = 0 then
      wait ~no_action ~verbose n
    else
      Lwt.return_ok ()
  | _ -> Lwt.return_ok ()
