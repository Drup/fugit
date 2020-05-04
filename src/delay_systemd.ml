open Lwt_result.Infix
module P = CalendarLib.Calendar.Precise

let systemd_run_cmd target_time cmd =
  let pp_for_systemd =
    CalendarLib.Printer.Precise_Calendar.fprint "%F %T UTC"
  in
  Bos.Cmd.(
    v "systemd-run"
    % "--user"
    % "--no-ask-password"
    % "--collect"
    % "--timer-property=AccuracySec=1s"
    % "--timer-property=Persistent=true"
    % "--on-calendar"
    % Fmt.strf "%a" pp_for_systemd target_time
    % "--" %% cmd
  )

let mk_fugit_command (n : Notif.t) =
  Lwt_result.lift @@
  CCResult.map_err (fun (`Msg s) -> s) @@
  Bos.OS.Cmd.resolve @@
  Bos.Cmd.(
    v Bos.OS.Arg.exec
    % "ding"
    % n.message
    % Fmt.strf "--raw-delay=%a" Delay.Raw.pp n.delay
  )

let launch ~no_action ~verbose (n : Notif.t) =
  let target = n.delay.stop in
  mk_fugit_command n >>= fun fugit_command ->
  let command = systemd_run_cmd target fugit_command in
  if verbose then Fmt.pr "Calling: %a@." Bos.Cmd.pp command;
  if no_action then Lwt.return_ok ()
  else begin
    let r = Bos.OS.Cmd.(in_null |> run_io command |> out_stdout) in
    match r with
    | Ok ((),_) -> Lwt_result.return ()
    | Error (`Msg s) ->
      Lwt_result.fail (Fmt.strf "Systemd is unhappy: %s" s)
  end
