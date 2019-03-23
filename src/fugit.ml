open Lwt.Infix
open Cmdliner

(* systemd-run --user --no-ask-password --on-active=1m --timer-property=AccuracySec=1ms -- notify-send -u critical -t 0 "ploup"
*)

let verbose =
  let i = Arg.info ~doc:"Enable verbose messages." ["v";"verbose"] in
  Arg.(value & flag i)

type options = {
  verbose : bool ;
}

let common_opts =
  let f verbose = {
    verbose ;
  }
  in
  Term.(pure f $ verbose)

let message =
  let i = Arg.info ~docv:"MSG" ~doc:"Description of the alert." [] in
  Arg.(required & pos 0 (some string) None i)

let lwt_ret f =
  Term.(ret (pure Lwt_main.run $ f))

let alert_command =
  let i =
    Term.info ~doc:"Launch an alert later." "alert"
  in
  let f opts delay s =
    begin
      if opts.verbose then
        Fmt.pr "Delay: %a@." Delay.Raw.pp delay
    end;
    let n = Notif.make delay s in
    Delay_sleep.launch n >|= fun () ->
    `Ok ()
  in
  let cmd =
    Term.(lwt_ret (pure f $ common_opts $ Delay.Cmd.term 0 $ message))
  in
  cmd, i

let ding_command =
  let i =
    Term.info ~doc:"Launch an alert immediatly." "ding"
  in
  let f delay s =
    let n = Notif.make delay s in
    Notif.notif n >|= fun _ ->
    `Ok ()
  in
  let cmd =
    Term.(lwt_ret (pure f $ Delay.Raw.term $ message))
  in
  cmd, i

let default =
  let i =
    Term.info "fugit"
  in
  let cmd =
    Term.(ret (const (fun _ -> `Help (`Pager, None)) $ common_opts))
  in
  cmd, i

let cmds = [ ding_command; alert_command ]

let () =
  CalendarLib.Time_Zone.change UTC;
  match Term.eval_choice default cmds with
  | r -> Term.exit r
