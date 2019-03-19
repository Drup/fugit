open Lwt.Infix

(* systemd-run --user --no-ask-password --on-active=1m --timer-property=AccuracySec=1ms -- notify-send -u critical -t 0 "ploup"
*)

open Cmdliner

let common_opts = Term.(pure ())

let message =
  let i = Arg.info ~docv:"MSG" ~doc:"Description of the alert." [] in
  Arg.(required & pos 0 (some string) None i)

let lwt_ret f =
  Term.(ret (pure Lwt_main.run $ f))

let ding_command =
  let i =
    Term.info ~doc:"Launch an alert immediatly." "ding"
  in 
  let f orig s =
    let n = Notif.make orig s in
    Notif.notif n >>= fun _ ->
    Lwt.return (`Ok ())
  in
  let cmd =
    Term.(lwt_ret (pure f $ Delay.term $ message))
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

let cmds = [ ding_command ]

let () =
  match Term.eval_choice default cmds with
  | r -> Term.exit r
