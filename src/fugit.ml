open Lwt_result.Infix
open Cmdliner

(* systemd-run --user --no-ask-password --on-active=1m --timer-property=AccuracySec=1ms -- notify-send -u critical -t 0 "ploup"
*)

(** Utility functions *)

let to_ret ?(usage=false) =
  Lwt.map @@ function
  | Ok x -> `Ok x
  | Error s -> `Error (usage, s)

let lwt_ret f =
  Term.(ret (pure Lwt_main.run $ f))


(** Common options *)

type options = {
  verbose : bool ;
  raw_delay : Delay.t option ;
}

let verbose =
  let i = Arg.info ~doc:"Enable verbose messages." ["v";"verbose"] in
  Arg.(value & flag i)

let common_opts =
  let f verbose raw_delay = {
    verbose ;
    raw_delay ;
  }
  in
  Term.(pure f $ verbose $ Delay.Raw.term)

let message =
  let i = Arg.info ~docv:"MSG" ~doc:"Description of the alert." [] in
  Arg.(required & pos 0 (some string) None i)


(** Commands *)

let parse_delay opts l =
  match opts.raw_delay, l with
  | None, [] ->
    Lwt_result.fail "No delay was provided"
  | Some _, _ :: _ ->
    Lwt_result.fail "Either a delay or a raw delay should be provided"
  | Some d, [] ->
    Lwt_result.return d
  | None, l ->
    Lwt_result.lift (Delay.parse l) >|= Delay.of_duration

let alert_command opts delay message =
  begin
    if opts.verbose then
      Fmt.pr
        "@[<2>Launching the alert: %s@]@.Delay: %a@."
        message
        Delay.Raw.pp delay
  end;
  let n = Notif.make delay message in
  Lwt_result.ok @@ Delay_sleep.launch n

let ding_command _opts delay message =
  let n = Notif.make delay message in
  Lwt_result.ok (Notif.notif n) >|=
  ignore

(** Actions *)
  
type action =
  | Notif
  | Ding
  | Other of string

let action_arg = 
  let sl = [
    "notif", Notif ;
    "ding", Ding ;
  ]
  in
  let e = Arg.enum sl in
  let parse s =
    match Arg.conv_parser e s with
    | Ok _ as x -> x
    | _ -> Ok (Other s)
  in
  let action = Arg.conv (parse, Arg.conv_printer e)  in
  Arg.(required & pos 0 (some action) None & info [])

let main opts action args =
  match action, args with
  | Notif, m :: l
  | Other m, l ->
    let u =
      parse_delay opts l >>= fun delay ->
      alert_command opts delay m
    in
    to_ret u
  | Ding, m :: l ->
    let u =
      parse_delay opts l >>= fun delay ->
      ding_command opts delay m in
    to_ret u
  | _ ->
    Lwt.return @@ `Error (true, "Invalid command")

let term =
  let doc = "Launch notifications at a specified time" in
  let man_xrefs = [ `Main ] in
  let man = [
    `S Manpage.s_synopsis;
    `P "$(mname) $(i,MSG) [$(i,DELAY)]...";
    `Noblank;
    `P "$(mname) $(i,ACTION) $(i,MSG) [$(i,DELAY)]...";
    `S Manpage.s_description;
    `P "The $(mname) command launch a notification with the message $(i,MSG) \
        after a specified $(i,DELAY). \
        See the available actions below.";
    `S "ACTIONS";
    `P {|If no action is provided, $(b,notif) is used by default.|};
    `I ("$(b,notif)", "Launch an alert later.");
    `I ("$(b,ding)", "Launch an alert now.");
    `S "DELAY";
    `P {|A delay can be specified either with the option $(b,--raw-delay) or
using the $(i,DELAY) argument.|};
    `I ("$(i,DELAY)",{|Can be expressed in natural language such as 
"in 5 minutes 2 seconds".
It can also be expressed using ISO8601 duration: "D2DT1H".|});
  ]
  in
  let rest_arg = Arg.(value & pos_right 0 string [] & info [] ~docv:"DELAY") in
  Term.(lwt_ret (pure main $ common_opts $ action_arg $ rest_arg)),
  Term.info "fugit" ~doc ~man ~man_xrefs
  
let () =
  CalendarLib.Time_Zone.change UTC;
  match Term.eval term with
  | r -> Term.exit r
