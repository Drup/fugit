module P = CalendarLib.Printer.Precise_Calendar
module C = CalendarLib.Calendar.Precise

type t = {
  start : C.t option ;
  length : C.Period.t option ;
}

let pp_calendar ppf c =
  P.fprint "%c" ppf c
let pp_period ppf p =
  let years, months, days, seconds = C.Period.ymds p in
  let hours, minutes, seconds =
    let m = seconds mod 60 in
    let h = m mod 24 in
    h, m - 24*h, seconds - m*60
  in
  let condfmt ppf (i,fmt,fmts) =
    if i > 1 then Fmt.pf ppf fmts i
    else if i = 1 then Fmt.pf ppf fmt
    else ()
  in
  Fmt.(pf ppf "@[%a@]." (list ~sep:comma condfmt))
    [ years, "a year", "%i years";
      months, "a month", "%i months";
      days, "a day", "%i days";
      hours, "an hour", "%i hours";
      minutes, "a min", "%i mins";
      seconds, "a sec", "%i secs";
    ]

let pp_explain ppf { start ; length } =
  Fmt.pf ppf "It is <b>%a</b>.\n" pp_calendar (C.now ()) ;
  begin match start, length with
    | None, None -> ()
    | Some start, None ->
      Fmt.pf ppf "Alert started at <b>%a</b>." pp_calendar start
    | Some start, Some length ->
      Fmt.pf ppf "Alert was started at <b>%a</b> and programmed to trigger after <b>%a<:/b>." pp_calendar start pp_period length
    | None, Some length ->
      Fmt.pf ppf "Alert has been running for <b>%a</b>." pp_period length
  end 

module Arg = struct
  open Cmdliner.Arg
    
  let calendar =
    let docv = "TIME" in
    let parser s =
      try Ok (P.from_string s) with Invalid_argument s -> Error (`Msg s)
    in
    conv ~docv (parser, pp_calendar)

  let start_time =
    let i =
      info ~docv:"TIME"
        ~doc:"Date on which the alert was started."
        ["start"]
    in
    value & opt (some calendar) None i
end

let term =
  let f start = { start ; length = None } in
  Cmdliner.Term.(pure f $ Arg.start_time)

