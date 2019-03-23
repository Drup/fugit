module C = CalendarLib.Calendar.Precise

let calendar_pp_with fmt ppf d =
  let open CalendarLib in
  let d = C.convert d UTC Local in
  Printer.Precise_Calendar.fprint fmt ppf d

let calendar_parse_with fmt s = 
  let open CalendarLib in
  C.convert
    (Printer.Precise_Calendar.from_fstring fmt s)
    Local UTC


type t = {
  start : C.t ;
  stop : C.t ;
}


type precision =
  | Hour
  | Day
  | Yesterday
  | Week
  | Month
  | Year
  | Unknown

let decide_precision c =
  let c = C.convert c UTC Local in
  let c_now = C.convert (C.now ()) UTC Local in
  let date = C.to_date c and date_now = C.to_date c_now in
  if C.Date.equal date date_now then
    begin if C.hour c = C.hour c_now then
        Hour
      else
        Day
    end
  else if C.(day_of_year c_now - day_of_year c) = 1 then Yesterday
  else if C.(week c_now = week c) then Week
  else if C.(month c_now = month c) then Month
  else if C.(year c_now = year c) then Year
  else
    Unknown

let format_start = function
  | Hour -> "at <b>%T</b>"
  | Day -> "at <b>%T</b>"
  | Yesterday
    -> "<b>yesterday at <b>%R</b>"
  | Week
    -> "<b>%A</b> at <b>%R</b>"
  | Month
    -> "the <b>%dth</b> at <b>%R</b>"
  | Year
    -> "the <b>%dth</b> <b>%B</b> at <b>%R</b>"
  | Unknown
    -> "the <b>%dth</b> <b>%B</b> <b>%Y</b> at <b>%R</b>"

let format_now = function
  | Hour
  | Day
    -> "<b>%T</b>"
  | Yesterday
  | Week
    -> "<b>%A</b> at <b>%R</b>"
  | Month
    -> "<b>%A</b> <b>%dth</b> at <b>%R</b>"
  | Year
    -> "<b>%A</b> <b>%dth</b> <b>%B</b> at <b>%R</b>"
  | Unknown
    -> "<b>%A</b> <b>%dth</b> <b>%B</b> <b>%Y</b> at <b>%R</b>"

let pp_period ppf p =
  let years, months, days, seconds = C.Period.ymds p in
  let hours, minutes, seconds =
    let m = seconds / 60 in
    let h = m / 24 in
    h, m - 24*h, seconds - m*60
  in
  let condfmt ppf (i,fmt,fmts) =
    if i > 1 then Fmt.pf ppf fmts i
    else if i = 1 then Fmt.pf ppf fmt
    else ()
  in
  let rec pp_list ppf l = match l with
    | [] -> ()
    | [b] ->
      Fmt.pf ppf "%a" condfmt b
    | (0, _, _) :: l -> pp_list ppf l
    | b :: l ->
      Fmt.pf ppf "%a, %a" condfmt b pp_list l
  in
  Fmt.(pf ppf "@[%a@]" pp_list)
    [ years, "1 year", "%i years";
      months, "1 month", "%i months";
      days, "1 day", "%i days";
      hours, "1 hour", "%i hours";
      minutes, "1 minute", "%i minutes";
      seconds, "1 second", "%i seconds";
    ]

let pp_explain ppf { start ; stop } =
  let precision = decide_precision start in
  let length = C.sub stop start in
  Fmt.pf ppf "It is %a.\n"
    (calendar_pp_with @@ format_now precision)
    (C.now ()) ;
  if precision = Hour then
    Fmt.pf ppf "Alert started %a ago."
      pp_period length
  else
    Fmt.pf ppf "Alert started %a.\n%a ago."
      (calendar_pp_with @@ format_start precision) start
      pp_period length

let parse s =
  `Ok (C.Period.second @@ int_of_string s)

module Arg = struct
  open Cmdliner
    
  let rfc3339 =
    let docv = "TIME" in
    let format = "%FT%T%:z" in
    let printer = calendar_pp_with format in
    let parser s =
      try Ok (calendar_parse_with format s) with
        Invalid_argument s -> Error (`Msg s)
    in
    Arg.conv ~docv (parser, printer)
  
  let delay n =
    let i =
      Arg.info ~docv:"DELAY"
        ~doc:"Delay before the alert."
        []
    in
    let a = Arg.(non_empty & pos_right n string [] i) in
    let f l = parse @@ String.concat " " l in
    Term.(ret (pure f $ a))
  
  let delay_raw =
    let i =
      Arg.info ~docv:"DELAY"
        ~doc:"Provide the start and end time as pair of rfc3339 dates."
        ["raw-delay"]
    in
    Arg.(required & opt (some & pair rfc3339 rfc3339) None i)
end

let term_raw =
  let f (start, stop) = { start ; stop } in
  Cmdliner.Term.(pure f $ Arg.delay_raw)

let term_duration n =
  let f dur =
    let start = C.now () in
    let stop = C.add start dur in
    { start ; stop }
  in
  Cmdliner.Term.(pure f $ Arg.delay n)
