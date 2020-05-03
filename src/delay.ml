module C = CalendarLib.Calendar.Precise

type duration = C.Period.t

type t = {
  start : C.t ;
  stop : C.t ;
}

let of_duration p =
  let start = C.now () in
  let stop = C.add start p in
  { start ; stop }

let duration p = C.sub p.stop p.start

let calendar_pp_with fmt ppf d =
  let open CalendarLib in
  let d = C.convert d UTC Local in
  Printer.Precise_Calendar.fprint fmt ppf d

let calendar_parse_with fmt s =
  let open CalendarLib in
  C.convert
    (Printer.Precise_Calendar.from_fstring fmt s)
    Local UTC

module Raw = struct
  open Cmdliner

  let format = "%FT%T%:z"
  let printer = calendar_pp_with format
  let parser s =
    try Ok (calendar_parse_with format s) with
      Invalid_argument s -> Error (`Msg s)

  let rfc3339 =
    let docv = "TIME" in
    let parser s =
      try Ok (calendar_parse_with format s) with
        Invalid_argument s -> Error (`Msg s)
    in
    Arg.conv ~docv (parser, printer)

  let arg =
    let i =
      Arg.info ~docs:"DELAY" ~docv:"START/STOP"
        ~doc:"Provide the start and end time as pair of rfc3339 dates separated by /."
        ["raw-delay"]
    in
    Arg.(value & opt (some & pair ~sep:'/' rfc3339 rfc3339) None i)

  let term =
    let f (start, stop) = { start ; stop } in
    Cmdliner.Term.(pure (CCOpt.map f) $ arg)

  let pp ppf { start ; stop } =
    Fmt.pf ppf "%a/%a" printer start printer stop
end


(** Printing *)
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

let pp_duration ppf p =
  let years, months, days, seconds = C.Period.ymds p in
  let hours, minutes, seconds =
    let m = seconds / 60 in
    let h = m / 24 in
    h, m - 24*h, seconds - m*60
  in
  let condfmt ppf (i,fmt,fmts) =
    if i > 1 then Fmt.pf ppf fmts i
    else if i = 1 then Fmt.pf ppf fmt
    else assert false
  in
  let pp_list ppf l =
    let l = CCList.filter (fun (i,_,_) -> i <> 0) l in
    match List.rev l with
    | [] -> ()
    | [b] -> Fmt.pf ppf "%a" condfmt b
    | b :: l ->
      Fmt.pf ppf "%a and %a"
        Fmt.(list ~sep:comma condfmt) (List.rev l)
        condfmt b
  in
  Fmt.(pf ppf "@[%a@]" pp_list)
    [ years, "1 year", "%i years";
      months, "1 month", "%i months";
      days, "1 day", "%i days";
      hours, "1 hour", "%i hours";
      minutes, "1 minute", "%i minutes";
      seconds, "1 second", "%i seconds";
    ]

let pp_explain ppf d =
  let precision = decide_precision d.start in
  let duration = duration d in
  Fmt.pf ppf "It is %a.\n"
    (calendar_pp_with @@ format_now precision)
    (C.now ()) ;
  if precision = Hour then
    Fmt.pf ppf "Alert started %a ago."
      pp_duration duration
  else
    Fmt.pf ppf "Alert started %a.\n%a ago."
      (calendar_pp_with @@ format_start precision) d.start
      pp_duration duration

(** Parsing *)

module Parsing = struct
  open Angstrom

  let char_ci i =
    let x = CCChar.lowercase_ascii i and y = CCChar.uppercase_ascii i in
    satisfy (fun c -> c = x || c = y)

  let digits =
    let f i =
      match int_of_string i with
      | v -> return v
      | exception _ -> fail @@ Fmt.strf "Not a valid integer: %s" i
    in
    (take_while1 CCParse.is_num >>= f) <?> "Integer"

  let iso8601_duration =
    let elem c =
      option None (digits <* char_ci c >>| fun x -> Some x)
    in
    let date =
      let f year month day (hour, minute, second) =
        C.Period.lmake ?year ?month ?day ?hour ?minute ?second () in
      lift3 f (elem 'Y') (elem 'M') (elem 'D')
    in
    let time =
      lift3 (fun a b c -> a,b,c) (elem 'H') (elem 'M') (elem 'S')
    in
    char_ci 'P' *>
    date <*>
    option (None,None,None) (char_ci 'T' *> time)


  let white = skip_while CCParse.is_white
  let skip =
    white *> (
      string_ci "and" <* white <|>
      string_ci "," <* white <|>
      return ""
    ) 

  let ( <** ) a b = a <* skip *> b
  let ( **> ) a b = a *> skip *> b
  let ( <**> ) a b = a <*> (skip *> b)
  
  let human_duration =
    let unit f l =
      let l = List.sort CCOrd.(opp string) l in 
      choice (List.map string_ci l) *> return f
    in
    let units = [
      unit C.Period.year ["y";"year";"years"];
      unit C.Period.month ["month";"months"];
      unit C.Period.day ["d";"day";"days"];
      unit C.Period.hour ["h";"hour";"hours"];
      unit C.Period.minute ["m";"min";"mins";"minute";"minutes"];
      unit C.Period.second ["s";"sec";"secs";"second";"seconds"];
    ]
    in 
    let elem =
      lift2 (|>)
        (digits <* white)
        (choice ~failure_msg:"Unknown time unit" units)
    in
    let elems =
      sep_by1 skip elem
    in
    List.fold_left C.Period.add C.Period.empty <$> elems

  let duration =
    (iso8601_duration <?> "invalid ISO8601 duration")
    <|> (human_duration <?> "invalid duration")

  let parser =
    (string_ci "in" *> skip_many1 white *> commit *> duration) <|>
    duration

  let interp_error x = 
    let map_error s =
      Fmt.strf "Unrecognized duration: %s" s
    in
    CCResult.map_err map_error x

  let parse_duration s =
    Angstrom.parse_string ~consume:Consume.All (parser <** end_of_input) s
    
end

let parse_duration s =
  Parsing.(interp_error @@ parse_duration s)

let parse_durationl l =
  let s = String.concat " " l in
  parse_duration s

let parse l = 
  let s = String.concat " " l in
  Parsing.(interp_error @@ CCResult.map of_duration @@ parse_duration s)

