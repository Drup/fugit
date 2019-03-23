module C = CalendarLib.Calendar.Precise

type t = {
  start : C.t ;
  stop : C.t ;
}


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
      Arg.info ~docv:"START/STOP"
        ~doc:"Provide the start and end time as pair of rfc3339 dates separated by /."
        ["raw-delay"]
    in
    Arg.(required & opt (some & pair ~sep:'/' rfc3339 rfc3339) None i)

  let term =
    let f (start, stop) = { start ; stop } in
    Cmdliner.Term.(pure f $ arg)

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

(** Parsing *)

module Parsing = struct
  open Angstrom

  let white = satisfy CCParse.is_white

  let ( <** ) a b = a <* skip_while CCParse.is_white *> b
  let ( **> ) a b = a *> skip_while CCParse.is_white *> b
  let ( <**> ) a b = a <*> (skip_while CCParse.is_white *> b)

  let char_ci i =
    let x = CCChar.lowercase_ascii i and y = CCChar.uppercase_ascii i in
    satisfy (fun c -> c = x || x = y)

  let digits n =
    assert (n < Sys.word_size) ;
    let i = ref 0 in
    let is_digit = function '0'..'9' -> true | _ -> false in
    let f c = incr i ; !i < n && is_digit c in
    int_of_string <$> take_while f

  let iso8601_duration =
    let elem n c =
      option None (digits n <** char_ci c >>| fun x -> Some x)
    in
    let date =
      let f year month day (hour, minute, second) =
        C.Period.lmake ?year ?month ?day ?hour ?minute ?second () in
      lift3 f (elem 4 'Y') (elem 2 'M') (elem 2 'D')
    in
    let time =
      lift3 (fun a b c -> a,b,c) (elem 4 'H') (elem 2 'M') (elem 2 'S')
    in
    char_ci 'P' **>
    date <**>
    option (None,None,None) (char_ci 'T' **> time)

  let human_duration =
    let elem n l =
      let l = List.sort CCOrd.(opp string) l in
      option None
        (digits n <** choice (List.map string_ci l) >>| fun x -> Some x)
    in
    let f year month day hour minute second =
      C.Period.lmake ?year ?month ?day ?hour ?minute ?second ()
    in
    lift4 f
      (elem 4 ["y";"year";"years"])
      (elem 2 ["month";"months"])
      (elem 2 ["d";"day";"days"])
      (elem 4 ["h";"hour";"hours"])
      <**> (elem 2 ["m";"min";"mins";"minute";"minutes"])
      <**> (elem 2 ["s";"sec";"secs";"second";"seconds"])

  let duration =
    (iso8601_duration <?> "invalid ISO8601 duration")
    <|> (human_duration <?> "invalid duration")

  let parser =
    (string_ci "in" *> skip_many1 white *> commit *> duration) <|>
    duration

  let go l =
    let s = String.concat " " l in
    match Angstrom.parse_string (parser <** end_of_input) s with
    | Ok x -> `Ok x
    | Error s ->
      let s = Fmt.strf "Unrecognized duration: %s" s in
      `Error (false, s)
end


module Cmd = struct
  open Cmdliner

  let arg n =
    let i =
      Arg.info ~docv:"DELAY"
        ~doc:"Delay before the alert."
        []
    in
    let a = Arg.(non_empty & pos_right n string [] i) in
    Term.(ret (pure Parsing.go $ a))

  let term n =
    let f dur =
      let start = C.now () in
      let stop = C.add start dur in
      { start ; stop }
    in
    Cmdliner.Term.(pure f $ arg n)
end
