
module Period = struct
  type t = CalendarLib.Calendar.Precise.Period.t
  let pp = Delay.pp_duration
  let equal = CalendarLib.Calendar.Precise.Period.equal
end
let period : Period.t Alcotest.testable = (module Period)

let periods =
  let parse p s = Angstrom.(parse_string (p <* end_of_input)) s in
  let f s d =
    let open Alcotest in
    test_case d `Quick @@ fun () ->
    check (result period string) d
      (parse Delay.Parsing.human_duration s)
      (parse Delay.Parsing.iso8601_duration d)
  in
  "Periods", [
    f "1s" "PT1S";
    f "75min" "PT1H15M";
    f "1 year and 2 mins" "P1YT2M";
    f "2month, 3day 23 min" "P2M3DT23M";
    f "1s and 2mins" "PT2M1S";
    f "3 days, 2h and 1 year" "P1Y3DT2H";
  ]



let () = 
  Alcotest.run "test fugit" [
    periods
  ]
