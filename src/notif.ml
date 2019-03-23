module ONotif = Notification

type t = {
  icon : string ;
  message : string ;
  timeout : int ;
  orig : Delay.t ;
}

let make ?(icon="appointment-soon") ?(timeout=0) orig message =
  { icon ; timeout ; message ; orig}

let notif { icon ; message ; timeout ; orig } =
  let body = Fmt.strf "%a" Delay.pp_explain orig in
  ONotif.notify
    ~app_name:"fugit"
    ~icon
    ~summary:message
    ~body
    ~timeout
    ()
