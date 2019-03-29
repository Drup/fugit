module ONotif = Notification

type icon = string

type t = {
  icon : icon ;
  message : string ;
  timeout : int ;
  delay : Delay.t ;
}

let make ?(icon="appointment-soon") ?(timeout=0) delay message =
  { icon ; timeout ; message ; delay}

let notif { icon ; message ; timeout ; delay } =
  let body = Fmt.strf "%a" Delay.pp_explain delay in
  ONotif.notify
    ~app_name:"fugit"
    ~icon
    ~summary:message
    ~body
    ~timeout
    ()
