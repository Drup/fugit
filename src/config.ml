
type name = string
module NameTbl = CCHashtbl.Make(CCString)

type record = {
  message : string;
  duration : Delay.duration option;
  icon : Notif.icon option;
}
let mk_record ?duration ?icon message =
  { message ; duration ; icon }
let pp_record ppf { message ; duration ; icon } =
  let once fmt f ppf x = Fmt.pf ppf fmt f x in
  Fmt.pf ppf "@[<v>message: %s@,%a%a@]"
    message
    (Fmt.option @@ once "duration: %a@." Delay.pp_duration) duration
    (Fmt.option @@ once "icon: %a@." Fmt.string) icon

type config = {
  records : record NameTbl.t
}

module Format = struct
  module T = Lenses
  open T

  let message =
    (fun x -> x.message) >$<
    (T.key "message" |-- T.string)
  let duration =
    let create v = Fmt.strf "%a" Delay.pp_duration v in
    let get x = CCResult.to_opt @@ Delay.parse_duration x in
    let set v _ = create v in
    (fun x -> x.duration) >$<
    T.optional "duration" (T.string |-- {T. create ; get ; set })
  let icon =
    (fun x -> x.icon) >$<
    T.optional "icon" T.string

  let record =
    let f icon duration message = mk_record ?icon ?duration message in
    f <$> icon <*> duration <*> message

  let alerts = T.field "alerts"

  let entry r =
    alerts |-- T.field r |-- record

  let get_entry r c =
    try T.get c (entry r) with TomlTypes.Table.Key.Bad_key _ -> None
  let get_all_entry c =
    let tbl =
      CCOpt.get_or ~default:TomlTypes.Table.empty @@
      T.get c @@ T.field "alerts"
    in
    let f (n,c) = TomlTypes.Table.Key.to_string n, T.get c (T.table |-- record) in
    List.map f @@ TomlTypes.Table.bindings tbl

  let add_entry r o c =
    T.set o c (entry r)

  let parse_file =
    Lwt_preemptive.detach Toml.Parser.from_filename

  let read ~default f filename =
    let filename = Fpath.to_string filename in
    let open Lwt.Infix in
    Lwt_unix.file_exists filename >>= function
    | false -> Lwt_result.return default
    | true ->
      parse_file filename >>= function
      | `Ok x -> Lwt_result.return (f x)
      | `Error _ -> Lwt_result.fail "Invalid configuration file."
  let update f filename =
    let filename = Fpath.to_string filename in
    let open Lwt.Infix in
    Lwt_unix.file_exists filename >>= fun b ->
    begin
      if b
      then parse_file filename
      else Lwt.return (`Ok TomlTypes.Table.empty)
    end >>= function
    | `Ok c ->
      Lwt_io.open_file ~mode:Output filename >>= fun oc ->
      let fmt = Lwt_fmt.of_channel oc in
      Lwt_fmt.fprintf fmt "%a@." Toml.Printer.table (f c) >>= fun () ->
      Lwt_result.return ()
    | `Error _ ->
      Lwt_result.fail "Invalid configuration file."

end


type t = {
  filename : Fpath.t option ;
  (* config : config Lazy.t; *)
}

open Rresult
open Bos.OS

let pre_create filename =
  match filename with
  | None -> R.ok ()
  | Some filename ->
    File.exists filename |> function
    | Ok true -> R.ok ()
    | Ok false | Error _ ->
      Dir.create ~path:true (Fpath.parent filename) >>| ignore

let mk filename =
  pre_create filename >>| fun () ->
  { filename ;
    (* config = lazy (read filename) ; *)
  }
(* let get t = Lazy.force t.config *)

let read ~default f t =
  match t.filename with
  | None -> Lwt_result.return default
  | Some file -> Format.read ~default f file

let get_entry t name =
  read ~default:None (Format.get_entry name) t

let get_all_entry t =
  read ~default:[] Format.get_all_entry t

let pp_entries =
  let pp_entry ppf (n,c) =
    Fmt.pf ppf "@[<v2>%s:@,%a@]" n (Fmt.option pp_record) c
  in
  Fmt.vbox @@ Fmt.list ~sep:Fmt.cut pp_entry

let add_entry t name r =
  match t.filename with
  | None -> Lwt_result.fail "Can't find the configuration file."
  | Some f -> Format.(update (add_entry name r)) f

let default_file =
  let open CCOpt.Infix in
  let open Fpath in
  let var s = Bos.OS.Env.var s >|= Fpath.v in
  let default_config_dir =
    var "XDG_CONFIG_HOME"
    <+> (var "HOME" >|= fun x -> x / ".config")
  in
  default_config_dir >|= fun s -> s / "fugit" / "config"
let term =
  let open Cmdliner in
  let doc = "Config file to use." in
  let i = Arg.info ~doc ["config"] in
  let file_conv = Arg.conv ((fun x -> Ok (Fpath.v x)), Fpath.pp) in
  let arg =
    Arg.(value & opt (some file_conv) default_file i)
  in
  Term.(term_result (pure mk $ arg))
