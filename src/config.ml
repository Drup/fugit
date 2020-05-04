
type name = string
module NameTbl = CCHashtbl.Make(CCString)

type record = {
  message : string;
  duration : Delay.duration option;
  icon : Notif.icon option;
}
let mk_record ?duration ?icon message =
  { message ; duration ; icon }

type config = {
  records : record NameTbl.t
}

module Format = struct
  module T = TomlLenses
  let (|--) = T.(|--)

  let message = T.string
  let duration =
    let get x = CCResult.to_opt @@ Delay.parse_duration x in
    let set v _ = Some (Fmt.strf "%a" Delay.pp_duration v) in
    T.string |-- { get ; set }
  let icon = T.string

  let record =
    let get a =
      let message = T.get a message
      and icon = T.get a icon
      and duration = T.get a duration in
      CCOpt.map (mk_record ?icon ?duration) message
    and set a _ =
      let open CCOpt.Infix in
      let t = TomlTypes.(TTable Table.empty) in
      T.set a.message t message >>= fun t ->
      CCOpt.flat_map (fun d -> T.set d t duration) a.duration >>= fun t ->
      CCOpt.flat_map (fun i -> T.set i t icon) a.icon >>= fun t ->
      Some t
    in
    {T. get ; set}

  let get_record r c =
    T.get c (T.field "alerts" |-- T.key r |-- record)
  let add_record r o c =
    T.set o c (T.field "alerts" |-- T.key r |-- record)
    |> CCOpt.get_exn

  let parse_file =
    Lwt_preemptive.detach Toml.Parser.from_filename

  let read f filename =
    let filename = Fpath.to_string filename in
    let open Lwt.Infix in
    Lwt_unix.file_exists filename >>= function
    | false -> Lwt_result.return None
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

let get_record t name =
  match t.filename with
  | None -> Lwt_result.return None
  | Some f -> Format.(read (get_record name)) f
let add_record t name r =
  match t.filename with
  | None -> Lwt_result.fail "Can't find the configuration file."
  | Some f -> Format.(update (add_record name r)) f


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
