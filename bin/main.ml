open Cohttp_eio
module Github = Starred_ml.Github

let () = Logs.set_reporter (Logs_fmt.reporter ())
and () = Logs_threaded.enable ()

let null_auth ?ip:_ ~host:_ _ = Ok None

let https ~authenticator =
  let tls_config = Tls.Config.client ~authenticator () in
  fun uri raw ->
    let host =
      Uri.host uri
      |> Option.map (fun x -> Domain_name.(host_exn (of_string_exn x)))
    in
    Tls_eio.client_of_flow ?host tls_config raw

let token = Sys.getenv "TOKEN"

let fetch sw client token =
  let headers =
    Eio.traceln "Token %s" token;

    Http.Header.of_list [ ("Authorization", Format.sprintf "Bearer %s" token) ]
  in
  let resp, body =
    Client.get ~headers ~sw client
      (Uri.of_string
      @@ Format.sprintf "https://api.github.com/user/starred?per_page=%i" 100)
  in
  if Http.Status.compare resp.status `OK = 0 then
    Some (Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int)
  else None

open Github
open Jingoo

let output models = Jg_template.from_file "default.jingoo" ~models

let print_content s =
  let items = Github.from_string s in
  let bz = Github.by_topic items in
  let m =
    List.map
      (fun (topic, items) ->
        Jg_types.Tobj
          [
            ("topic", Jg_types.Tstr topic);
            ( "starred",
              Jg_types.Tlist
                (List.map
                   (fun i ->
                     Jg_types.Tobj
                       [
                         ("name", Jg_types.Tstr i.name);
                         ("url", Jg_types.Tstr i.url);
                         ( "description",
                           match i.description with
                           | Some d -> Jg_types.Tstr d
                           | None -> Jg_types.Tnull );
                       ])
                   items) );
          ])
      bz
  in
  output [ ("by_topic", Jg_types.Tlist m) ]

let () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  let client =
    Client.make ~https:(Some (https ~authenticator:null_auth)) env#net
  in
  Eio.Switch.run @@ fun sw ->
  let t = token in
  match fetch sw client t with
  | Some r -> print_endline @@ print_content r
  | None -> Logs.info (fun m -> m ":aaa")
