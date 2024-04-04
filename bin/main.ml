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

let next_link (s : Http.Header.t) =
  Eio.traceln "%s" @@ Http.Header.to_string s;
  match Http.Header.get s "Link" with
  | None -> None
  | Some l ->
      let re = Re2.create_exn "<([^;]+)>; rel=\"next\"" in
      let link =
        try Some (Re2.find_first_exn ~sub:(`Index 1) re l)
        with Re2.Exceptions.Regex_match_failed _ -> None
      in
      link

let fetch apai_url client token =
  Eio.Switch.run @@ fun sw ->
  let headers =
    Http.Header.of_list [ ("Authorization", Format.sprintf "Bearer %s" token) ]
  in
  let resp, body = Client.get ~headers ~sw client (Uri.of_string apai_url) in

  if Http.Status.compare resp.status `OK = 0 then
    Some
      ( Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int,
        next_link resp.headers )
  else None

open Github
open Jingoo

let render_template models = Jg_template.from_file "default.jingoo" ~models

let unique_lang (bz : (string * starred list) list) =
  let rec unique' b acc =
    match b with [] -> acc | (lang, _) :: xs -> unique' xs (lang :: acc)
  in
  let u = unique' bz [] |> List.rev in
  Jg_types.Tlist (List.map (fun w -> Jg_types.Tstr w) u)

let print_content items =
  let bz = Github.by_language items in
  let unique_languages = unique_lang bz in
  let m =
    List.map
      (fun (language, items') ->
        Jg_types.Tobj
          [
            ("language", Jg_types.Tstr language);
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
                   items') );
          ])
      bz
  in
  let count = List.length bz in
  render_template
    [
      ("lang_count", Jg_types.Tint count);
      ("languages", unique_languages);
      ("by_language", Jg_types.Tlist m);
    ]

let () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  let client =
    Client.make ~https:(Some (https ~authenticator:null_auth)) env#net
  in
  let t =
    try Sys.getenv "TOKEN"
    with Not_found ->
      Eio.traceln "You need to provide a TOKEN env";
      raise Not_found
  in

  let rec fetch_github l acc =
    match fetch l client t with
    | Some (r, Some n) -> fetch_github n (Github.from_string r @ acc)
    | Some (r, None) -> Github.from_string r @ acc
    | None -> []
  in
  let content =
    fetch_github "https://api.github.com/user/starred?per_page=100" []
  in
  Eio.Stdenv.stdout env |> Eio.Flow.copy_string @@ print_content content
