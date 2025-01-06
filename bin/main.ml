open Cohttp_eio
open Starred_ml.Util
open Starred_ml.Http_util
open Cmdliner
module Github = Starred_ml.Github

let () = Logs.set_reporter (Logs_fmt.reporter ())
and () = Logs_threaded.enable ()

let template =
  let doc = "Template used to generate the markdown" in
  Arg.(
    value & opt file "default.jingoo"
    & info [ "j"; "template" ] ~docv:"TEMPLATE" ~doc)

let max_pages =
  let doc = "Max number of pages to be used" in
  Arg.(
    value
    & opt (some int) None
    & info [ "m"; "max-pages" ] ~docv:"MAX_PAGES" ~doc)

let token =
  let env =
    let doc = "Github Token." in
    Cmd.Env.info "TOKEN" ~doc
  in
  let doc = "Github Token argument." in
  Arg.(
    required
    & opt (some string) None
    & info [ "t"; "token" ] ~env ~docv:"TOKEN" ~doc)

let url =
  let env =
    let doc = "Env var for Github starred REST api endpoint." in
    Cmd.Env.info "GITHUB_URL" ~doc
  in
  let doc = "Github REST api endpoint." in
  Arg.(
    value
    & opt string "https://api.github.com/user/starred"
    & info [ "u"; "url" ] ~env ~docv:"GITHUB_URL" ~doc)

let fetch (max_pages : int option) url token template =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  let client =
    Client.make ~https:(Some (https ~authenticator:null_auth)) env#net
  in
  (* The rec function using max pages is generated here *)
  let fetch_github_with_max max =
    let rec fetch_github l acc (curr_page : int) =
      match fetch l client token with
      | Some (r, Some next_url) when curr_page <= max ->
          fetch_github next_url (Github.from_string r @ acc) (curr_page + 1)
      | Some (r, _) -> Github.from_string r @ acc
      | None -> []
    in
    fetch_github
  in
  (* The regular function without max page is pretty similar. It has no current_page tracking *)
  let rec fetch_github l acc =
    match fetch l client token with
    | Some (r, Some next_url) ->
        fetch_github next_url (Github.from_string r @ acc)
    | Some (r, _) -> Github.from_string r @ acc
    | None -> []
  in
  let content =
    match max_pages with
    | Some m ->
        (fetch_github_with_max m) (Format.sprintf "%s?per_page=100" url) [] 1
    | None -> fetch_github (Format.sprintf "%s?per_page=100" url) []
  in
  Eio.Stdenv.stdout env
  |> Eio.Flow.copy_string @@ print_content content template

let fetch_t = Term.(const fetch $ max_pages $ url $ token $ template)

let cmd =
  let doc = "Syncs Github starred items for the authenticated user" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Are you a compulsive Github stargazer? Starred_ml is here for you! It \
         will access Github https://api.github.com/user/starred API, fetch all \
         starred repositories and dump a mardown that you can use as README.md \
         in a repository.";
      `S Manpage.s_bugs;
      `P "Create a issue at https://github.com/paulosuzart/starred_ml";
    ]
  in
  let info = Cmd.info "starred_ml" ~version:"%%VERSION%%" ~doc ~man in
  Cmd.v info fetch_t

let () = Cmd.eval cmd |> exit
