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
    value
    & opt string "default.jingoo"
    & info [ "j"; "template" ] ~docv:"TEMPLATE" ~doc)

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

let fetch url token template =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  let client =
    Client.make ~https:(Some (https ~authenticator:null_auth)) env#net
  in
  let rec fetch_github l acc =
    match fetch l client token with
    | Some (r, Some n) -> fetch_github n (Github.from_string r @ acc)
    | Some (r, None) -> Github.from_string r @ acc
    | None -> []
  in
  let content = fetch_github (Format.sprintf "%s?per_page=100" url) [] in
  Eio.Stdenv.stdout env
  |> Eio.Flow.copy_string @@ print_content content template

let fetch_t = Term.(const fetch $ url $ token $ template)

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
