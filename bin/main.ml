open Cmdliner

let () = Logs.set_reporter (Logs_fmt.reporter ())
and () = Logs_threaded.enable ()
and () = Printexc.record_backtrace true

module Render_cli = struct
  let name = "render"

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

  let fetch_t = Term.(const Fetcher.run $ max_pages $ url $ token $ template)

  let cmd =
    let doc = "Syncs Github starred items for the authenticated user" in
    let man =
      [
        `S Manpage.s_description;
        `P
          "Are you a compulsive Github stargazer? Starred_ml is here for you! \
           It will access Github https://api.github.com/user/starred API, \
           fetch all starred repositories and dump a mardown that you can use \
           as README.md in a repository.";
        `S Manpage.s_bugs;
        `P "Create a issue at https://github.com/paulosuzart/starred_ml";
      ]
    in
    let info = Cmd.info name ~version:"%%VERSION%%" ~doc ~man in
    Cmd.v info fetch_t
end

module Main_cli = struct
  let cmd =
    let doc = "Syncs Github starred items for the authenticated user" in
    let man =
      [
        `S Manpage.s_description;
        `P
          "Are you a compulsive Github stargazer? Starred_ml is here for you! \
           It will access Github https://api.github.com/user/starred API, \
           fetch all starred repositories and dump a mardown that you can use \
           as README.md in a repository.";
        `S Manpage.s_bugs;
        `P "Create a issue at https://github.com/paulosuzart/starred_ml";
      ]
    in
    let info = Cmd.info "starred_ml" ~version:"%%VERSION%%" ~doc ~man in
    Cmd.group info [ Render_cli.cmd ]
end

let () = Cmd.eval Main_cli.cmd |> exit
