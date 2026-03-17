open Cohttp_eio
open Starred_ml.Util
open Starred_ml.Http_util
module Github = Starred_ml.Github

let spinner_frames = [| "/"; "-"; "\\"; "|" |]
let total_pages = ref 0

let show_progress frame page (max_pages : int option) =
  let max = Option.fold ~none:"" ~some:(Printf.sprintf "of %i") max_pages in
  Printf.eprintf "\r%s Fetching page %d %s..."
    spinner_frames.(frame mod Array.length spinner_frames)
    page max;
  incr total_pages;
  flush stderr

let clear_progress () =
  Printf.eprintf "\r\027[K";
  flush stderr

let print_summary repo_count elapsed =
  Printf.eprintf "✓ %d page%s  •  %d repositor%s  •  %.1fs\n" !total_pages
    (if !total_pages = 1 then "" else "s")
    repo_count
    (if repo_count = 1 then "y" else "ies")
    elapsed

let run (max_pages : int option) url token template =
  total_pages := 0;
  let t0 = Unix.gettimeofday () in
  try
    Eio_main.run @@ fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let client =
      Client.make ~https:(Some (https ~authenticator:null_auth)) env#net
    in
    Eio.Switch.run @@ fun sw ->
    (* [fetch_github l acc curr_page frame] recursively pages through the
       Github starred API starting at URL [l], accumulating parsed repos in
       [acc].

       [curr_page] tracks the logical page number so it can be compared
       against [max_pages]: once [curr_page] exceeds the limit, or the
       response carries no [next] link, recursion stops.

       [frame] is an ever-incrementing counter passed solely to advance the
       terminal spinner; it has no effect on the fetching logic. *)
    let rec fetch_github l acc curr_page frame =
      show_progress frame curr_page max_pages;
      let result =
        Eio.Fiber.fork_promise ~sw (fun () -> fetch ~sw l client token)
        |> Eio.Promise.await_exn
      in
      match result with
      | Some (r, Some next_url)
        when Option.value ~default:max_int max_pages > curr_page ->
          fetch_github next_url
            (acc @ Github.from_string r)
            (curr_page + 1) (frame + 1)
      | Some (r, _) -> acc @ Github.from_string r
      | None -> acc
    in
    let content = fetch_github (Format.sprintf "%s?per_page=100" url) [] 1 0 in
    clear_progress ();
    print_summary (List.length content) (Unix.gettimeofday () -. t0);
    Eio.Stdenv.stdout env
    |> Eio.Flow.copy_string @@ print_content content template
  with
  | Failure msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | exn ->
      Printf.eprintf "Fatal: %s\n" (Printexc.to_string exn);
      Printexc.print_backtrace stderr;
      exit 1
