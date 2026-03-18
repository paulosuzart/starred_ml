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

let run (max_pages : int option) (page_size : int) (timeout_s : float)
    (max_retries : int) url token template =
  if page_size < 1 || page_size > 100 then (
    Printf.eprintf "Error: --page-size must be between 1 and 100 (got %d)\n"
      page_size;
    exit 1);
  total_pages := 0;
  let t0 = Unix.gettimeofday () in
  try
    Eio_main.run @@ fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let clock = env#mono_clock in
    let config = { timeout_s; max_retries; backoff_base_s = 1.0 } in
    let client =
      Client.make ~https:(Some (https ~authenticator:null_auth)) env#net
    in
    Eio.Switch.run @@ fun sw ->
    (* Stream carries one page-batch per item; None signals end-of-stream. *)
    let stream : Github.starred list option Eio.Stream.t =
      Eio.Stream.create 2
    in
    (* Producer fiber: pages through the API, pushes batches onto the stream.
       Uses [Eio.Fiber.fork ~sw] (fire-and-forget): exceptions cancel the
       switch, which unblocks [Eio.Stream.take] in the consumer automatically
       via structured concurrency — no extra error handling needed. *)
    Eio.Fiber.fork ~sw (fun () ->
        let rec produce url curr_page frame =
          show_progress frame curr_page max_pages;
          match fetch ~sw ~clock ~config url client token with
          | Some (body, next_url_opt) -> (
              Eio.Stream.add stream (Some (Github.from_string body));
              let within_limit =
                Option.value ~default:max_int max_pages > curr_page
              in
              match next_url_opt with
              | Some next_url when within_limit ->
                  produce next_url (curr_page + 1) (frame + 1)
              | _ -> ())
          | None -> ()
        in
        produce (Format.sprintf "%s?per_page=%d" url page_size) 1 0;
        Eio.Stream.add stream None);
    (* Consumer: drains the stream into a Queue (O(1) push per item).
       [Queue.to_seq] traverses front-to-back, preserving GitHub insertion
       order with no reversal needed. *)
    let q = Queue.create () in
    let rec consume () =
      match Eio.Stream.take stream with
      | Some batch ->
          List.iter (fun item -> Queue.push item q) batch;
          consume ()
      | None -> ()
    in
    consume ();
    let content = Queue.to_seq q |> List.of_seq in
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
