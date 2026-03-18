open Alcotest
module Http_util = Starred_ml.Http_util
open Http_util

let make_attempt responses =
  let q = Queue.of_seq (List.to_seq responses) in
  fun () -> match Queue.pop q with `Raise exn -> raise exn | `Return r -> r

let base_config = { timeout_s = 1.0; max_retries = 2; backoff_base_s = 0.5 }

(** Retries twice on transient (500) errors then succeeds on 200. *)
let test_retry_transient () =
  let sleeps = ref [] in
  let sleep_fn d = sleeps := d :: !sleeps in
  let attempt =
    make_attempt
      [
        `Return (`Internal_server_error, "", None);
        `Return (`Internal_server_error, "", None);
        `Return (`OK, "body", None);
      ]
  in
  let result = retry ~sleep_fn ~config:base_config ~attempt in
  check bool "got result" true (Option.is_some result);
  check int "slept twice" 2 (List.length !sleeps)

(** Retries twice on Timeout exceptions then succeeds. *)
let test_retry_timeout () =
  let sleeps = ref [] in
  let sleep_fn d = sleeps := d :: !sleeps in
  let attempt =
    make_attempt
      [
        `Raise Eio.Time.Timeout;
        `Raise Eio.Time.Timeout;
        `Return (`OK, "body", None);
      ]
  in
  let result = retry ~sleep_fn ~config:base_config ~attempt in
  check bool "got result" true (Option.is_some result);
  check int "slept twice" 2 (List.length !sleeps)

(** Fatal error (401) never retries — sleep never called. *)
let test_fatal_no_retry () =
  let sleeps = ref [] in
  let sleep_fn d = sleeps := d :: !sleeps in
  let attempt = make_attempt [ `Return (`Unauthorized, "", None) ] in
  check_raises "raises on fatal"
    (Failure "401 Unauthorized. Please check the provided token.")
    (fun () -> ignore (retry ~sleep_fn ~config:base_config ~attempt));
  check int "never slept" 0 (List.length !sleeps)

(** All retries exhausted on transient errors raises Failure. *)
let test_exhausted_retries () =
  let sleep_fn _ = () in
  let attempt =
    make_attempt
      [
        `Return (`Internal_server_error, "", None);
        `Return (`Internal_server_error, "", None);
        `Return (`Internal_server_error, "", None);
      ]
  in
  check_raises "fails after all retries"
    (Failure "500 Internal Server Error after all retries") (fun () ->
      ignore (retry ~sleep_fn ~config:base_config ~attempt))

(** Sleep is called with exponentially doubling delays. *)
let test_backoff_doubling () =
  let sleeps = ref [] in
  let sleep_fn d = sleeps := d :: !sleeps in
  let attempt =
    make_attempt
      [
        `Return (`Internal_server_error, "", None);
        `Return (`Internal_server_error, "", None);
        `Return (`OK, "body", None);
      ]
  in
  ignore (retry ~sleep_fn ~config:base_config ~attempt);
  let recorded = List.rev !sleeps in
  check (list (float 0.001)) "delays double" [ 0.5; 1.0 ] recorded

let () =
  run "Retry"
    [
      ( "Http_util.retry",
        [
          test_case "Retry on transient" `Quick test_retry_transient;
          test_case "Retry on timeout" `Quick test_retry_timeout;
          test_case "Fatal no retry" `Quick test_fatal_no_retry;
          test_case "Exhausted retries" `Quick test_exhausted_retries;
          test_case "Backoff doubling" `Quick test_backoff_doubling;
        ] );
    ]
