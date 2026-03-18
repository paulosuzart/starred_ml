open Cohttp_eio

(** Just a empty authz. Copied from cohttp. Needsinvestigation *)
let null_auth ?ip:_ ~host:_ _ = Ok None

(** Effectively calls the https endpoint *)
let https ~authenticator =
  let tls_config =
    match Tls.Config.client ~authenticator () with
    | Error (`Msg msg) -> failwith ("tls configuration problem: " ^ msg)
    | Ok tls_config -> tls_config
  in
  fun uri raw ->
    let host =
      Uri.host uri
      |> Option.map (fun x -> Domain_name.(host_exn (of_string_exn x)))
    in
    Tls_eio.client_of_flow ?host tls_config raw

(** Github uses rel links to indicate the next page. It's better to rely on them
    instead of keeping a page counter *)
let next_re = Re2.create_exn "<([^;]+)>; rel=\"next\""

let next_link s =
  match Http.Header.get s "Link" with
  | None -> None
  | Some l -> (
      try Some (Re2.find_first_exn ~sub:(`Index 1) next_re l)
      with Re2.Exceptions.Regex_match_failed _ -> None)

let classify_status status =
  match status with
  | `OK -> `Ok
  | `Unauthorized ->
      `Fatal
        (Http.Status.to_string status ^ ". Please check the provided token.")
  | `Too_many_requests -> `Transient (Http.Status.to_string status)
  | #Http.Status.server_error -> `Transient (Http.Status.to_string status)
  | #Http.Status.client_error -> `Fatal (Http.Status.to_string status)
  | s ->
      `Fatal (Printf.sprintf "Unexpected status %s" (Http.Status.to_string s))

type fetch_config = {
  timeout_s : float;
  max_retries : int;
  backoff_base_s : float;
}

let fetch ~sw ~clock ~config api_url client token =
  let headers =
    Http.Header.of_list [ ("Authorization", Format.sprintf "Bearer %s" token) ]
  in
  let timeout = Eio.Time.Timeout.seconds clock config.timeout_s in
  let attempt () =
    Eio.Time.Timeout.run_exn timeout (fun () ->
        let resp, body =
          Client.get ~headers ~sw client (Uri.of_string api_url)
        in
        let body_str =
          Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int
        in
        ( resp.Http.Response.status,
          body_str,
          next_link resp.Http.Response.headers ))
  in
  let sleep_with_jitter delay =
    let jittered = delay +. (0.1 +. Random.float 0.9) in
    Eio.Time.Mono.sleep clock jittered
  in
  let rec loop retries delay =
    match attempt () with
    | exception Eio.Time.Timeout ->
        if retries = 0 then failwith "Request timed out after all retries"
        else (
          sleep_with_jitter delay;
          loop (retries - 1) (delay *. 2.0))
    | exception (Eio.Io _ as e) ->
        if retries = 0 then raise e
        else (
          sleep_with_jitter delay;
          loop (retries - 1) (delay *. 2.0))
    | status, body_str, next_url -> (
        match classify_status status with
        | `Ok -> Some (body_str, next_url)
        | `Fatal msg -> failwith msg
        | `Transient msg ->
            if retries = 0 then failwith (msg ^ " after all retries")
            else (
              sleep_with_jitter delay;
              loop (retries - 1) (delay *. 2.0)))
  in
  loop config.max_retries config.backoff_base_s
