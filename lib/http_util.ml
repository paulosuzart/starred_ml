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

let handle_status status =
  match status with
  | `OK -> ()
  | `Unauthorized ->
      failwith
        (Http.Status.to_string status ^ ". Please check the provided token.")
  | #Http.Status.client_error | #Http.Status.server_error ->
      failwith (Http.Status.to_string status)
  | status ->
      raise
        (Invalid_argument
           (Printf.sprintf "Catastrophic failure: unexpected status %s"
              (Http.Status.to_string status)))

let fetch ~sw api_url client token =
  let headers =
    Http.Header.of_list [ ("Authorization", Format.sprintf "Bearer %s" token) ]
  in
  let resp, body = Client.get ~headers ~sw client (Uri.of_string api_url) in
  handle_status resp.status;
  Some
    ( Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int,
      next_link resp.headers )
