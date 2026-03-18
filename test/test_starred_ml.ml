open Alcotest
module Github = Starred_ml.Github
module Http_util = Starred_ml.Http_util
open Http_util
open Github
module Util = Starred_ml.Util

let starred_pp ppf i =
  List.iter
    (fun (t, p) ->
      List.iter (fun z -> Fmt.pf ppf "%s -> %s" t (show_starred z)) p)
    i

let starred_testable = Alcotest.testable starred_pp ( = )

(** [test_group] verifies that [by_language] correctly partitions a mixed list
    of repos into per-language buckets, preserving insertion order within each
    bucket and ordering buckets by first occurrence. *)
let test_group () =
  let sample_java_repo =
    {
      name = "Xample";
      description = Some "Description";
      topics = [ "Flow" ];
      language = Some "Java";
      html_url = "example.com";
      owner = { login = "auser" };
    }
  and sample_java_repo2 =
    {
      name = "Sample";
      description = Some "Description";
      topics = [ "Flow" ];
      language = Some "Java";
      html_url = "example.com";
      owner = { login = "viola" };
    }
  and sample_ocaml_repo =
    {
      name = "Another Repo";
      description = Some "Description";
      topics = [ "Flow" ];
      language = Some "Ocaml";
      html_url = "example.com";
      owner = { login = "bar" };
    }
  in
  Alcotest.(check starred_testable)
    "Repos are grouped by topic"
    [
      ("Java", [ sample_java_repo2; sample_java_repo ]);
      ("Ocaml", [ sample_ocaml_repo ]);
    ]
    (by_language [ sample_java_repo; sample_ocaml_repo; sample_java_repo2 ])

let option_pp ppf o =
  match o with Some l -> Fmt.pf ppf "%s" l | None -> Fmt.pf ppf "No next link"

let testable_link = Alcotest.testable option_pp ( = )

(** [test_no_next_page] verifies that [next_link] returns [None] when the [Link]
    header only contains a [prev] relation, indicating we are on the last page
    of Github pagination. *)
let test_no_next_page () =
  Alcotest.(check testable_link)
    "A last page returns None" None
    (next_link
    @@ Http.Header.of_list [ ("Link", "<http://prev>; rel=\"prev\"") ])

(** [test_next_page] verifies that [next_link] extracts the URL from a [Link]
    header that contains a [next] relation, which Github uses to signal there
    are more pages of starred repos to fetch. *)
let test_next_page () =
  Alcotest.(check testable_link)
    "A page with next link returns Some url" (Some "http://s")
    (next_link @@ Http.Header.of_list [ ("Link", "<http://s>; rel=\"next\"") ])

(** [test_unauthorized] checks that a 401 response is classified as a fatal
    error with a message instructing the user to check their token. *)
let test_unauthorized () =
  Alcotest.(check string)
    "401 is Fatal with token hint"
    "401 Unauthorized. Please check the provided token."
    (match Http_util.classify_status `Unauthorized with
    | `Fatal msg -> msg
    | _ -> "unexpected variant")

(** [test_server_error] checks that a 5xx response is classified as transient,
    meaning the caller can retry the request. *)
let test_server_error () =
  Alcotest.(check string)
    "500 is Transient with status string" "500 Internal Server Error"
    (match Http_util.classify_status `Internal_server_error with
    | `Transient msg -> msg
    | _ -> "unexpected variant")

(** [test_client_error] checks that a generic 4xx response (e.g. 403 Forbidden)
    is classified as a fatal error with the HTTP status string. *)
let test_client_error () =
  Alcotest.(check string)
    "403 is Fatal with status string" "403 Forbidden"
    (match Http_util.classify_status `Forbidden with
    | `Fatal msg -> msg
    | _ -> "unexpected variant")

let () =
  run "Starred_ml"
    [
      ("Github", [ test_case "Group" `Quick test_group ]);
      ( "Http_util",
        [
          test_case "No Pagination" `Quick test_no_next_page;
          test_case "Next Pat" `Quick test_next_page;
          test_case "Unauthorized" `Quick test_unauthorized;
          test_case "Server Error" `Quick test_server_error;
          test_case "Client Error" `Quick test_client_error;
        ] );
    ]
