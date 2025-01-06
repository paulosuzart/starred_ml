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

let test_group () =
  let sample_java_repo =
    {
      name = "Xample";
      description = Some "Description";
      topics = [ "Flow" ];
      language = Some "Java";
      html_url = "example.com";
      owner = { login = "auser" };
      slug = Slug.slugify "Java" ~lowercase:false;
    }
  and sample_java_repo2 =
    {
      name = "Sample";
      description = Some "Description";
      topics = [ "Flow" ];
      language = Some "Java";
      html_url = "example.com";
      owner = { login = "viola" };
      slug = Slug.slugify "Java" ~lowercase:false;
    }
  and sample_ocaml_repo =
    {
      name = "Another Repo";
      description = Some "Description";
      topics = [ "Flow" ];
      language = Some "Ocaml";
      html_url = "example.com";
      owner = { login = "bar" };
      slug = Slug.slugify "Ocaml" ~lowercase:false;
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

let test_with_slug () =
  let src =
    Github.from_string
      {|[
        {"name" : "Sample with Slug",
         "language" : "Vim Script",
         "owner": {
            "login" : "foo"
         },
         "description" : null,
         "html_url" : "example.com",
         "topics" : []
        }
      ]|}
  in
  let repo =
    match src with
    | [] -> failwith "Non empty list needed for our test"
    | x :: _ -> x
  in
  Alcotest.(check string) "Repos get a slug" repo.slug "Vim-Script"

let testable_link = Alcotest.testable option_pp ( = )

let test_no_next_page () =
  Alcotest.(check testable_link)
    "A last page returns None" None
    (next_link
    @@ Http.Header.of_list [ ("Link", "<http://prev>; rel=\"prev\"") ])

let test_next_page () =
  Alcotest.(check testable_link)
    "A last page returns None" (Some "http://s")
    (next_link @@ Http.Header.of_list [ ("Link", "<http://s>; rel=\"next\"") ])

let () =
  run "Starred_ml"
    [
      ("Github", [ test_case "Group" `Quick test_group ]);
      ( "Http_util",
        [
          test_case "No Pagination" `Quick test_no_next_page;
          test_case "Next Pat" `Quick test_next_page;
          test_case "Slug test" `Quick test_with_slug;
        ] );
    ]
