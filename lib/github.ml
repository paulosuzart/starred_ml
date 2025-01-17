type owner = { login : string }
[@@deriving show, yojson { strict = false; exn = true }]

type starred = {
  name : string;
  description : string option;
  topics : string list;
  language : string option;
  html_url : string;
  owner : owner;
}
[@@deriving show, yojson { strict = false; exn = true }]

type starred_response = starred list
[@@deriving yojson { strict = false; exn = true }]

let from_string s = Yojson.Safe.from_string s |> starred_response_of_yojson_exn
let language_not_set = "Not Set"

(** group_by_first will group starred items by its language, if present. returns
    a assoc list of starred items. The list is sorted by language
    alphabetically. Each left value of a language (the list of starred) is also
    sorted alphabetically. *)
let group_by_first lst =
  let ht = Hashtbl.create 30 in
  List.iter
    (fun (key, value) ->
      let values = try Hashtbl.find ht key with Not_found -> [] in
      Hashtbl.replace ht key (value :: values))
    lst;
  Hashtbl.fold
    (fun lang repos acc ->
      (* Sorts the language list while folding into the final assoc list*)
      (lang, List.sort (fun e e2 -> compare e.name e2.name) repos) :: acc)
    ht []
  |> List.sort (fun (c1, _) (c2, _) -> Stdlib.compare c1 c2)

let by_language s =
  let bz =
    List.map
      (fun i ->
        match i.language with Some l -> (l, i) | None -> (language_not_set, i))
      s
  in
  group_by_first bz

module StringSet = Set.Make (String)

let languages ?(default_language = language_not_set) starred_items =
  StringSet.elements @@ StringSet.of_list
  @@ List.map
       (fun item ->
         match item.language with Some l -> l | None -> default_language)
       starred_items
