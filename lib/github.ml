type starred = {
  name : string;
  description : string option;
  topics : string list;
  language : string option;
  url : string;
}
[@@deriving show, yojson { strict = false; exn = true }]

type starred_response = starred list
[@@deriving yojson { strict = false; exn = true }]

let from_string s = Yojson.Safe.from_string s |> starred_response_of_yojson_exn

let group_by_first lst =
  let ht = Hashtbl.create 30 in
  List.iter
    (fun (key, value) ->
      let values = try Hashtbl.find ht key with Not_found -> [] in
      Hashtbl.replace ht key (value :: values))
    lst;
  Hashtbl.fold (fun key values acc -> (key, List.rev values) :: acc) ht []

let by_topic s =
  let bz =
    List.map (fun i -> List.map (fun topic -> (topic, i)) i.topics) s
    |> List.flatten
  in
  group_by_first bz
