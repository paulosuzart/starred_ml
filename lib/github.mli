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

val from_string : string -> starred_response
(** Converts a result page of starred paged result into a list of starred *)

val by_language : starred list -> (string * starred list) list
(** Converts a list of starred items into a struc grouped by language like
    [("java", [starred; starred]), ("scala", [starred;...])] *)

val languages : ?default_language:string -> starred list -> string list
(** Return the languages for the repositories. *)
