type owner = { login : string }

type starred = {
  name : string;
  description : string option;
  topics : string list;
  language : string option;
  html_url : string;
  owner : owner;
}

type starred_response = starred list

val show_starred : starred -> string
(** Pretty-prints a [starred] record; used in test assertions. *)

val from_string : string -> starred_response
(** Converts a result page of starred paged result into a list of starred *)

val by_language : starred list -> (string * starred list) list
(** Converts a list of starred items into a struct grouped by language like
    [("java", [starred; starred]), ("scala", [starred;...])] *)

val languages : ?default_language:string -> starred list -> string list
(** Return the languages for the repositories. *)
