open Jingoo
open Github

let language_not_set = "Not Set"
let render_template models template = Jg_template.from_file template ~models

module TemplateVariables = struct
  let variables = [ "lang_count"; "languages"; "repositories"; "by_language" ]

  exception NonDeclaredVariable of string

  (* Will be repalced by a ppx module for compile time check *)
  let ensure_variables model =
    let used_variables = List.map (fun (v, _) -> v) model in
    if not (List.for_all (fun e -> List.mem e variables) used_variables) then
      raise (NonDeclaredVariable "All variables must be declared")
    else model
end

let print_content items template =
  let bz = Github.by_language items in
  let unique_languages =
    Github.languages ~default_language:language_not_set items
  in
  let unique_language_model =
    List.map (fun i -> Jg_types.Tstr i) unique_languages
  in
  let by_language =
    List.map
      (fun (language, items') ->
        Jg_types.Tobj
          [
            ("language", Jg_types.Tstr language);
            ( "starred",
              Jg_types.Tlist
                (List.map
                   (fun i ->
                     Jg_types.Tobj
                       [
                         ("name", Jg_types.Tstr i.name);
                         ( "language",
                           match i.language with
                           | Some l -> Jg_types.Tstr l
                           | None -> Jg_types.Tstr language_not_set );
                         ("html_url", Jg_types.Tstr i.html_url);
                         ( "description",
                           match i.description with
                           | Some d -> Jg_types.Tstr d
                           | None -> Jg_types.Tnull );
                         ("owner_login", Jg_types.Tstr i.owner.login);
                       ])
                   items') );
          ])
      bz
  in
  let repositories =
    List.map
      (fun i ->
        Jg_types.Tobj
          [
            ("name", Jg_types.Tstr i.name);
            ( "language",
              match i.language with
              | Some l -> Jg_types.Tstr l
              | None -> Jg_types.Tstr language_not_set );
            ("html_url", Jg_types.Tstr i.html_url);
            ( "description",
              match i.description with
              | Some d -> Jg_types.Tstr d
              | None -> Jg_types.Tnull );
            ("owner_login", Jg_types.Tstr i.owner.login);
          ])
      items
  in
  let count = List.length unique_languages in
  let model =
    TemplateVariables.ensure_variables
      [
        ("lang_count", Jg_types.Tint count);
        ("languages", Jg_types.Tlist unique_language_model);
        ("repositories", Jg_types.Tlist repositories);
        (* Will be removed in the future *)
        ("by_language", Jg_types.Tlist by_language);
      ]
  in
  render_template model template
