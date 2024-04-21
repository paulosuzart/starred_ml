open Jingoo
open Github

let render_template models template = Jg_template.from_file template ~models

let unique_lang (bz : (string * starred list) list) =
  let rec unique' b acc =
    match b with [] -> acc | (lang, _) :: xs -> unique' xs (lang :: acc)
  in
  let u = unique' bz [] |> List.rev in
  Jg_types.Tlist (List.map (fun w -> Jg_types.Tstr w) u)

let print_content items template =
  let bz = Github.by_language items in
  let unique_languages = unique_lang bz in
  let m =
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
  let count = List.length bz in
  render_template
    [
      ("lang_count", Jg_types.Tint count);
      ("languages", unique_languages);
      ("by_language", Jg_types.Tlist m);
    ]
    template
