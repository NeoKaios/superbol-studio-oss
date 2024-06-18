(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open EzCompat

open Cobol_common                                                  (* Visitor *)
open Cobol_common.Srcloc.INFIX

open Lsp_completion_keywords
open Lsp.Types

module Menhir = struct include Cobol_parser.INTERNAL.Grammar.MenhirInterpreter end
module Expect = struct include Cobol_parser.Expect end

let name_proposals ast ~filename pos =
  let visitor = object
    inherit [StringSet.t] Cobol_ptree.Visitor.folder

    method! fold_compilation_unit' cu =
      if Lsp_position.is_in_srcloc ~filename pos ~@cu
      then Visitor.do_children
      else Visitor.skip_children

    method! fold_name name acc =
      Visitor.skip_children @@ StringSet.add name acc

    end
  in
  Cobol_ptree.Visitor.fold_compilation_group visitor ast StringSet.empty
  |> StringSet.elements

let qualname_proposals ~filename pos group =
  match Lsp_lookup.cobol_unit_at_position ~filename pos group with
  | None -> []
  | Some cu ->
    List.filter_map Cobol_data.Item.def_qualname cu.unit_data.data_items.list

let procedure_proposals ~filename pos group =
  match Lsp_lookup.cobol_unit_at_position ~filename pos group with
  | None -> []
  | Some cu ->
      let map_paragraph (paragraph:Cobol_unit.Types.procedure_paragraph with_loc) =
        Option.to_list @@ Option.map Cobol_common.Srcloc.payload ~&paragraph.paragraph_name
      in
      List.flatten @@ List.map (function
        | Cobol_unit.Types.Paragraph paragraph -> map_paragraph paragraph
        | Section section ->
            List.flatten @@ List.map map_paragraph ~&section.section_paragraphs.list)
      cu.unit_procedure.list

(*If need be, get the qualname_proposals "X OF Y"... from the definition maps*)


(*TODO: If the partial parsing could give more information
        like in which statement the position is(or even better, in which clause/phrase),
        Then we can remove the keywords that cannot appear in this statement from
        the keyword list.
*)
(* type div =
| Ident_div
| Env_div
| Data_div
| Proc_div

let keyword_proposals ast pos =
  let visitor = object
    inherit [div option] Cobol_parser.PTree_visitor.folder

    method! fold_data_division' {loc; _} _ =
      Visitor.skip_children @@
        if Lsp_position.is_in_srcloc pos loc
        then Some Data_div
        else None

    method! fold_procedure_division' {loc; _} _ =
      Visitor.skip_children @@
        if Lsp_position.is_in_srcloc pos loc
        then Some Proc_div
        else None

    end
  in
  match Cobol_parser.PTree_visitor.fold_compilation_group visitor ast None with
  | Some Proc_div -> keywords_proc
  | Some Data_div -> keywords_data (*does not work*)
  | _ -> [] *)

let keyword_proposals _ast _pos = keywords_all

let completion_item label ~range ~kind =
    (*we may change the ~sortText/preselect for reason of priority *)
    let textedit = TextEdit.create ~newText:label ~range in
    CompletionItem.create
      ~label
      ~kind
      ~preselect:false
      ~textEdit:(`TextEdit textedit)
      ()

let range (pos:Position.t) text =
  let { line; character }: Position.t = pos in
  let get_nthline s n =
    let rec inner line_start i : int * int =
      let line_end = try String.index_from s line_start '\n' with _ -> String.length s in
      if i >= n
        then (line_start, line_end)
        else inner (line_end+1) (i+1)
      in
      let (start,end_) = inner 0 0 in
      String.sub s start (end_-start)
      in
    let text_line = get_nthline (Lsp.Text_document.text text) line in
    let index =
      try 1 + String.rindex_from text_line (character - 1) ' ' with _ ->
      try 1 + String.rindex_from text_line (character - 1) '\t'
      with _ -> 0 in
    let position_start = Position.create ~character:index ~line in
    Range.create ~start:position_start ~end_:pos

let map_to_completion_item ~kind ~range qualnames =
  List.flatten @@ List.map (function
    | Cobol_ptree.Name name -> [~&name]
    | Qual (name,_) as qualname ->
        [~&name; Pretty.to_string "%a" Cobol_ptree.pp_qualname qualname]
    | _ -> []) qualnames |>
    List.map @@ completion_item ~kind ~range

let get_completion_items ~(range:Range.t) ~group ~filename comp_entries =
      let pos = range.end_ in
      if List.length comp_entries < 10 then
        Lsp_io.log_info "Comp entries are [%a]\n" (Fmt.list ~sep:(Fmt.any ";") Expect.pp_completion_entry) comp_entries
      else
        Lsp_io.log_info "====> Comp entries are %d <====\n" (List.length comp_entries);
      List.flatten @@ List.map (function
        | Expect.QualifiedRef ->
            map_to_completion_item
              ~kind:CompletionItemKind.Variable ~range
              (qualname_proposals ~filename pos group)
        | ProcedureRef ->
            map_to_completion_item
            ~kind:CompletionItemKind.Module ~range
            (procedure_proposals ~filename range.end_ group)
        | K token ->
            let token' = token &@ Srcloc.dummy in
            try let token = Pretty.to_string "%a" Cobol_parser.INTERNAL.pp_token token' in
              [completion_item ~kind:CompletionItemKind.Keyword ~range token]
            with Not_found -> [])
      comp_entries

let listpop l i =
  let rec inner h t i =
    if i == 0 then (h, t) else
    match t with
    | [] -> (h, [])
    | hd::tl -> inner (hd::h) tl (i-1)
  in let h, t = inner [] l i in List.rev h, t

let context_completion_items (doc:Lsp_document.t) Cobol_typeck.Outputs.{ group; _ } (pos:Position.t) =
  let filename = Lsp.Uri.to_path (Lsp.Text_document.documentUri doc.textdoc) in
  let range = range pos doc.textdoc in
  let start_pos = range.start in
  let rec pop env =
    let state = Expect.state_of_int (Menhir.current_state_number env) in
    let has_default = (Menhir.env_has_default_reduction env) in
    match Menhir.pop env with
        | None -> [(state, has_default)]
        | Some env -> (state, has_default)::(pop env) in
  let f env_l = begin
    let rec inner env_l acc =
      match env_l with
      | [] -> []
      | (state, true)::_tl -> begin
        Lsp_io.log_info "State with default: %d" (Expect.state_to_int state);
        let acc = Expect.transition_tokens state @ acc in
        let rhslen, lhs = Expect.get_default_production state in (* This may need to be a list, imagine lr1 891 but with more cases *)
        let _, states = listpop env_l rhslen in
        match states with
        | [] -> acc
        | (redu_state,_)::_ -> begin
          let next = Expect.follow_transition redu_state lhs in
          Lsp_io.log_info "Following %d -> %d" (Expect.state_to_int redu_state) (Expect.state_to_int @@ fst next);
          inner (next::states) acc
        end
      end
      | (state, false)::_tl ->
          Lsp_io.log_info "State without default: %d" (Expect.state_to_int state);
          Expect.transition_tokens state @ acc
    in
    let comp_entries = inner env_l [] in
    get_completion_items ~range ~group ~filename comp_entries

  end in

  begin match Lsp_document.inspect_at ~position:start_pos doc with
    | Some Env env -> begin
      Lsp_io.log_info "%a" Fmt.(list ~sep:(any " ") (fun ppf (i,d) ->
        Fmt.pf ppf "%d%s" (Expect.state_to_int i) (if d then "T" else "")
      )) (pop env);
       f @@ pop env
       end
    | _ -> [] end

let completion_items (doc:Lsp_document.t) (pos:Position.t) ast =
  let text = doc.textdoc in
  let filename = Lsp.Uri.to_path (Lsp.Text_document.documentUri text) in
  let range = range pos text in
  let names = name_proposals ast ~filename pos in
  let keywords = keyword_proposals ast pos in
  let keywordsItem = List.map (completion_item ~kind:CompletionItemKind.Keyword ~range) keywords in
  let namesItem = List.map (completion_item ~kind:CompletionItemKind.Method ~range) names in
    keywordsItem @ namesItem
