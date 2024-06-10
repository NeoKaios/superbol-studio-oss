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

open Cobol_parser.Expect
open Lsp_completion_keywords
open Lsp.Types

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
    let line = pos.line in
    let character = pos.character in
    let texts = String.split_on_char '\n' @@ Lsp.Text_document.text text in
    let text_line = List.nth texts line in
    let index = try 1 + String.rindex_from text_line (character - 1) ' ' with _ -> 1 in
    let position_start = Position.create ~character:index ~line in
    Range.create ~start:position_start ~end_:pos

let map_to_completion_item ~kind ~range qualnames =
  List.flatten @@ List.map (function
    | Cobol_ptree.Name name -> [~&name]
    | Qual (name,_) as qualname ->
        [~&name; Pretty.to_string "%a" Cobol_ptree.pp_qualname qualname]
    | _ -> []) qualnames |>
    List.map @@ completion_item ~kind ~range

let context_completion_items (doc:Lsp_document.t) Cobol_typeck.Outputs.{ group; _ } (pos:Position.t) =
  let filename = Lsp.Uri.to_path (Lsp.Text_document.documentUri doc.textdoc) in
  let range = range pos doc.textdoc in
  let start_pos = range.start in
  begin match Lsp_document.inspect_at ~position:start_pos doc with
    | Some Env env -> Some (Cobol_parser.INTERNAL.Grammar.MenhirInterpreter.current_state_number env)
    | _ -> None end
  |> Option.fold ~none:[completion_item ~range ~kind:CompletionItemKind.Color "Failing state here"] ~some:(fun state_num ->
      let tokens = Cobol_parser.Expect.next_symbol_of_state state_num in
      Lsp_io.log_info "Requested with num: %d" state_num;
    Lsp_io.log_info "List is [%a]\n" (Fmt.list ~sep:(Fmt.any ";") pp_completion_entry) tokens;
    (List.flatten @@ List.map (function
      | QualifiedRef ->
          map_to_completion_item
            ~kind:CompletionItemKind.Variable ~range
            (qualname_proposals ~filename pos group)
      | ProcedureRef ->
          map_to_completion_item
          ~kind:CompletionItemKind.Module ~range
          (procedure_proposals ~filename pos group)
      | K token -> [Cobol_parser.Printer.print_token token |> completion_item ~kind:CompletionItemKind.Keyword ~range])
    tokens) @ [completion_item ~range ~kind:CompletionItemKind.Color "Empty tokens list here"])

let completion_items (doc:Lsp_document.t) (pos:Position.t) ast =
  let text = doc.textdoc in
  let filename = Lsp.Uri.to_path (Lsp.Text_document.documentUri text) in
  let range = range pos text in
  let names = name_proposals ast ~filename pos in
  let keywords = keyword_proposals ast pos in
  let keywordsItem = List.map (completion_item ~kind:CompletionItemKind.Keyword ~range) keywords in
  let namesItem = List.map (completion_item ~kind:CompletionItemKind.Method ~range) names in
    keywordsItem @ namesItem
