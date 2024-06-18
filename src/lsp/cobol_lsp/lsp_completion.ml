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

let to_completion_item ~kind ~range qualnames =
  List.flatten @@ List.map (function
    | Cobol_ptree.Name name -> [~&name]
    | Qual (name,_) as qualname ->
        [~&name; Pretty.to_string "%a" Cobol_ptree.pp_qualname qualname]
    | _ -> []) qualnames |>
    List.map @@ completion_item ~kind ~range

let map_completion_items ~(range:Range.t) ~group ~filename comp_entries =
      let pos = range.end_ in
      if List.length comp_entries < 10 then
        Lsp_io.log_debug "Comp entries are [%a]\n" (Fmt.list ~sep:(Fmt.any ";") Expect.pp_completion_entry) comp_entries
      else
        Lsp_io.log_debug "====> Comp entries are %d <====\n" (List.length comp_entries);
      List.flatten @@ List.map (function
        | Expect.QualifiedRef ->
            to_completion_item
              ~kind:CompletionItemKind.Variable ~range
              (qualname_proposals ~filename pos group)
        | ProcedureRef ->
            to_completion_item
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

let pp_state ppf state = (* for debug *)
  let has_default = try let _ = Expect.get_default_nonterminal_produced state in true with _ -> false in
  Fmt.pf ppf "%d%s" (Expect.state_to_int state) (if has_default then "_" else "")

let expected_tokens env =
  let rec get_stack env =
    let state = Expect.state_of_int (Menhir.current_state_number env) in
    let has_default = try let _ = Expect.get_default_nonterminal_produced state in true with _ -> false in
    match Menhir.pop env with
        | None -> [(state, has_default)]
        | Some env -> (state, has_default)::(get_stack env) in
  let rec inner env_stack acc =
    match env_stack with
      | [] -> []
      | (state, false)::_tl ->
          Lsp_io.log_debug "State without default: %a" pp_state state;
          Expect.transition_tokens state @ acc
      | (state, true)::_tl ->
          Lsp_io.log_debug "State with default: %a" pp_state state;
          let acc = Expect.transition_tokens state @ acc in
          let defaults = Expect.get_default_nonterminal_produced state in
          List.fold_left (fun acc (rhslen, lhs) ->
            let _, states = listpop env_stack rhslen in
            match List.hd states with
              | (transition_state, _) ->
                  let next = Expect.follow_transition transition_state lhs in
                  Lsp_io.log_debug "From %a following %a -> %a" pp_state state pp_state transition_state pp_state (fst next);
                  inner (next::states) acc
              | exception Failure _ ->
                  Lsp_io.log_warn "Had to pop too many states during context completion [%a]"
                  (Fmt.list ~sep:(Fmt.any " ") Fmt.int)
                  (List.map (fun (s, _) -> Expect.state_to_int s) env_stack);
                  acc)
          acc defaults in
  Lsp_io.log_debug "%a" Fmt.(list ~sep:(any " ") (fun ppf (i,d) ->
    Fmt.pf ppf "%d%s" (Expect.state_to_int i) (if d then "T" else ""))) (get_stack env);
  inner (get_stack env) []

let context_completion_items (doc:Lsp_document.t) Cobol_typeck.Outputs.{ group; _ } (pos:Position.t) =
  let filename = Lsp.Uri.to_path (Lsp.Text_document.documentUri doc.textdoc) in
  let range = range pos doc.textdoc in
  begin match Lsp_document.inspect_at ~position:(range.start) doc with
    | Some Env env -> map_completion_items ~range ~group ~filename (expected_tokens env)
    | _ -> [] end
