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

(** Type-checking and validation of COBOL compilation groups *)

module DIAGS = Cobol_common.Diagnostics

let analyze_compilation_group
    (type m) : ?config: _ -> m Cobol_parser.Outputs.parsed_compilation_group -> _ =
  fun ?(config = Cobol_config.default) ->
  function
  | Only None | WithArtifacts (None, _) ->
      DIAGS.result (Cobol_unit.Group.empty, None)
  | Only Some cg | WithArtifacts (Some cg, _) ->
      match Typeck_units.of_compilation_group config cg with
      (* | { diags; _ } when DIAGS.Set.has_errors diags -> *)
      (*     DIAGS.result ~diags (Cobol_unit.Group.empty, Some cg) *)
      | { diags; result } ->
          DIAGS.result ~diags (result, Some cg)
