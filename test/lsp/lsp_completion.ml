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

open EzCompat                                                    (* StringMap *)
open Lsp.Types
open Lsp_testing

let complete_positions (doc, positions) : string -> unit =
  let { end_with_postproc; projdir }, server = make_lsp_project () in
  let server, prog = add_cobol_doc server ~projdir "prog.cob" doc in
  (* let doc = LSP.Server.find_document prog server in *)
  let location_as_srcloc = new srcloc_resuscitator_cache in
  let completions_at_position ?key (position: Position.t) =
    let location =
      let range = Range.create ~start:position ~end_:position in
      Location.create ~range ~uri:prog.uri
    in
    let params = CompletionParams.create ~position ~textDocument:prog () in
    Pretty.out "%a%a(line %d, character %d): "
      location_as_srcloc#pp location
      Fmt.(option ~none:nop (string ++ sp)) key
      position.line position.character;
    match LSP.Request.complete server params with
    | None ->
        Pretty.error "Failed completion@."
    | Some `CompletionList { items; _ } when items == [] ->
        Pretty.error "Empty completion list@."
    | Some `CompletionList { items; _ } ->
        Pretty.out "List of completions: ";
        List.iter (fun (item: CompletionItem.t) ->
          Pretty.out "%s, " item.label) items;
        Pretty.out "\n";

  in
  StringMap.iter (fun n p -> completions_at_position ~key:n p) positions.pos_map;
  List.iter (fun p -> completions_at_position p) positions.pos_anonymous;
  end_with_postproc
;;


let%expect_test "basic-completion" =
  let end_with_postproc = complete_positions @@ extract_position_markers {cobol|
        _|_IDENTIFICATION D_|_IVISION_|_._|_
        PROGRAM_|_-ID. prog.
        DATA DIV_|_ISION.
        WORKING-STORAGE _|_SECTION.
        01 _|_DATA-NAME _|_PIC X.
        _|_PROCEDU_|_RE _|_DIVISION.
          _|_DISPLAY _|_DATA-NAME
          _|_STOP _|_RUN._|_
  |cobol} in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    __rootdir__/prog.cob:2.8:
       1
       2 >         IDENTIFICATION DIVISION.
    ----           ^
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
    {"params":{"message":"Requested with num: 0","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"message":"List is [PROGRAM_ID;INTERFACE_ID;IDENTIFICATION;ID;FUNCTION_ID;CONTROL;CLASS_ID]\n","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    (line 1, character 8): List of completions: PROGRAM_ID, INTERFACE_ID, IDENTIFICATION, ID, FUNCTION_ID, CONTROL, CLASS_ID, Empty tokens list here,
    __rootdir__/prog.cob:2.24:
       1
       2 >         IDENTIFICATION DIVISION.
    ----                           ^
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
    {"params":{"message":"Requested with num: 127","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"message":"List is [DIVISION]\n","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    (line 1, character 24): List of completions: DIVISION, Empty tokens list here,
    __rootdir__/prog.cob:2.31:
       1
       2 >         IDENTIFICATION DIVISION.
    ----                                  ^
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
    {"params":{"message":"Requested with num: 127","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"message":"List is [DIVISION]\n","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    (line 1, character 31): List of completions: DIVISION, Empty tokens list here,
    __rootdir__/prog.cob:2.32:
       1
       2 >         IDENTIFICATION DIVISION.
    ----                                   ^
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
    {"params":{"message":"Requested with num: 127","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"message":"List is [DIVISION]\n","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    (line 1, character 32): List of completions: DIVISION, Empty tokens list here,
    __rootdir__/prog.cob:3.15:
       1
       2           IDENTIFICATION DIVISION.
       3 >         PROGRAM-ID. prog.
    ----                  ^
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
    {"params":{"message":"Requested with num: 129","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"message":"List is [SECURITY;REMARKS;INSTALLATION;DATE_WRITTEN;DATE_MODIFIED;DATE_COMPILED;AUTHOR]\n","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    (line 2, character 15): List of completions: SECURITY, REMARKS, INSTALLATION, DATE_WRITTEN, DATE_MODIFIED, DATE_COMPILED, AUTHOR, Empty tokens list here,
    __rootdir__/prog.cob:4.16:
       1
       2           IDENTIFICATION DIVISION.
       3           PROGRAM-ID. prog.
       4 >         DATA DIVISION.
    ----                   ^
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
    {"params":{"message":"Requested with num: 2001","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"message":"List is [DIVISION]\n","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    (line 3, character 16): List of completions: DIVISION, Empty tokens list here,
    __rootdir__/prog.cob:5.24:
       2           IDENTIFICATION DIVISION.
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
       5 >         WORKING-STORAGE SECTION.
    ----                           ^
       6           01 DATA-NAME PIC X.
       7           PROCEDURE DIVISION.
    {"params":{"message":"Requested with num: 2008","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"message":"List is [SECTION]\n","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    (line 4, character 24): List of completions: SECTION, Empty tokens list here,
    __rootdir__/prog.cob:6.11:
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6 >         01 DATA-NAME PIC X.
    ----              ^
       7           PROCEDURE DIVISION.
       8             DISPLAY DATA-NAME
    {"params":{"message":"Requested with num: 826","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"message":"List is [FILLER]\n","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    (line 5, character 11): List of completions: FILLER, Empty tokens list here,
    __rootdir__/prog.cob:6.21:
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6 >         01 DATA-NAME PIC X.
    ----                        ^
       7           PROCEDURE DIVISION.
       8             DISPLAY DATA-NAME
    {"params":{"message":"Requested with num: 116","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"message":"List is []\n","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    (line 5, character 21): List of completions: Empty tokens list here,
    __rootdir__/prog.cob:7.8:
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
       7 >         PROCEDURE DIVISION.
    ----           ^
       8             DISPLAY DATA-NAME
       9             STOP RUN.
    {"params":{"message":"Requested with num: 1766","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"message":"List is []\n","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    (line 6, character 8): List of completions: Empty tokens list here,
    __rootdir__/prog.cob:7.15:
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
       7 >         PROCEDURE DIVISION.
    ----                  ^
       8             DISPLAY DATA-NAME
       9             STOP RUN.
    {"params":{"message":"Requested with num: 1766","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"message":"List is []\n","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    (line 6, character 15): List of completions: Empty tokens list here,
    __rootdir__/prog.cob:7.18:
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
       7 >         PROCEDURE DIVISION.
    ----                     ^
       8             DISPLAY DATA-NAME
       9             STOP RUN.
    {"params":{"message":"Requested with num: 2527","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"message":"List is [DIVISION]\n","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    (line 6, character 18): List of completions: DIVISION, Empty tokens list here,
    __rootdir__/prog.cob:8.10:
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
       7           PROCEDURE DIVISION.
       8 >           DISPLAY DATA-NAME
    ----             ^
       9             STOP RUN.
      10
    {"params":{"message":"Requested with num: 4545","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"message":"List is [DECLARATIVES]\n","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    (line 7, character 10): List of completions: DECLARATIVES, Empty tokens list here,
    __rootdir__/prog.cob:8.18:
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
       7           PROCEDURE DIVISION.
       8 >           DISPLAY DATA-NAME
    ----                     ^
       9             STOP RUN.
      10
    {"params":{"message":"Requested with num: 3540","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"message":"List is [QualifiedRef;ZERO;SUPER;SPACE;SELF;QUOTE;PAGE_COUNTER;NULL;LOW_VALUE;LINE_COUNTER;LINAGE_COUNTER;HIGH_VALUE;FUNCTION;EXCEPTION_OBJECT;ALL;ADDRESS]\n","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    (line 7, character 18): List of completions: DATA-NAME, ZERO, SUPER, SPACE, SELF, QUOTE, PAGE_COUNTER, NULL, LOW_VALUE, LINE_COUNTER, LINAGE_COUNTER, HIGH_VALUE, FUNCTION, EXCEPTION_OBJECT, ALL, ADDRESS, Empty tokens list here,
    __rootdir__/prog.cob:9.10:
       6           01 DATA-NAME PIC X.
       7           PROCEDURE DIVISION.
       8             DISPLAY DATA-NAME
       9 >           STOP RUN.
    ----             ^
      10
    {"params":{"message":"Requested with num: 116","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"message":"List is []\n","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    (line 8, character 10): List of completions: Empty tokens list here,
    __rootdir__/prog.cob:9.15:
       6           01 DATA-NAME PIC X.
       7           PROCEDURE DIVISION.
       8             DISPLAY DATA-NAME
       9 >           STOP RUN.
    ----                  ^
      10
    {"params":{"message":"Requested with num: 2811","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"message":"List is [QualifiedRef;ZERO;THREAD;SPACE;RUN;QUOTE;LOW_VALUE;HIGH_VALUE;ERROR;ALL]\n","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    (line 8, character 15): List of completions: DATA-NAME, ZERO, THREAD, SPACE, RUN, QUOTE, LOW_VALUE, HIGH_VALUE, ERROR, ALL, Empty tokens list here,
    __rootdir__/prog.cob:9.19:
       6           01 DATA-NAME PIC X.
       7           PROCEDURE DIVISION.
       8             DISPLAY DATA-NAME
       9 >           STOP RUN.
    ----                      ^
      10
    {"params":{"message":"Requested with num: 2811","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"message":"List is [QualifiedRef;ZERO;THREAD;SPACE;RUN;QUOTE;LOW_VALUE;HIGH_VALUE;ERROR;ALL]\n","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    (line 8, character 19): List of completions: DATA-NAME, ZERO, THREAD, SPACE, RUN, QUOTE, LOW_VALUE, HIGH_VALUE, ERROR, ALL, Empty tokens list here, |}];;

let%expect_test "datadiv-completion" =
  let end_with_postproc = complete_positions @@ extract_position_markers {cobol|
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIV_|_ISION.
        WORKING-STORAGE SECTION.
        01 _|_DATA-NAME _|_PIC X.
        _|_01 VAR PICTURE _|_X _|_USAGE _|_DISPLAY
        PROCEDURE DIVISION.
          DISPLAY DATA-NAME
          STOP RUN.
  |cobol} in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[{"message":"Missing .","range":{"end":{"character":38,"line":6},"start":{"character":38,"line":6}},"severity":4}],"uri":"file://__rootdir__/prog.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    __rootdir__/prog.cob:4.16:
       1
       2           IDENTIFICATION DIVISION.
       3           PROGRAM-ID. prog.
       4 >         DATA DIVISION.
    ----                   ^
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
    {"params":{"message":"Requested with num: 2001","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"message":"List is [DIVISION]\n","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    (line 3, character 16): List of completions: DIVISION, Empty tokens list here,
    __rootdir__/prog.cob:6.11:
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6 >         01 DATA-NAME PIC X.
    ----              ^
       7           01 VAR PICTURE X USAGE DISPLAY
       8           PROCEDURE DIVISION.
    {"params":{"message":"Requested with num: 826","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"message":"List is [FILLER]\n","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    (line 5, character 11): List of completions: FILLER, Empty tokens list here,
    __rootdir__/prog.cob:6.21:
       3           PROGRAM-ID. prog.
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6 >         01 DATA-NAME PIC X.
    ----                        ^
       7           01 VAR PICTURE X USAGE DISPLAY
       8           PROCEDURE DIVISION.
    {"params":{"message":"Requested with num: 116","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"message":"List is []\n","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    (line 5, character 21): List of completions: Empty tokens list here,
    __rootdir__/prog.cob:7.8:
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
       7 >         01 VAR PICTURE X USAGE DISPLAY
    ----           ^
       8           PROCEDURE DIVISION.
       9             DISPLAY DATA-NAME
    {"params":{"message":"Requested with num: 1766","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"message":"List is []\n","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    (line 6, character 8): List of completions: Empty tokens list here,
    __rootdir__/prog.cob:7.23:
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
       7 >         01 VAR PICTURE X USAGE DISPLAY
    ----                          ^
       8           PROCEDURE DIVISION.
       9             DISPLAY DATA-NAME
    {"params":{"message":"Requested with num: 1640","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"message":"List is [IS]\n","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    (line 6, character 23): List of completions: IS, Empty tokens list here,
    __rootdir__/prog.cob:7.25:
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
       7 >         01 VAR PICTURE X USAGE DISPLAY
    ----                            ^
       8           PROCEDURE DIVISION.
       9             DISPLAY DATA-NAME
    {"params":{"message":"Requested with num: 1642","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"message":"List is [LOCALE]\n","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    (line 6, character 25): List of completions: LOCALE, Empty tokens list here,
    __rootdir__/prog.cob:7.31:
       4           DATA DIVISION.
       5           WORKING-STORAGE SECTION.
       6           01 DATA-NAME PIC X.
       7 >         01 VAR PICTURE X USAGE DISPLAY
    ----                                  ^
       8           PROCEDURE DIVISION.
       9             DISPLAY DATA-NAME
    {"params":{"message":"Requested with num: 1376","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    {"params":{"message":"List is [IS]\n","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    (line 6, character 31): List of completions: IS, Empty tokens list here, |}]
