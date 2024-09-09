type ref_value =
  | Reference of
      { prefix : string;
        var : string
      }
  | Value of
      { prefix : string;
        var : string
      }

type error_treatment =
  { prefix : string;
    not_found_whenever : whenever_continuation option;
    sql_warning_whenever : whenever_continuation option;
    sql_error_whenever : whenever_continuation option
  }

and whenever_continuation =
  | Continue
  | Perform of string
  | Goto of string

type declaration =
  | Simple_var_declaration of
      { prefix : string;
        var_importance : string;
        var_name : string option;
        var_type : string;
        var_content : string option
      }
  | Field_var_declaration of
      { prefix : string;
        var_importance : string;
        var_name : string;
        field : declaration list
      }

type trans_stm =
  | CallStatic of
      { prefix : string;
        fun_name : string;
        ref_value : ref_value list (*Can be empty*)
      }
  | Copy of
      { prefix : string;
        file_name : string
      }
  | GotoStatement of
      { prefix : string;
        target : string
      }
  | PerformStatement of
      { prefix : string;
        target : string
      }
  | If of
      { prefix : string;
        condition : string;
        if_stm : trans_stm list
      }
  | IfElse of
      { prefix : string;
        condition : string;
        if_stm : trans_stm list;
        else_stm : trans_stm list
      }
  | Move of
      { prefix : string;
        src : string;
        dest : string
      }
  | Declaration of declaration
  | Comment of { content : string }
  | Section of { name : string }
  | LinkageSection
  | WorkingStorageSection
  | ProcedureDivision
  (*these type are for debug*)
  | NonFatalErrorWarning of { content : string }
  | Todo of { prefix : string }

type generated_stm =
  | NoChange of { content : string }
  | Added of
      { content : trans_stm list;
        error_treatment : error_treatment option;
        with_dot : bool
      }
  | Change of
      { old_stms : string list;
        trans_stm : trans_stm list;
        error_treatment : error_treatment option;
        with_dot : bool
      }

type generated = generated_stm list

module Printer = struct
  (*TODO: a function that cut (with &) the resquest if too long*)

  let rec pp fmt gen =
    match gen with
    | h :: t -> Format.fprintf fmt "%a%a" pp_gene h pp t
    | [] -> ()

  and pp_gene fmt x =
    match x with
    | NoChange { content } -> Format.fprintf fmt "%s\n" content
    | Added { content; error_treatment; with_dot } ->
      let dot =
        if with_dot then
          "."
        else
          ""
      in
      Format.fprintf fmt "%a%a%s\n" pp_trans_stm content pp_error_treatment
        error_treatment dot
    | Change { old_stms; trans_stm; error_treatment; with_dot } ->
      let dot =
        if with_dot then
          "."
        else
          ""
      in
      Format.fprintf fmt "%a\n%a%a%s\n" pp_old_stms old_stms pp_trans_stm
        trans_stm pp_error_treatment error_treatment dot

  and pp_old_stms fmt x =
    match x with
    | h :: t -> Format.fprintf fmt "      *> REMOVED: %s\n%a" h pp_old_stms t
    | [] -> ()

  and pp_trans_stm fmt x =
    match x with
    | [ h ] -> Format.fprintf fmt "%a" pp_trans_stm_aux h
    | h :: t -> Format.fprintf fmt "%a\n%a" pp_trans_stm_aux h pp_trans_stm t
    | [] -> ()

  and pp_trans_stm_aux fmt x =
    match x with
    | Section { name } -> Format.fprintf fmt "       %s." name
    | Comment { content } -> Format.fprintf fmt "ADDED *%s" content
    | CallStatic { prefix; fun_name; ref_value } ->
      Format.fprintf fmt "%sCALL STATIC \"%s\"%a%sEND-CALL" prefix fun_name
        pp_ref_value_list ref_value prefix
    | Copy { prefix; file_name } ->
      Format.fprintf fmt "%sCOPY %s" prefix file_name
    | GotoStatement { prefix; target } ->
      Format.fprintf fmt "%sGO TO %s." prefix target
    | PerformStatement { prefix; target } ->
      Format.fprintf fmt "%sPERFORM %s" prefix target
    | If { prefix; condition; if_stm } ->
      Format.fprintf fmt "%sIF %s THEN\n%a\n%sEND-IF" prefix condition
        pp_trans_stm if_stm prefix
    | IfElse { prefix; condition; if_stm; else_stm } ->
      Format.fprintf fmt "%sIF %s THEN\n%a\n%sELSE\n%a\n%sEND-IF" prefix
        condition pp_trans_stm if_stm prefix pp_trans_stm else_stm prefix
    | Move { prefix; src; dest } ->
      Format.fprintf fmt "%sMOVE '%s' TO %s" prefix src dest
    | Declaration d -> Format.fprintf fmt "%a" pp_declaration d
    | LinkageSection -> Format.fprintf fmt "       LINKAGE SECTION."
    | WorkingStorageSection ->
      Format.fprintf fmt "       WORKING-STORAGE SECTION."
    | ProcedureDivision -> Format.fprintf fmt "       PROCEDURE DIVISION."
    | NonFatalErrorWarning { content } ->
      Format.fprintf fmt "      *> WARNING: %s" content
    | Todo { prefix } -> Format.fprintf fmt "%sTODO" prefix


    (*TODO: maybe redo this, but nicer*)
  and  split_line max_length line =
    let rec aux acc max_length current_line =
      if String.length current_line <= max_length then
        List.rev (current_line :: acc)
      else
        let part = String.sub current_line 0 max_length in
        let rest =
          String.sub current_line max_length
            (String.length current_line - max_length)
        in
        aux ((part ^ "\"\n        &  \"") :: acc) 59 rest (*72 (character limit) - 12 (size of prefix '        &  "' ) - 1 (for the '"')*)
    in
    aux [] max_length line

  and pp_declaration fmt = function
    | Simple_var_declaration
        { prefix; var_importance; var_name; var_type; var_content } ->
      let var_name =
        match var_name with
        | Some n -> n
        | None -> "FILLER"
      in
      let var_content =
        match var_content with
        | Some n -> "VALUE " ^ n
        | None -> ""
      in
      let line =
        Printf.sprintf "%s%s %s PIC %s %s." prefix var_importance var_name
          var_type var_content
      in
      let lines = split_line 71 line in (*72 (character limit) - 1 (for the '"')*)
      List.iter (Format.fprintf fmt "%s") lines
    | Field_var_declaration { prefix; var_importance; var_name; field } ->
      Format.fprintf fmt "%s%s %s.%a" prefix var_importance var_name pp_field
        field

  and pp_field fmt x =
    match x with
    | h :: t -> Format.fprintf fmt "\n%a%a" pp_declaration h pp_field t
    | [] -> Format.fprintf fmt ""

  and pp_error_treatment fmt = function
    | Some
        { prefix; not_found_whenever; sql_warning_whenever; sql_error_whenever }
      -> begin
      let print_continuation fmt continuation =
        match continuation with
        | Continue -> Format.fprintf fmt "   CONTINUE"
        | Perform sqlVarToken -> Format.fprintf fmt "   PERFORM %s" sqlVarToken
        | Goto sqlVarToken -> Format.fprintf fmt "   GO TO %s." sqlVarToken
      in
      let print_error fmt (not_found_whenever, str) =
        match not_found_whenever with
        | Some continuation ->
          Format.fprintf fmt "\n%s%s\n%s%a" prefix str prefix print_continuation
            continuation
        | None ->
          Format.fprintf fmt "\n%s%s\n%s%s" prefix str prefix "   CONTINUE"
      in
      Format.fprintf fmt "\n%sEVALUATE TRUE%a%a%a\n%sEND-EVALUATE" prefix
        print_error
        (not_found_whenever, "WHEN SQLCODE = 100")
        print_error
        (sql_warning_whenever, "WHEN SQLCODE = 1")
        print_error
        (sql_error_whenever, "WHEN SQLCODE < 0")
        prefix
    end
    | None -> ()

  and pp_ref_value_list fmt x =
    let rec pp_ref_value_list_aux fmt x =
      match x with
      | h :: t ->
        Format.fprintf fmt "%a\n%a" pp_ref_value h pp_ref_value_list_aux t
      | [] -> ()
    in
    match x with
    | [] -> Format.fprintf fmt "\n"
    | _ -> Format.fprintf fmt " USING\n%a" pp_ref_value_list_aux x

  and pp_ref_value fmt x =
    match x with
    | Reference { prefix; var } ->
      Format.fprintf fmt "%sBY REFERENCE %s" prefix var
    | Value { prefix; var } -> Format.fprintf fmt "%sBY VALUE %s" prefix var
end