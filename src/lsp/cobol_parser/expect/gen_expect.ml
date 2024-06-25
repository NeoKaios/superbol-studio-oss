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

module NEL = Cobol_common.Basics.NEL

let cmlyname = ref None
let external_tokens = ref ""

let usage_msg = Fmt.str "%s [OPTIONS] file.cmly" Sys.argv.(0)
let anon str = match !cmlyname with
  | None -> cmlyname := Some str
  | Some _ -> raise @@ Arg.Bad "Only one anonymous argument may be given"

let () =
  Arg.parse
    Arg.[
      ("--external-tokens", Set_string external_tokens,
       "<module> Import token type definition from <module>");
    ]
    anon usage_msg

let cmlyname = match !cmlyname with
  | None | Some "" -> Fmt.epr "%s@." usage_msg; exit 1
  | Some s -> s

(* --- UTILS --- *)

let inv f a b = f b a

(** Find all keys that have the same value and merge them into a single key_list entry *)
let sort_and_merge compare equal l = l |>
  List.sort (fun (_, value1) (_, value2) ->
    compare value1 value2)
        |>
  List.fold_left (fun acc (key, value) -> begin
    match acc with
      | (prev_keys, prev_value)::t
        when equal value prev_value ->
          (key::prev_keys, prev_value)::t
      | _ -> ([key], value)::acc
  end) []

let pp_brackets pp =
  Fmt.(any "[" ++ pp ++ any "]")

let pp_pair pp_a pp_b = fun ppf (a, b) ->
    Fmt.pf ppf "(%a,%a)" pp_a a pp_b b

let pp_list pp = Fmt.list ~sep:(Fmt.any ";") pp

let rec pp_nel pp ppf = function
  | NEL.One v -> Fmt.pf ppf "One %a" pp v
  | v::nel -> Fmt.pf ppf "%a::" pp v; pp_nel pp ppf nel

let pp_match_cases ppf pp_key pp_value default l =
  List.iter (fun (keys, value) ->
    Fmt.pf ppf "  | %a -> %a\n"
      Fmt.(list ~sep:(any " | ") pp_key) keys
      pp_value value) l;
  Fmt.pf ppf "  | _ -> %s\n" default

(* --- *)

include MenhirSdk.Cmly_read.Read (struct let filename = cmlyname end)

type completion_entry =
  | K of terminal NEL.t
  | Custom of string
(* [@@deriving ord] *)

let completion_entry_equal entry1 entry2 =
  match entry1, entry2 with
  | Custom c1, Custom c2 -> c1 == c2
  | K nel1, K nel2 -> NEL.equal Terminal.equal nel1 nel2
  | _ -> false

let completion_entry_compare entry1 entry2 =
  match entry1, entry2 with
    | Custom s1, Custom s2 -> String.compare s2 s1
    | K nel1, K nel2 -> NEL.compare Terminal.compare nel1 nel2
    | Custom _, K _ -> -1
    | K _, Custom _ -> 1

let pp_completion_entry: completion_entry Fmt.t = fun ppf -> function
  | K list -> Fmt.pf ppf "K (%a)" (pp_nel Print.terminal) list
  | Custom custom_type -> Fmt.pf ppf "%s" custom_type


let terminal_condition: terminal -> bool = fun term ->
  Terminal.kind term == `REGULAR &&
  Terminal.attributes term |>
  List.exists (fun attrib ->
    Attribute.has_label "keyword" attrib ||
    Attribute.has_label "completion" attrib ||
    Attribute.has_label "keyword.combined" attrib)

let terminal_filter_map: terminal -> completion_entry option = fun term ->
  if terminal_condition term then Some (K (NEL.One term)) else None

let nonterminal_filter_map: nonterminal -> completion_entry option = fun nonterm ->
  Nonterminal.attributes nonterm |>
  List.find_opt (Attribute.has_label "completion") |>
  Option.map Attribute.payload |>
  fun s -> Option.bind s (fun s -> Some (Custom s))

let pp_xsymbol_of_nt ppf nonterminal =
  Fmt.pf ppf "X(N N_%s)" (Nonterminal.mangled_name nonterminal)

(* --- *)

module SymbolSet = Set.Make(struct
  type t = symbol
  let compare = Symbol.compare
end)

module CompEntrySet = struct
  include Set.Make(struct
    type t = completion_entry
    let compare = completion_entry_compare
  end)
  let pp _ppf = ()
end

module Map5 (OrderedType : Map.OrderedType) = struct
  include Map.Make(OrderedType)

  let add_to_list x data m =
    let add = function None -> Some [data] | Some l -> Some (data :: l) in
    update x add m
end

module CompEntryMap = Map5(struct
  type t = CompEntrySet.t
  let compare = CompEntrySet.compare
end)

module DefaultNTMap = Map5(struct
  type t = (int * nonterminal) list
  let compare = List.compare (fun (i, nt) (i2, nt2) ->
    match i-i2 with 0 -> Nonterminal.compare nt nt2 | diff -> diff)
end)

module ProdMap = Map5(struct
  type t = Production.t list
  let compare = List.compare (fun p1 p2 ->
    Production.to_int p1 - Production.to_int p2)
end)

module SymbolMap = Map5(struct
  type t = SymbolSet.t
  let compare = SymbolSet.compare
end)

(* --- *)

let completion_entry_t = "completion_entry"
let state_t = "state"
let nonterminal_t = "nonterminal"

let emit_pp_completion_entry ppf custom_types = (* For debug *)
  Fmt.pf ppf "\nlet pp_%s: %s Fmt.t = fun ppf -> function\n"
  completion_entry_t completion_entry_t;
  Fmt.pf ppf "  | K tokens -> Fmt.pf ppf \"%%a\" (Fmt.list ~sep:Fmt.sp Fmt.string) (List.map Grammar_printer.print_token @@@@ NEL.to_list tokens)\n";
  List.iter
  (fun s -> Fmt.pf ppf "  | %s -> Fmt.pf ppf \"%s\"\n" s s)
  custom_types

let emit_complentryset ppf custom_types =
  Fmt.(pf ppf {|
module CompEntrySet = Set.Make(struct
  type t = completion_entry
  let compare ce1 ce2 =
    let to_int = function
      %a| K _ -> %d in
    match ce1, ce2 with
      | K nel1, K nel2 -> NEL.compare Stdlib.compare nel2 nel1
      | _ -> to_int ce1 - to_int ce2
end)
|}
  (list ~sep:nop (fun ppf (i,t) ->
    pf ppf "| %s -> %d\n    " t i)) (List.mapi (fun i v -> (i,v)) custom_types)
  (List.length custom_types))

let emit_completion_entry ppf =
  Fmt.pf ppf "type %s =\n" completion_entry_t;
  Fmt.pf ppf "  | K of token NEL.t\n";
  let custom_types = Nonterminal.fold (fun nonterm acc ->
    match nonterminal_filter_map nonterm with
    | Some (Custom s) -> (Fmt.pf ppf "  | %s\n" s; s::acc)
    | _ -> acc) [] in
  emit_pp_completion_entry ppf custom_types;
  emit_complentryset ppf custom_types

let emit_types ppf =
  Fmt.pf ppf {|
module type TYPES = sig
  type %s = private int
  val state_to_int: state -> int
  val state_of_int: int -> state
  type %s = private int
  val nonterminal_to_int: nonterminal -> int
  val nonterminal_of_int: int -> nonterminal
end

module TYPES: TYPES = struct
  type state = int
  let state_to_int state = state
  let state_of_int state = state
  type nonterminal = int
  let nonterminal_to_int nt = nt
  let nonterminal_of_int nt = nt
end

include TYPES
|}
  state_t nonterminal_t;
  Fmt.cut ppf ();
  ()

let nullable_nonterminals lr1 =
  let lr0 = Lr1.lr0 lr1 in
  List.fold_left (fun acc (prod, idx) ->
    let rhs = Production.rhs prod in
    match rhs.(idx) with
      | N nt, _, _ when Nonterminal.nullable nt -> SymbolSet.add (N nt) acc
      | _ | exception Invalid_argument _ -> acc)
  SymbolSet.empty (Lr0.items lr0)

let reducable_productions lr1 =
  List.sort_uniq Stdlib.compare @@
  match Lr1.default_reduction lr1 with (* TODO check if this is needed *)
  | Some prod -> [prod]
  | None -> begin
    List.fold_left (fun acc (prod, idx) -> (* TODO change to filtermap *)
      if Array.length @@ Production.rhs prod == idx
      then prod::acc else acc)
    [] (Lr0.items @@ Lr1.lr0 lr1) end

let emit_accaptable_nullable_nonterminals_in_env ppf =
  Fmt.pf ppf {|
let accaptable_nullable_nonterminals_in ~env : Menhir.xsymbol list =
  Menhir.(match current_state_number env with
|};
  Lr1.fold (fun lr1 acc ->
    match nullable_nonterminals lr1 with
    | emtpy when SymbolSet.is_empty emtpy -> acc
    | symbols -> SymbolMap.add_to_list symbols lr1 acc)
  SymbolMap.empty
  |> inv (SymbolMap.fold (fun s l acc -> (l,
      List.map (function  N nt -> nt | T _ -> failwith "impossible" ) @@ SymbolSet.elements s)::acc)) []
  |> pp_match_cases ppf
    Fmt.(using Lr1.to_int int)
    (pp_brackets @@ pp_list @@ pp_xsymbol_of_nt)
    "[])"

let emit_reducable_productions_in_env ppf =
  Fmt.pf ppf {|
let reducable_productions_in ~env : Menhir.production list =
  List.rev_map Menhir.find_production @@@@
  match Menhir.current_state_number env with
|};
  Lr1.fold (fun lr1 acc ->
    match reducable_productions lr1 with
    | [] -> acc
    | defaults -> ProdMap.add_to_list defaults lr1 acc)
  ProdMap.empty
  |> inv (ProdMap.fold (fun s l acc -> (l, s)::acc)) []
  |> pp_match_cases ppf
    Fmt.(using Lr1.to_int int)
    Fmt.(pp_brackets @@ pp_list @@ (using Production.to_int int))
    "[]"

let emit_follow_transition ppf = (* OLD, may be removed ? *)
  Fmt.pf ppf {|
(** For a given state and nonterminal,
  returns the state the automaton will be in after taking the transition *)
let follow_transition: %s -> %s -> %s = fun state nt ->
  let state = state_to_int state in
  let nt = nonterminal_to_int nt in
  state_of_int @@@@ match state, nt with
|} state_t nonterminal_t state_t;
  Lr1.fold (fun lr1 acc -> begin
    List.fold_left (fun acc (symbol, next_state) ->
      match symbol with
      | T _ -> acc
      | N nt -> ((Lr1.to_int lr1, Nonterminal.to_int nt),
          Lr1.to_int next_state)::acc
      ) acc (Lr1.transitions lr1)
  end) []
  |> sort_and_merge Int.compare Int.equal
  |> pp_match_cases ppf
    Fmt.(pp_pair int int)
    Fmt.int
    "raise (Invalid_argument \"This state and nonterminal don't lead to any transition\")"

let completion_entries_of ~lr1 =
  let next_tokens_from ~item =
    let (prod, idx) = item in
    let rec eagerly_get_terminals rhs i l =
      match rhs.(i) with
        | T t, _, _ when terminal_condition t -> eagerly_get_terminals rhs (i+1) (t::l)
        | _ | exception Invalid_argument _ -> List.rev l
    in
      match (Production.rhs prod).(idx) with
        | N nt, _, _ -> List.filter_map terminal_filter_map (Nonterminal.first nt)
        | T _, _, _ -> begin
          match eagerly_get_terminals (Production.rhs prod) idx [] with
            | [] -> []
            | l -> [K (NEL.of_list l)] end
        | exception Invalid_argument _ -> []
  in
  Lr1.tabulate begin fun lr1 ->
    let custom_comp_entries = List.fold_left (fun acc -> function
      | N nonterm, _ -> Option.fold ~none:acc
        ~some:(inv CompEntrySet.add acc) (nonterminal_filter_map nonterm)
      | _ -> acc)
    CompEntrySet.empty (Lr1.transitions lr1) in
    List.fold_left (fun acc item ->
      CompEntrySet.(union acc (of_list @@ next_tokens_from ~item)))
    custom_comp_entries (Lr0.items @@ Lr1.lr0 lr1)
  end lr1

let emit_acceptable_terminals_in_env ppf = (* taking transitions *)
  Fmt.pf ppf {|
let acceptable_terminals_in ~env : completion_entry list =
  NEL.(match Menhir.current_state_number env with
|};
  Lr1.fold (fun lr1 acc ->
    match completion_entries_of ~lr1 with
      | s when CompEntrySet.is_empty s -> acc
      | s -> CompEntryMap.add_to_list s lr1 acc)
  CompEntryMap.empty
  |> inv (CompEntryMap.fold (fun s l acc -> (l, CompEntrySet.elements s)::acc)) []
  |> pp_match_cases ppf
    Fmt.(using Lr1.to_int int)
    (pp_brackets @@ pp_list pp_completion_entry)
    "[])"

let guess_default_value mangled_name typ =
  match typ, mangled_name with
  | _ when EzString.ends_with ~suffix:"option" typ ||
          EzString.starts_with ~prefix:"option_" mangled_name
  -> Some "None"
  | _ when EzString.ends_with ~suffix:"list" typ ||
  EzString.starts_with ~prefix:"loption_" mangled_name ||
  EzString.starts_with ~prefix:"list_" mangled_name
  -> Some "[]"
  | _ when EzString.ends_with ~suffix:"unit" typ
  -> Some "()"
  | _ when EzString.ends_with ~suffix:"bool" typ ||
  EzString.starts_with ~prefix:"boption_" mangled_name
  -> Some "false"
  | _ -> None

let is_nullable_without_recovery nt =
  Nonterminal.nullable nt &&
    Nonterminal.attributes nt
    |> List.find_opt (Attribute.has_label "recovery")
    |> Option.is_none

let default_attribute_payload nt =
  Nonterminal.attributes nt
  |> List.find_opt (Attribute.has_label "default")
  |> Option.map Attribute.payload

let emit_default_value_of_nullables ppf =
  Fmt.pf ppf {|
let guessed_default_value_of_nullables (type a): a Menhir.symbol -> a = function
  | T _ -> raise Not_found
  | N nt -> begin match nt with
|};
  Nonterminal.fold (fun nt acc -> begin
    if is_nullable_without_recovery nt
    then
      let guessed_default = Nonterminal.typ nt
        |> inv Option.bind (guess_default_value @@ Nonterminal.mangled_name nt) in
      match default_attribute_payload nt, guessed_default with
        | None, None -> acc
        | Some default, _ -> ([nt], default)::acc
        | None, Some typ -> ([nt], typ):: acc
      else acc
  end) []
  |> pp_match_cases ppf
    Fmt.(using Nonterminal.mangled_name (any "N_" ++ string))
    Fmt.string
    "raise Not_found end"

module DEBUG = struct

let emit_firsts ppf =
  Nonterminal.iter (fun nonterm ->
    let kind = if Nonterminal.kind nonterm  == `START then "start" else "" in
    let nullable = if Nonterminal.nullable nonterm then "nullable" else "" in
    Fmt.(
      pf ppf "-%a- %s %s\n\t[%a]\n\n"
      Print.nonterminal nonterm
      nullable kind
      (pp_list Print.terminal) (Nonterminal.first nonterm)))

let emit_state_productions ppf =
  Lr1.iter (fun lr1 -> begin
    let lr0 = Lr1.lr0 lr1 in
    let items = Lr0.items lr0 in
    Fmt.pf Fmt.stdout "\nState lr1-%d- lr0(%d):\n%a"
      (Lr1.to_int lr1) (Lr0.to_int lr0)
      Fmt.(option ~none:nop (any "DEFAULT-PROD: " ++ Print.production))
      (Lr1.default_reduction lr1);
    Print.itemset Fmt.stdout items;
    let reductions = Lr1.get_reductions lr1 in
    Fmt.pf ppf "TRANSITIONS [%a]\n"
      (Fmt.list ~sep:(Fmt.any "; ") (fun ppf (s, i) ->
        Fmt.pf ppf "%a(%d)" Print.symbol s (Lr1.to_int i))) (Lr1.transitions lr1);
    Fmt.pf ppf "REDUCTIONS [%a]\n"
      Fmt.(list ~sep:(any "; ") (using fst Print.terminal)) reductions;
  end)

let emit_nullable_unrecoverable ppf =
  let nt_without_default = Nonterminal.fold (fun nt acc -> begin
    if is_nullable_without_recovery nt
    && default_attribute_payload nt |> Option.is_none
    && Nonterminal.typ nt
      |> inv Option.bind (guess_default_value @@ Nonterminal.mangled_name nt)
      |> Option.is_none
    then nt::acc else acc
  end) [] in
  if nt_without_default == []
  then Fmt.string ppf "All nullable nonterminals have a recovery/default/guessed-default"
  else List.iter (fun nt ->
    Fmt.pf ppf "%s  (%s)\n"
      (Nonterminal.mangled_name nt)
      (Option.value ~default:"no typ" @@ Nonterminal.typ nt))
  nt_without_default

end


let () =
  let ppf = Fmt.stdout in
  Fmt.pf ppf
    "(* Caution: this file was automatically generated from %s; do not edit *)\
     \nmodule NEL = Cobol_common.Basics.NEL\
     \nmodule Menhir = Grammar.MenhirInterpreter\
     \nopen Grammar_tokens@\n\n"
    cmlyname;

  emit_types ppf;
  emit_completion_entry ppf;
  emit_reducable_productions_in_env ppf;
  emit_accaptable_nullable_nonterminals_in_env ppf;
  emit_acceptable_terminals_in_env ppf;
  emit_default_value_of_nullables ppf;

  (* emit_follow_transition ppf; *)
  (* DEBUG.emit_firsts ppf; *)
  (* DEBUG.emit_state_productions ppf; *)
  (* DEBUG.emit_nullable_unrecoverable ppf; *)
  ()
