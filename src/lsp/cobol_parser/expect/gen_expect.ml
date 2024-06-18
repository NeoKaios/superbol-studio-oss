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

(* --- *)

include MenhirSdk.Cmly_read.Read (struct let filename = cmlyname end)

type completion_entry =
  | K of terminal
  | Custom of string
(* [@@deriving ord] *)

let completion_entry_equal entry1 entry2 =
  match entry1, entry2 with
  | K t1, K t2 -> t1 == t2
  | Custom c1, Custom c2 -> c1 == c2
  | _ -> false

let completion_entry_compare entry1 entry2 =
  match entry1, entry2 with
    | K t1, K t2 -> Terminal.compare t1 t2
    | Custom s1, Custom s2 -> String.compare s2 s1
    | Custom _, K _ -> -1
    | K _, Custom _ -> 1

let pp_completion_entry: completion_entry Fmt.t = fun ppf -> function
  | K term -> Fmt.pf ppf "K %a" Print.terminal term
  | Custom custom_type -> Fmt.pf ppf "%s" custom_type

let terminal_filter_map: terminal -> completion_entry option = fun term ->
  if Terminal.kind term == `REGULAR &&
  Terminal.attributes term |>
  List.exists (fun attrib ->
    Attribute.has_label "keyword" attrib ||
    Attribute.has_label "keyword.combined" attrib)
  then
    Some (K term)
  else None

let nonterminal_filter_map: nonterminal -> completion_entry option = fun nonterm ->
  Nonterminal.attributes nonterm |>
  List.find_opt (Attribute.has_label "completion") |>
  Option.map Attribute.payload |>
  fun s -> Option.bind s (fun s -> Some (Custom s))

let completion_entry_t = "completion_entry"
let state_t = "state"
let nonterminal_t = "nonterminal"

let emit_pp_completion_entry ppf custom_types = (* For debug *)
  Fmt.pf ppf "\nlet pp_%s: %s Fmt.t = fun ppf -> function\n"
  completion_entry_t completion_entry_t;
  Fmt.pf ppf "  | K token -> Fmt.pf ppf \"%%s\" @@@@ Grammar_printer.print_token token\n";
  List.iter
  (fun s -> Fmt.pf ppf "  | %s -> Fmt.pf ppf \"%s\"\n" s s)
  custom_types

let emit_completion_entry ppf =
  Fmt.pf ppf "type %s =\n" completion_entry_t;
  Fmt.pf ppf "  | K of token\n";
  let custom_types = Nonterminal.fold (fun nonterm acc ->
    match nonterminal_filter_map nonterm with
    | Some (Custom s) -> (Fmt.pf ppf "  | %s\n" s; s::acc)
    | _ -> acc
  ) [] in
  emit_pp_completion_entry ppf custom_types

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

include TYPES|}
  state_t nonterminal_t;
  Fmt.cut ppf ();
  Fmt.cut ppf ();
  (* Fmt.pf ppf "let %s_of_int (state:int): %s = state \n\n" state_type_name state_type_name; *)
  ()

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


(* SHOULD BE REMOVED BEFORE MERGE *)
let emit_firsts ppf =
  Nonterminal.iter (fun nonterm ->
    let kind = if Nonterminal.kind nonterm  == `START then "start" else "" in
    let nullable = if Nonterminal.nullable nonterm then "nullable" else "" in
    Fmt.(
      pf ppf "-%a- %s %s\n\t[%a]\n\n"
      Print.nonterminal nonterm
      nullable kind
      (list ~sep:(any ";") Print.terminal) (Nonterminal.first nonterm)))

  (* SHOULD BE REMOVED BEFORE MERGE *)
let emit_lr0 ppf =
  Lr1.iter (fun lr1 -> begin
    let lr0 = Lr1.lr0 lr1 in
    let items = Lr0.items lr0 in
    Fmt.pf Fmt.stdout "\nState %d -%d-:\n" (Lr0.to_int lr0) (Lr1.to_int lr1);
    Print.itemset Fmt.stdout items;
    let items = List.filter_map (fun (prod,idx) ->
      let rhs = Production.rhs prod in
      try let (symbol, _,_) = rhs.(idx) in
      Some symbol
      with _ -> None
      ) items in
    let sorted = List.sort_uniq Symbol.compare items in
    (Fmt.list ~sep:(Fmt.any ";") Print.symbol) ppf sorted;
    if snd @@ List.fold_left (fun acc s ->
    match acc,s with
    | (true,_), N _ -> (true,true)
    | (false,_), N _ -> (true,false)
    | _ -> acc
    ) (false,false) sorted then begin
    (* if List.length sorted < List.length items then begin *)
      Fmt.string ppf "<-OO->" end;
  end)

  (* SHOULD BE REMOVED BEFORE MERGE *)
let emit_productions ppf =
  Fmt.pf ppf "(*";
  Lr1.iter (fun lr1 -> begin
    let lr0 = Lr1.lr0 lr1 in
    let items = Lr0.items lr0 in
    Fmt.pf Fmt.stdout "State %d -%d-: %a" (Lr0.to_int lr0) (Lr1.to_int lr1)
    (Fmt.option ~none:Fmt.cut Print.production) (Lr1.default_reduction lr1);
    Print.itemset Fmt.stdout items;
    let items = List.filter_map (fun (prod,idx) ->
      try let rhs = Production.rhs prod in
      let (symbol, _,_) = rhs.(idx) in
      Some symbol
    with _ -> None
      ) items in
    let from_red = Lr1.reductions lr1 in
    let from_tra = Lr1.transitions lr1 in
    Fmt.pf ppf "  | %d\t ->tra [%a]\n"
      (Lr1.to_int lr1)
      (Fmt.list ~sep:(Fmt.any ";") (fun ppf (s,i) ->
        Fmt.pf ppf "%a(%d)" Print.symbol s (Lr1.to_int i))) from_tra;
    Fmt.pf ppf "==> %a\n" (Fmt.list ~sep:(Fmt.any ";") Print.symbol) (List.sort_uniq Symbol.compare items);
      Fmt.pf ppf "  | %d\t ->red [%a]\n"
    (Lr1.to_int lr1)
    (Fmt.list ~sep:(Fmt.any ";") (fun ppf (t,pl) ->
      Fmt.pf ppf "%a <> %a" Print.terminal t (Fmt.list Print.production) pl)) from_red;
    Fmt.string ppf "\n"
end);
      Fmt.pf ppf "*)\n"

let reducable_productions lr1: production list=
  match Lr1.default_reduction lr1 with
  | Some prod -> [prod]
  | None -> begin
    let lr0 = Lr1.lr0 lr1 in
    List.fold_left (fun acc (prod, idx) ->
        let rhs = Production.rhs prod in
        if Array.length rhs == idx then prod::acc else acc)
    [] (Lr0.items lr0)
  end

let has_reducable_productions lr1: bool =
  match reducable_productions lr1 with
  | [] -> false
  | _ -> true

let emit_get_default_production ppf = (* taking reduction(if no default) and transitions *)
  Fmt.pf ppf "\nlet get_default_production: %s -> int * %s = fun state ->\n  let rhslen, lhs = match state_to_int state with\n" state_t nonterminal_t;
  Lr1.fold (fun lr1 acc -> begin
    match reducable_productions lr1 with
    | [] -> acc
    | prod::_ ->
        let rhslen = Array.length @@ Production.rhs prod in
        let lhs = Nonterminal.to_int @@ Production.lhs prod in
        (Lr1.to_int lr1, (rhslen, lhs))::acc
  end) [] |>
  sort_and_merge Stdlib.compare Stdlib.(=) |>
  List.iter (fun (states,(rhslen, lhs)) ->
    Fmt.(pf ppf "  | %a -> (%d,%d)\n"
          (list ~sep:(any " | ") Fmt.int) states
          rhslen lhs));
  Fmt.pf ppf "  | _ -> raise (Invalid_argument \"This state doesn't have any default reduction\") in\n";
  Fmt.pf ppf "  (rhslen, nonterminal_of_int lhs)\n"

let emit_follow_transition ppf = (* taking reduction(if no default) and transitions *)
  Fmt.pf ppf "\nlet follow_transition: %s -> %s -> %s * bool = fun state nt ->\n\
  \  let state = state_to_int state in\n\
  \  let nt = nonterminal_to_int nt in\n\
  \  let state, has_default = match state, nt with\n"
    state_t nonterminal_t state_t;
  Lr1.fold (fun lr1 acc -> begin
    List.fold_left (fun acc (symbol, next_state) ->
      match symbol with
      | T _ -> acc
      | N nt -> ((Lr1.to_int lr1, Nonterminal.to_int nt),
      (Lr1.to_int next_state, has_reducable_productions next_state))::acc
      ) acc (Lr1.transitions lr1)
  end) [] |>
  sort_and_merge
    (fun (i,_) (i2,_) -> i-i2)
    (fun next prev -> fst next == fst prev && snd next == snd prev)
      |>
  List.iter (fun (state_nt, (next_state, has_default)) ->
    Fmt.(pf ppf "  | %a -> (%d,%b)\n"
          (list ~sep:(any " | ") (fun ppf (s,nt) -> Fmt.pf ppf "(%d,%d)" s nt)) state_nt
          next_state has_default));
  Fmt.pf ppf "  | _ -> raise (Invalid_argument \"This state and nonterminal don't lead to any transition\") in\n";
  Fmt.pf ppf "  (state_of_int state, has_default)\n"

let emit_transition_tokens ppf = (* taking reduction(if no default) and transitions *)
  Fmt.pf ppf "\nlet transition_tokens: %s -> %s list = fun state ->\n  match state_to_int state with\n" state_t completion_entry_t;
  Lr1.fold (fun lr1 acc -> begin
    let comp_entries = List.filter_map (function
      | T term, _ -> terminal_filter_map term
      | N nonterm, _ -> nonterminal_filter_map nonterm
    ) (Lr1.transitions lr1) in
    let comp_entries = if not @@ has_reducable_productions lr1 then
      List.fold_left (fun acc (t,_) ->
        (Option.to_list @@ terminal_filter_map t) @ acc
        ) comp_entries (Lr1.get_reductions lr1)
        else comp_entries in
    if comp_entries == [] then acc
    else (lr1, List.sort_uniq completion_entry_compare comp_entries)::acc
  end) [] |>
  sort_and_merge
    (List.compare completion_entry_compare)
    (List.equal completion_entry_equal)
          |>
  List.iter (fun (states, comp_entries) ->
    Fmt.(pf ppf "  | %a ->\n    [%a]\n"
            (list ~sep:(any " | ") (using Lr1.to_int int)) states
            (list ~sep:(any ";") pp_completion_entry) comp_entries));
  Fmt.pf ppf "  | _ -> []\n"

let emit_next_symbol_of_state_test ppf = (* Taking items' firsts and following transitions if nullable *)
  Fmt.pf ppf "\nlet transition_tokens2: %s -> %s list = fun state ->\n  match state_to_int state with\n" state_t completion_entry_t;
  let rec get_comp = fun lr1 -> begin
    let symbols = List.sort_uniq Symbol.compare @@
      List.filter_map (fun (prod,idx) ->
        let rhs = Production.rhs prod in
        try let (symbol, _, _) = rhs.(idx) in Some symbol
        with _ -> None
        ) @@ Lr0.items @@ Lr1.lr0 lr1 in
    let comp_entries = List.flatten @@ List.map (function
      | T term -> Option.to_list @@ terminal_filter_map term
      | N nonterm -> begin
        let firsts = List.filter_map terminal_filter_map @@ Nonterminal.first nonterm in
        if Nonterminal.nullable nonterm then
          let tra = Lr1.transitions lr1 in
          match List.find_map (fun (s,next_lr1) ->
            if Symbol.equal s (N nonterm) then
              Some next_lr1 else None) tra with
                | Some lr1 -> get_comp lr1 @ firsts
                | None -> firsts
            else firsts end)
    symbols in comp_entries end in
  Lr1.iter (fun lr1 -> begin
    let comp_entries = get_comp lr1 in
    let uniq_comp_entries = List.sort_uniq completion_entry_compare comp_entries in
    match uniq_comp_entries with
        | [] -> ()
        | lr1transition ->
            Fmt.pf ppf "  | %d\t -> [%a]\n"
            (Lr1.to_int lr1)
            (Fmt.list ~sep:(Fmt.any ";") pp_completion_entry) lr1transition
      end);
      Fmt.pf ppf "  | _ -> []\n"

let emit_next_symbol_of_state_sorted ppf =
  (* Fmt.pf ppf "\nlet transition_tokens: %s -> %s list = function\n" *)
  (* state_type_name completion_type_name; *)
  Fmt.pf ppf "\nlet transition_tokens: %s -> %s list = fun state ->\n  match state_to_int state with\n"
  state_t completion_entry_t;
  Lr1.fold (fun lr1 acc -> begin
    let comp_entries = List.filter_map (fun (s, _) ->
      match s with
        | T term -> terminal_filter_map term
        | N nonterm -> nonterminal_filter_map nonterm) (Lr1.transitions lr1) in
    (* let comp_entries = *)
    (*   List.fold_left (fun acc (term, _) -> *)
    (*     (Option.to_list @@ terminal_filter_map term) @ acc) *)
    (*   comp_entries (Lr1.reductions lr1) *)
    (* in *)
    if comp_entries == []
    then acc
    else let sorted_comp_entries =
      List.sort_uniq Stdlib.compare comp_entries in
    (lr1, sorted_comp_entries)::acc
  end) []
        |> (* Finds all state with equal token list and merge them *)
  List.sort (fun (_, terms1) (_, terms2) ->
    List.compare completion_entry_compare terms1 terms2)
        |>
  List.fold_left (fun acc (lr1, comp_entries) -> begin
    match acc with
      | (prev_lr1s, prev_comp_entries)::t
        when List.equal completion_entry_equal comp_entries prev_comp_entries ->
          (lr1::prev_lr1s, prev_comp_entries)::t
      | _ -> ([lr1], comp_entries)::acc
  end) []
        |>
  List.iter (fun (states, tokens) ->
    Fmt.(pf ppf "  | %a ->\n    [%a]\n"
            (list ~sep:(any " | ") (using Lr1.to_int int)) states
            (list ~sep:(any ";") pp_completion_entry) tokens));
  Fmt.pf ppf "  | _ -> []\n"

let emit ppf =
  Fmt.pf ppf
    "(* Caution: this file was automatically generated from %s; do not edit *)\
     @\nopen Grammar_tokens@\n"
    cmlyname;
    emit_types ppf;
    emit_completion_entry ppf;
    emit_get_default_production ppf;
    emit_follow_transition ppf;
    (* emit_next_symbol_of_state_sorted ppf; *)
    (* emit_next_symbol_of_state_test ppf; *)
    emit_transition_tokens ppf;
    (* emit_next_symbol_of_state ppf; *)
    (* emit_productions ppf; *)
    (* emit_firsts ppf; *)
    (* emit_lr0 ppf; *)
    (* Terminal.iter (fun term -> Print.terminal ppf term; Fmt.string ppf ";") *)
    ()

let () =
  emit Fmt.stdout
