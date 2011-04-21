(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2010-                                                   *)
(*    François Bobot                                                     *)
(*    Jean-Christophe Filliâtre                                          *)
(*    Claude Marché                                                      *)
(*    Andrei Paskevich                                                    *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(** SMT v1 printer with some extensions *)

open Format
open Pp
open Ident
open Ty
open Term
open Decl
open Theory
open Task
open Printer

(** Options of this printer *)
let use_trigger = Theory.register_meta_excl "Smt : trigger" []
let use_builtin_array = Theory.register_meta_excl "Smt : builtin array" []

(** *)

let ident_printer =
  let bls = (*["and";" benchmark";" distinct";"exists";"false";"flet";"forall";
     "if then else";"iff";"implies";"ite";"let";"logic";"not";"or";
     "sat";"theory";"true";"unknown";"unsat";"xor";
     "assumption";"axioms";"defintion";"extensions";"formula";
     "funs";"extrafuns";"extrasorts";"extrapreds";"language";
     "notes";"preds";"sorts";"status";"theory";"Int";"Real";"Bool";
     "Array";"U";"select";"store"]*)
    (** smtlib2 V2 p71 *)
    [(** General: *)
      "!";"_"; "as"; "DECIMAL"; "exists"; "forall"; "let"; "NUMERAL";
      "par"; "STRING";
       (**Command names:*)
      "assert";"check-sat"; "declare-sort";"declare-fun";"define-sort";
      "define-fun";"exit";"get-assertions";"get-assignment"; "get-info";
      "get-option"; "get-proof"; "get-unsat-core"; "get-value"; "pop"; "push";
      "set-logic"; "set-info"; "set-option";
       (** for security *)
      "Bool";"unsat";"sat";"true";"false";"select";"store"]
  in
  let san = sanitizer char_to_alpha char_to_alnumus in
  create_ident_printer bls ~sanitizer:san

let print_ident fmt id =
  fprintf fmt "%s" (id_unique ident_printer id)

(** type *)
type info = {
  info_syn : string Mid.t;
  info_rem : Sid.t;
  use_trigger : bool;
  barrays : (ty*ty) Mts.t;
  complex_type : ty Hty.t;
}

let rec print_type info fmt ty = match ty.ty_node with
  | Tyvar _ -> unsupported "smt : you must encode the polymorphism"
  | Tyapp (ts, []) -> begin match query_syntax info.info_syn ts.ts_name with
      | Some s -> syntax_arguments s (print_type info) fmt []
      | None -> fprintf fmt "%a" print_ident ts.ts_name
  end
  | Tyapp (ts, l) ->
     begin match query_syntax info.info_syn ts.ts_name with
      | Some s -> syntax_arguments s (print_type info) fmt l
      | None -> fprintf fmt "(%a %a)" print_ident ts.ts_name
        (print_list space (print_type info)) l
     end

let iter_complex_type info fmt () ty =
    match ty.ty_node with
      | Tyapp (_,_::_) when not (Hty.mem info.complex_type ty) ->
        let id = id_fresh (Pp.string_of_wnl Pretty.print_ty ty) in
        let ts = create_tysymbol id [] None in
        let cty = ty_app ts [] in
        fprintf fmt "(define-sorts ((%a %a)))@."
          print_ident ts.ts_name
          (print_type info) ty;
        Hty.add info.complex_type ty cty
      | _ -> ()


let find_complex_type info fmt f =
  f_ty_fold (iter_complex_type info fmt) () f

let find_complex_type_expr info fmt f =
  e_fold
    (t_ty_fold (iter_complex_type info fmt))
    (f_ty_fold (iter_complex_type info fmt))
    () f

let print_type info fmt ty =
  print_type info fmt
    (try
       Hty.find info.complex_type ty
     with Not_found -> ty)

(* and print_tyapp info fmt = function *)
(*   | [] -> () *)
(*   | [ty] -> fprintf fmt "%a " (print_type info) ty *)
(*   | tl -> fprintf fmt "(%a) " (print_list comma (print_type info)) tl *)

let print_type info fmt =
  catch_unsupportedType (print_type info fmt)

let print_type_value info fmt = function
  | None -> fprintf fmt "Bool"
  | Some ty -> print_type info fmt ty

(** var *)
let forget_var v = forget_id ident_printer v.vs_name

let print_var fmt {vs_name = id} =
  let n = id_unique ident_printer id in
  fprintf fmt "%s" n

let print_typed_var info fmt vs =
  fprintf fmt "(%a %a)" print_var vs
    (print_type info) vs.vs_ty

let print_var_list info fmt vsl =
  print_list space (print_typed_var info) fmt vsl

(** expr *)
let rec print_term info fmt t = match t.t_node with
  | Tconst (ConstInt n) -> fprintf fmt "%s" n
  | Tconst (ConstReal c) ->
      Print_real.print_with_integers
	"%s.0" "(* %s.0 %s.0)" "(/ %s.0 %s.0)" fmt c
  | Tvar v -> print_var fmt v
  | Tapp (ls, tl) -> begin match query_syntax info.info_syn ls.ls_name with
      | Some s -> syntax_arguments_typed s (print_term info)
        (print_type info) (Some t) fmt tl
      | None -> begin match tl with (* for cvc3 wich doesn't accept (toto ) *)
          | [] -> fprintf fmt "@[%a@]" print_ident ls.ls_name
          | _ -> fprintf fmt "@[(%a@ %a)@]"
	      print_ident ls.ls_name (print_list space (print_term info)) tl
        end end
  | Tlet (t1, tb) ->
      let v, t2 = t_open_bound tb in
      fprintf fmt "@[(let (%a %a)@ %a)@]" print_var v
        (print_term info) t1 (print_term info) t2;
      forget_var v
  | Tif (f1,t1,t2) ->
      fprintf fmt "@[(ite %a@ %a@ %a)@]"
        (print_fmla info) f1 (print_term info) t1 (print_term info) t2
  | Tcase _ -> unsupportedTerm t
      "smtv2 : you must eliminate match"
  | Teps _ -> unsupportedTerm t
      "smtv2 : you must eliminate epsilon"

and print_fmla info fmt f = match f.f_node with
  | Fapp ({ ls_name = id }, []) ->
      print_ident fmt id
  | Fapp (ls, tl) -> begin match query_syntax info.info_syn ls.ls_name with
      | Some s -> syntax_arguments_typed s (print_term info)
        (print_type info) None fmt tl
      | None -> begin match tl with (* for cvc3 wich doesn't accept (toto ) *)
          | [] -> fprintf fmt "%a" print_ident ls.ls_name
          | _ -> fprintf fmt "(%a@ %a)"
	      print_ident ls.ls_name (print_list space (print_term info)) tl
        end end
  | Fquant (q, fq) ->
      let q = match q with Fforall -> "forall" | Fexists -> "exists" in
      let vl, tl, f = f_open_quant fq in
      (* TODO trigger dépend des capacités du prover : 2 printers?
      smtwithtriggers/smtstrict *)
      if tl = [] then
        fprintf fmt "@[(%s@ (%a)@ %a)@]"
          q
          (print_var_list info) vl
          (print_fmla info) f
      else
        fprintf fmt "@[(%s@ (%a)@ (! %a %a))@]"
          q
          (print_var_list info) vl
          (print_fmla info) f
          (print_triggers info) tl;
      List.iter forget_var vl
  | Fbinop (Fand, f1, f2) ->
      fprintf fmt "@[(and@ %a@ %a)@]" (print_fmla info) f1 (print_fmla info) f2
  | Fbinop (For, f1, f2) ->
      fprintf fmt "@[(or@ %a@ %a)@]" (print_fmla info) f1 (print_fmla info) f2
  | Fbinop (Fimplies, f1, f2) ->
      fprintf fmt "@[(implies@ %a@ %a)@]"
        (print_fmla info) f1 (print_fmla info) f2
  | Fbinop (Fiff, f1, f2) ->
      fprintf fmt "@[(iff@ %a@ %a)@]" (print_fmla info) f1 (print_fmla info) f2
  | Fnot f ->
      fprintf fmt "@[(not@ %a)@]" (print_fmla info) f
  | Ftrue ->
      fprintf fmt "true"
  | Ffalse ->
      fprintf fmt "false"
  | Fif (f1, f2, f3) ->
      fprintf fmt "@[(if_then_else %a@ %a@ %a)@]"
	(print_fmla info) f1 (print_fmla info) f2 (print_fmla info) f3
  | Flet (t1, tb) ->
      let v, f2 = f_open_bound tb in
      fprintf fmt "@[(let ((%a %a))@ %a)@]" print_var v
        (print_term info) t1 (print_fmla info) f2;
      forget_var v
  | Fcase _ -> unsupportedFmla f
      "smtv2 : you must eliminate match"

and print_expr info fmt = e_apply (print_term info fmt) (print_fmla info fmt)

and print_trigger info fmt e = fprintf fmt "(%a)" (print_expr info) e

and print_triggers info fmt = function
  | [] -> ()
  | a::l -> fprintf fmt ":pattern (%a) %a"
    (print_list space (print_trigger info)) a
    (print_triggers info) l

let print_logic_binder info fmt v =
  fprintf fmt "%a: %a" print_ident v.vs_name
    (print_type info) v.vs_ty

let print_type_decl info fmt = function
  | ts, Tabstract when Sid.mem ts.ts_name info.info_rem -> false
  | ts, Tabstract when ts.ts_args = [] ->
    begin try
      (* keep this hack for compatibility with smtv1 *)
      let key,elt = Mts.find ts info.barrays in
      fprintf fmt "(define-sorts (%a  (Array %a %a)))"
        print_ident ts.ts_name
        (print_type info) key
        (print_type info) elt; true
    with Not_found ->
      fprintf fmt "(declare-sort %a 0)" print_ident ts.ts_name; true end
  | ts, Tabstract when ts.ts_def = None ->
    let len = List.length ts.ts_args in
    fprintf fmt "(declare-sort %a %i)" print_ident ts.ts_name len; true
  | _, Tabstract -> false
  | _, Talgebraic _ -> unsupported
          "smtv2 : algebraic type are not supported"

let print_logic_decl info fmt (ls,ld) =
  if not (Mid.mem ls.ls_name info.info_syn) then
  match ld with
  | None ->
    fprintf fmt "@[<hov 2>(declare-fun %a (%a) %a)@]@\n"
      print_ident ls.ls_name
      (print_list space (print_type info)) ls.ls_args
      (print_type_value info) ls.ls_value
  | Some def ->
    let vsl,expr = Decl.open_ls_defn def  in
    find_complex_type_expr info fmt expr;
    fprintf fmt "@[<hov 2>(declare-fun %a (%a) %a %a)@]@\n"
      print_ident ls.ls_name
      (print_var_list info) vsl
      (print_type_value info) ls.ls_value
      (print_expr info) expr

let print_logic_decl info fmt d =
  if Sid.mem (fst d).ls_name info.info_rem then
    false else (print_logic_decl info fmt d; true)

let print_decl info fmt d = match d.d_node with
  | Dtype dl ->
      print_list_opt newline (print_type_decl info) fmt dl
  | Dlogic dl ->
      print_list_opt newline (print_logic_decl info) fmt dl
  | Dind _ -> unsupportedDecl d
      "smt : inductive definition are not supported"
  | Dprop (Paxiom, pr, _) when Sid.mem pr.pr_name info.info_rem -> false
  | Dprop (Paxiom, pr, f) ->
    find_complex_type info fmt f;
      fprintf fmt "@[<hov 2>;; %s@\n(assert@ %a)@]@\n"
        pr.pr_name.id_string
        (print_fmla info) f; true
  | Dprop (Pgoal, pr, f) ->
    find_complex_type info fmt f;
      fprintf fmt "@[(assert@\n";
      fprintf fmt "@[;; %a@]@\n" print_ident pr.pr_name;
      (match pr.pr_name.id_loc with
        | None -> ()
        | Some loc -> fprintf fmt " @[;; %a@]@\n"
            Loc.gen_report_position loc);
      fprintf fmt "  @[(not@ %a))@]" (print_fmla info) f;
      fprintf fmt "@[(check-sat)@]@\n";
      true
  | Dprop ((Plemma|Pskip), _, _) -> assert false

let print_decl info fmt = catch_unsupportedDecl (print_decl info fmt)

let barrays task =
  let fold barrays =
    function
      | [MAts tst;MAty tyk;MAty tye] ->
        (* let extract_ty ts = *)
        (*   if ts.ts_args <> [] then *)
        (*     unsupported "smtv2 : type with argument are not supported"; *)
        (*   match ts.ts_def with *)
        (*     | Some ty -> ty *)
        (*     | None -> ty_app ts [] in *)
        Mts.add tst (tyk,tye) barrays
      | _ -> assert false in
  Task.on_meta Encoding_arrays.meta_mono_array fold Mts.empty task

let meta_dist_syntax =
  Theory.register_meta "smt_dist_syntax" [MTlsymbol;MTstring]

let distingued =
  let dist_syntax mls = function
    | [MAls ls;MAstr s] -> Mls.add ls s mls
    | _ -> assert false in
  let dist_dist syntax mls = function
    | [MAls ls;MAls lsdis] ->
      begin try
              Mid.add lsdis.ls_name (Mls.find ls syntax) mls
        with Not_found -> mls end
    | _ -> assert false in
  Trans.on_meta meta_dist_syntax (fun syntax ->
    let syntax = List.fold_left dist_syntax Mls.empty syntax in
    Trans.on_meta Encoding_arrays.meta_lsdis (fun dis ->
      let dis2 = List.fold_left (dist_dist syntax) Mid.empty dis in
      Trans.return dis2))

let print_task pr thpr fmt task =
  print_prelude fmt pr;
  print_th_prelude task fmt thpr;
  let info = {
    info_syn = Mid.union (fun _ _ s -> Some s)
      (get_syntax_map task) (Trans.apply distingued task);
    info_rem = get_remove_set task;
    use_trigger = false;
    barrays = barrays task;
    complex_type = Hty.create 5;
  }
  in
  let decls = Task.task_decls task in
  ignore (print_list_opt (add_flush newline2) (print_decl info) fmt decls)

let () = register_printer "smtv2"
  (fun _env pr thpr ?old:_ fmt task ->
     forget_all ident_printer;
     print_task pr thpr fmt task)
