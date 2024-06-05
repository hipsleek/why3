(* Goal : print pmodule ast *)

open Format
open Why3
open Wstdlib

let print_string : string Pp.pp =
  fun fmt s -> fprintf fmt "\"%a\"" Pp.print_string s

and print_int : int Pp.pp =
  fun fmt i -> fprintf fmt "%d" i

and print_bool : bool Pp.pp =
  fun fmt b -> fprintf fmt "%b" b

and print_list : 'a Pp.pp -> 'a list Pp.pp =
  fun printer fmt l -> fprintf fmt "[%a]"
    (Pp.print_list Pp.newline printer) l

and print_option : 'a Pp.pp -> 'a option Pp.pp =
  fun printer fmt o -> fprintf fmt "<%a>"
    (Pp.print_option printer) o

let rec print_pmodule : Pmodule.pmodule Pp.pp =
  fun fmt m ->
    fprintf fmt "@[<hov 2>(pmodule@ %a)@]"
      (print_list print_mod_unit) m.Pmodule.mod_units

and print_mod_unit : Pmodule.mod_unit Pp.pp =
  fun fmt mu ->
    match mu with
    | Pmodule.Udecl d -> fprintf fmt "@[<hov 2>(Udecl@ %a)@]"
        print_pdecl d
    | Pmodule.Uuse _ -> fprintf fmt "@[<hov 2>(Uuse@ ...)@]"
    | Pmodule.Uclone mi -> fprintf fmt "@[<hov 2>(Uclone@ %a)@]"
        print_mod_inst mi
    | Pmodule.Umeta _ -> fprintf fmt "@[<hov 2>(Umeta@ ...)@]"
    | Pmodule.Uscope (s, mul) -> fprintf fmt "@[<hov 2>(Uscope@ %a@ %a)@]"
        print_string s
        (print_list print_mod_unit) mul

and print_namespace : Pmodule.namespace Pp.pp =
  fun fmt ns ->
    ignore ns;
    fprintf fmt "@[<hov 2>(namespace@ ...)@]" (* TODO *)

and print_mod_inst : Pmodule.mod_inst Pp.pp =
  fun fmt mi ->
    fprintf fmt "@[<hov 2>(mod_inst@ %a)@]"
      print_pmodule mi.Pmodule.mi_mod

and print_pdecl : Pdecl.pdecl Pp.pp =
  fun fmt pd ->
    fprintf fmt "@[<hov 2>(pdecl@ %a@ %a)@]"
      print_pdecl_node pd.Pdecl.pd_node
      (print_list print_decl) pd.Pdecl.pd_pure

and print_pdecl_node : Pdecl.pdecl_node Pp.pp =
  fun fmt pdn -> match pdn with
    | Pdecl.PDtype idl -> fprintf fmt "@[<hov 2>(PDtype@ %a)@]"
        (print_list print_its_defn) idl
    | Pdecl.PDlet ld -> fprintf fmt "@[<hov 2>(PDlet@ %a)@]"
        print_let_defn ld
    | Pdecl.PDexn xs -> fprintf fmt "@[<hov 2>(PDexn@ %a)@]"
        print_xsymbol xs
    | Pdecl.PDpure -> fprintf fmt "@[<hov 2>(PDPure)@]"

and print_its_defn : Pdecl.its_defn Pp.pp =
  fun fmt id ->
    ignore id;
    fprintf fmt "@[<hov 2>(its_defn@ ...)@]" (* TODO *)

and print_let_defn : Expr.let_defn Pp.pp =
  fun fmt ld -> match ld with
    | Expr.LDvar (pvs, e) -> fprintf fmt "@[<hov 2>(LDvar@ %a@ %a)@]"
        print_pvsymbol pvs
        print_expr e
    | Expr.LDsym (rs, ce) -> fprintf fmt "@[<hov 2>(LDsym@ %a@ %a)@]"
        print_rsymbol rs
        print_cexp ce
    | Expr.LDrec rdl -> fprintf fmt "@[<hov 2>(LDrec@ %a)@]"
        (print_list print_rec_defn) rdl

and print_expr : Expr.expr Pp.pp =
  fun fmt e ->
    fprintf fmt "@[<hov 2>(expr@ %a@ %a@ %a@ %a@ %a)@]"
      print_expr_node e.Expr.e_node
      print_ity e.Expr.e_ity
      print_mask e.Expr.e_mask
      print_effect e.Expr.e_effect
      (print_list print_attribute) (Ident.Sattr.elements e.Expr.e_attrs)

and print_expr_node : Expr.expr_node Pp.pp =
  fun fmt en ->
    ignore en;
    fprintf fmt "@[<hov 2>(expr_node)@]"

and print_cexp : Expr.cexp Pp.pp =
  fun fmt ce ->
    fprintf fmt "@[<hov 2>(cexp@ %a@ %a)@]"
      print_cexp_node ce.Expr.c_node
      print_cty ce.Expr.c_cty

and print_cexp_node : Expr.cexp_node Pp.pp =
  fun fmt cen -> match cen with
    | Expr.Capp (rs, pvl) -> fprintf fmt "@[<hov 2>(Capp@ %a@ %a)@]"
        print_rsymbol rs
        (print_list print_pvsymbol) pvl
    | Expr.Cpur (ls, pvl) -> fprintf fmt "@[<hov 2>(Cpur@ %a@ %a)@]"
        print_lsymbol ls
        (print_list print_pvsymbol) pvl
    | Expr.Cfun e -> fprintf fmt "@[<hov 2>(Cfun@ %a)@]"
        print_expr e
    | Expr.Cany -> fprintf fmt "@[<hov 2>(Cany)@]"

and print_assign : Expr.assign Pp.pp =
  fun fmt a ->
    ignore a;
    fprintf fmt "@[<hov 2>(assign)@]" (* TODO *)

and print_reg_branch : Expr.reg_branch Pp.pp =
  fun fmt rb ->
    ignore rb;
    fprintf fmt "@[<hov 2>(reg_branch)@]" (* TODO *)

and print_exn_branch : Expr.exn_branch Pp.pp =
  fun fmt eb ->
    ignore eb;
    fprintf fmt "@[<hov 2>(exn_branch)@]" (* TODO *)

and print_invariant : Expr.invariant Pp.pp =
  fun fmt iv ->
    fprintf fmt "@[<hov 2>(invariant@ %a)@]"
      print_term iv

and print_rec_defn : Expr.rec_defn Pp.pp =
  fun fmt rd -> fprintf fmt "@[<hov 2>(rec_defn@ %a@ %a@ %a@ %a)@]"
    print_rsymbol rd.Expr.rec_sym
    print_rsymbol rd.Expr.rec_rsym
    print_cexp rd.Expr.rec_fun
    (print_list print_variant) rd.Expr.rec_varl

and print_variant : Expr.variant Pp.pp =
  fun fmt v ->
    let (t, ls_opt) = v in
    fprintf fmt "@[<hov 2>(variant@ %a@ %a)@]"
      print_term t
      (print_option print_lsymbol) ls_opt

and print_decl : Decl.decl Pp.pp =
  fun fmt d ->
    fprintf fmt "@[<hov 2>(decl@ %a@ %a)@]"
      print_decl_node d.Decl.d_node
      (print_list print_ident) (Ident.Sid.elements d.Decl.d_news)

and print_decl_node : Decl.decl_node Pp.pp =
  fun fmt dn ->
    ignore dn;
    fprintf fmt "@[<hov 2>(decl_node)@]" (* TODO *)

and print_xsymbol : Ity.xsymbol Pp.pp =
  fun fmt xs ->
    ignore xs;
    fprintf fmt "@[<hov 2>(xsymbol)@]" (* TODO *)

and print_rsymbol : Expr.rsymbol Pp.pp =
  fun fmt rs ->
    fprintf fmt "@[<hov 2>(rsymbol@ %a@ %a@ %a@ %a)@]"
      print_ident rs.Expr.rs_name
      print_cty rs.Expr.rs_cty
      print_rs_logic rs.Expr.rs_logic
      (print_option print_pvsymbol) rs.Expr.rs_field

and print_ity : Ity.ity Pp.pp =
  fun fmt ity ->
    fprintf fmt "@[<hov 2>(ity@ %a@ %a)@]"
      print_ity_node ity.Ity.ity_node
      print_bool ity.Ity.ity_pure

and print_ity_node : Ity.ity_node Pp.pp =
  fun fmt ityn ->
    ignore ityn;
    fprintf fmt "@[<hov 2>(ity_node)@]"

and print_cty : Ity.cty Pp.pp =
  fun fmt cty ->
    ignore cty;
    fprintf fmt "@[<hov 2>(cty)@]"

and print_rs_logic : Expr.rs_logic Pp.pp =
  fun fmt logic ->
    match logic with
      | Expr.RLnone -> fprintf fmt "@[<hov 2>(RLnone)@]"
      | Expr.RLpv pvs -> fprintf fmt "@[<hov 2>(RLpv@ %a)@]"
          print_pvsymbol pvs
      | Expr.RLls ls -> fprintf fmt "@[<hov 2>(RLls@ %a)@]"
          print_lsymbol ls
      | Expr.RLlemma -> fprintf fmt "@[<hov 2>(RLlemma)@]"

and print_lsymbol : Term.lsymbol Pp.pp =
  fun fmt ls ->
    fprintf fmt "@[<hov 2>(lsymbol@ %a@ %a@ %a@ %a@ %a)@]"
      print_ident ls.Term.ls_name
      (print_list print_ty) ls.Term.ls_args
      (print_option print_ty) ls.Term.ls_value
      print_int ls.Term.ls_constr
      print_bool ls.Term.ls_proj

and print_pvsymbol : Ity.pvsymbol Pp.pp =
  fun fmt pvs ->
    fprintf fmt "@[<hov 2>(pvsymbol@ %a@ %a@ %a)@]"
      print_vsymbol pvs.Ity.pv_vs
      print_ity pvs.Ity.pv_ity
      print_bool pvs.Ity.pv_ghost

and print_vsymbol : Term.vsymbol Pp.pp =
  fun fmt vs ->
    ignore vs;
    fprintf fmt "@[<hov 2>(vsymbol)@]"

and print_ident : Ident.ident Pp.pp =
  fun fmt i ->
    fprintf fmt "@[<hov 2>(ident@ %a@ %a)@]"
      print_string i.Ident.id_string
      (print_list print_attribute) (Ident.Sattr.elements i.Ident.id_attrs)

and print_attribute : Ident.attribute Pp.pp =
  fun fmt attr ->
    fprintf fmt "@[<hov 2>(attribute@ %a)@]"
      print_string attr.Ident.attr_string

and print_ty : Ty.ty Pp.pp =
  fun fmt ty ->
    ignore ty;
    fprintf fmt "@[<hov 2>(ty)@]"

and print_mask : Ity.mask Pp.pp =
  fun fmt mask ->
    ignore mask;
    fprintf fmt "@[<hov 2>(mask)@]"

and print_effect : Ity.effect Pp.pp =
  fun fmt eff ->
    ignore eff;
    fprintf fmt "@[<hov 2>(effect)@]"

and print_term : Term.term Pp.pp =
  fun fmt t ->
    ignore t;
    fprintf fmt "@[<hov 2>(term)@]"

let usage_msg =
  "<file>\n\
  Parse and type check the given file, then display the AST"

let option_list = []

let opt_file = ref None

let add_opt_file file = opt_file := Some file

let config, env =
  eprintf "why3inspect.ml: initialize args@.";
  Whyconf.Args.initialize option_list add_opt_file usage_msg

let handle_no_file () =
  Whyconf.Args.exit_with_usage usage_msg

let handle_file file =
  let handle_module m = eprintf "%a@\n@." print_pmodule m in
  let (modules, _) = Env.read_file Pmodule.mlw_language env file in
  let modules = Mstr.values modules in
  List.iter handle_module modules

let () =
  eprintf "why3inspect.ml: handle input file@.";
  try
    match !opt_file with
      | None -> handle_no_file ()
      | Some file -> handle_file file
  with e when not (Debug.test_flag Debug.stack_trace) ->
    eprintf "%a@." Exn_printer.exn_printer e;
    exit 1
