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
    (Pp.print_list Pp.comma printer) l

and print_option : 'a Pp.pp -> 'a option Pp.pp =
  fun printer fmt o -> fprintf fmt "<%a>"
    (Pp.print_option printer) o

and print_binding : 'a Pp.pp -> 'b Pp.pp -> ('a * 'b) Pp.pp =
  fun printer_a printer_b fmt (a, b) -> fprintf fmt "(%a@ =>@ %a)"
    printer_a a
    printer_b b

let rec print_pmodule : Pmodule.pmodule Pp.pp =
  fun fmt m ->
    fprintf fmt "@[<hov 2>pmodule{mod_theory=%a,@ mod_units=%a,@ mod_export=%a,@ mod_known=%a,@ mod_local=%a,@ mod_used=%a}@]"
      print_theory m.Pmodule.mod_theory
      (print_list print_mod_unit) m.Pmodule.mod_units
      print_namespace m.Pmodule.mod_export
      (print_list print_ident) (Ident.Mid.keys m.Pmodule.mod_known)
      (print_list print_ident) (Ident.Sid.elements m.Pmodule.mod_local)
      (print_list print_ident) (Ident.Sid.elements m.Pmodule.mod_used)

and print_mod_unit : Pmodule.mod_unit Pp.pp =
  fun fmt mu ->
    match mu with
    | Pmodule.Udecl d -> fprintf fmt "@[<hov 2>Udecl(%a)@]"
        print_pdecl d
    | Pmodule.Uuse _ -> fprintf fmt "@[<hov 2>Uuse(...)@]"
    | Pmodule.Uclone mi -> fprintf fmt "@[<hov 2>Uclone(%a)@]"
        print_mod_inst mi
    | Pmodule.Umeta _ -> fprintf fmt "@[<hov 2>Umeta(...)@]"
    | Pmodule.Uscope (s, mul) -> fprintf fmt "@[<hov 2>Uscope(%a,@\n%a)@]"
        print_string s
        (print_list print_mod_unit) mul

and print_namespace : Pmodule.namespace Pp.pp =
  fun fmt ns ->
    ignore ns;
    fprintf fmt "@[<hov 2>namespace{...}@]"
    (* fprintf fmt "@[<hov 2>namespace@ {@ ns_ts=%a,@ ns_ps=%a,@ ns_xs=%a,@ ns_ns=%a@ }@]"
      (print_list print_string) (Mstr.keys ns.Pmodule.ns_ts)
      (print_list print_string) (Mstr.keys ns.Pmodule.ns_ps)
      (print_list print_string) (Mstr.keys ns.Pmodule.ns_xs)
      (print_list (print_binding print_string print_namespace)) (Mstr.bindings ns.Pmodule.ns_ns) *)

and print_mod_inst : Pmodule.mod_inst Pp.pp =
  fun fmt mi ->
    fprintf fmt "@[<hov 2>mod_inst{mi_mod=%a}@]"
      print_pmodule mi.Pmodule.mi_mod

and print_pdecl : Pdecl.pdecl Pp.pp =
  fun fmt pd ->
    fprintf fmt "@[<hov 2>pdecl{pd_node=%a,@ pd_pure=%a,@ pd_meta=%a,@ pd_syms=%a,@ pd_news=%a,@ pd_tag=%a}@]"
      print_pdecl_node pd.Pdecl.pd_node
      (print_list print_decl) pd.Pdecl.pd_pure
      (print_list print_meta_decl) pd.Pdecl.pd_meta
      (print_list print_ident) (Ident.Sid.elements pd.Pdecl.pd_syms)
      (print_list print_ident) (Ident.Sid.elements pd.Pdecl.pd_news)
      print_int pd.Pdecl.pd_tag

and print_pdecl_node : Pdecl.pdecl_node Pp.pp =
  fun fmt pdn -> match pdn with
    | Pdecl.PDtype idl -> fprintf fmt "@[<hov 2>PDtype(%a)@]"
        (print_list print_its_defn) idl
    | Pdecl.PDlet ld -> fprintf fmt "@[<hov 2>PDlet(%a)@]"
        print_let_defn ld
    | Pdecl.PDexn xs -> fprintf fmt "@[<hov 2>PDexn(%a)@]"
        print_xsymbol xs
    | Pdecl.PDpure -> fprintf fmt "@[<hov 2>PDPure@]"

and print_its_defn : Pdecl.its_defn Pp.pp =
  fun fmt itd ->
    fprintf fmt "@[<hov 2>its_defn{itd_its=%a,@ itd_fields=%a,@ itd_constructors=%a,@ itd_invariant=%a,@ itd_witness=%a}@]"
      print_itysymbol itd.Pdecl.itd_its
      (print_list print_rsymbol) itd.Pdecl.itd_fields
      (print_list print_rsymbol) itd.Pdecl.itd_constructors
      (print_list print_term) itd.Pdecl.itd_invariant
      (print_option print_expr) itd.Pdecl.itd_witness

and print_let_defn : Expr.let_defn Pp.pp =
  fun fmt ld -> match ld with
    | Expr.LDvar (pvs, e) -> fprintf fmt "@[<hov 2>LDvar(%a,@ %a)@]"
        print_pvsymbol pvs
        print_expr e
    | Expr.LDsym (rs, ce) -> fprintf fmt "@[<hov 2>LDsym(%a,@ %a)@]"
        print_rsymbol rs
        print_cexp ce
    | Expr.LDrec rdl -> fprintf fmt "@[<hov 2>LDrec(%a)@]"
        (print_list print_rec_defn) rdl

and print_expr : Expr.expr Pp.pp =
  fun fmt e ->
    fprintf fmt "@[<hov 2>expr{e_node=%a,@ e_ity=%a,@ e_mask=%a,@ e_effect=%a,@ e_attrs=%a}@]"
      print_expr_node e.Expr.e_node
      print_ity e.Expr.e_ity
      print_mask e.Expr.e_mask
      print_effect e.Expr.e_effect
      (print_list print_attribute) (Ident.Sattr.elements e.Expr.e_attrs)

and print_expr_node : Expr.expr_node Pp.pp =
  fun fmt en ->
    ignore en;
    fprintf fmt "@[<hov 2>(expr_node)@]" (* TODO *)

and print_cexp : Expr.cexp Pp.pp =
  fun fmt ce ->
    fprintf fmt "@[<hov 2>cexp{c_node=%a,@ c_cty=%a}@]"
      print_cexp_node ce.Expr.c_node
      print_cty ce.Expr.c_cty

and print_cexp_node : Expr.cexp_node Pp.pp =
  fun fmt cen -> match cen with
    | Expr.Capp (rs, pvl) -> fprintf fmt "@[<hov 2>Capp(%a,@ %a)@]"
        print_rsymbol rs
        (print_list print_pvsymbol) pvl
    | Expr.Cpur (ls, pvl) -> fprintf fmt "@[<hov 2>Cpur(%a,@ %a)@]"
        print_lsymbol ls
        (print_list print_pvsymbol) pvl
    | Expr.Cfun e -> fprintf fmt "@[<hov 2>Cfun(%a)@]"
        print_expr e
    | Expr.Cany -> fprintf fmt "@[<hov 2>Cany@]"

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
    fprintf fmt "@[<hov 2>invariant(%a)@]"
      print_term iv

and print_rec_defn : Expr.rec_defn Pp.pp =
  fun fmt rd -> fprintf fmt "@[<hov 2>rec_defn{rec_sym=%a,@ rec_rsym=%a,@ rec_fun=%a,@ rec_varl=%a}@]"
    print_rsymbol rd.Expr.rec_sym
    print_rsymbol rd.Expr.rec_rsym
    print_cexp rd.Expr.rec_fun
    (print_list print_variant) rd.Expr.rec_varl

and print_variant : Expr.variant Pp.pp =
  fun fmt v ->
    let (t, ls_opt) = v in
    fprintf fmt "@[<hov 2>variant(%a,@ %a)@]"
      print_term t
      (print_option print_lsymbol) ls_opt

and print_decl : Decl.decl Pp.pp =
  fun fmt d ->
    fprintf fmt "@[<hov 2>decl{d_node=%a,@ d_news=%a}@]"
      print_decl_node d.Decl.d_node
      (print_list print_ident) (Ident.Sid.elements d.Decl.d_news)

and print_decl_node : Decl.decl_node Pp.pp =
  fun fmt dn ->
    ignore dn;
    fprintf fmt "@[<hov 2>(decl_node)@]" (* TODO *)

and print_xsymbol : Ity.xsymbol Pp.pp =
  fun fmt xs ->
    fprintf fmt "@[<hov 2>xsymbol{xs_name=%a,@ xs_ity=%a,@ xs_mask=%a}@]"
      print_ident xs.Ity.xs_name
      print_ity xs.Ity.xs_ity
      print_mask xs.Ity.xs_mask

and print_rsymbol : Expr.rsymbol Pp.pp =
  fun fmt rs ->
    fprintf fmt "@[<hov 2>rsymbol{rs_name=%a,@ rs_cty=%a,@ rs_logic=%a,@ rs_field=%a}@]"
      print_ident rs.Expr.rs_name
      print_cty rs.Expr.rs_cty
      print_rs_logic rs.Expr.rs_logic
      (print_option print_pvsymbol) rs.Expr.rs_field

and print_itysymbol : Ity.itysymbol Pp.pp =
  fun fmt its ->
    fprintf fmt "@[<hov2>itysymbol{its_ts=%a,@ its_nonfree=%a,@ its_private=%a,@ its_mutable=%a,@ its_fragile=%a}@]"
      print_tysymbol its.Ity.its_ts
      print_bool its.Ity.its_nonfree
      print_bool its.Ity.its_private
      print_bool its.Ity.its_mutable
      print_bool its.Ity.its_fragile

and print_tysymbol : Ty.tysymbol Pp.pp =
  fun fmt ts ->
    fprintf fmt "@[<hov 2>tysymbol{ts_name=%a,@ ts_args=%a}@]"
      print_ident ts.Ty.ts_name
      (print_list print_tvsymbol) ts.Ty.ts_args

and print_tvsymbol : Ty.tvsymbol Pp.pp =
  fun fmt tv ->
    fprintf fmt "@[<hov 2>tvsymbol{tv_name=%a}@]"
      print_ident tv.Ty.tv_name

and print_ity : Ity.ity Pp.pp =
  fun fmt ity ->
    fprintf fmt "@[<hov 2>ity{ity_node=%a,@ ity_pure=%a}@]"
      print_ity_node ity.Ity.ity_node
      print_bool ity.Ity.ity_pure

and print_ity_node : Ity.ity_node Pp.pp =
  fun fmt ityn ->
    ignore ityn;
    fprintf fmt "@[<hov 2>(ity_node)@]" (* TODO *)

and print_cty : Ity.cty Pp.pp =
  fun fmt cty ->
    fprintf fmt "@[<hov 2>cty{cty_args=%a,@ cty_pre=%a,@ cty_post=%a,@ cty_xpost=%a,@ cty_oldies=%a,@ cty_effect=%a,@ cty_result=%a,@ cty_mask=%a}@]"
      (print_list print_pvsymbol) cty.Ity.cty_args
      (print_list print_pre) cty.Ity.cty_pre
      (print_list print_post) cty.Ity.cty_post
      (print_list print_xsymbol) (Ity.Mxs.keys cty.Ity.cty_xpost)
      (print_list print_pvsymbol) (Ity.Mpv.keys cty.Ity.cty_oldies)
      print_effect cty.Ity.cty_effect
      print_ity cty.Ity.cty_result
      print_mask cty.Ity.cty_mask

and print_pre : Ity.pre Pp.pp =
  fun fmt pre ->
    fprintf fmt "@[<hov 2>pre(%a)@]"
      print_term pre

and print_post : Ity.post Pp.pp =
  fun fmt post ->
    fprintf fmt "@[<hov 2>post(%a)@]"
      print_term post

and print_rs_logic : Expr.rs_logic Pp.pp =
  fun fmt logic ->
    match logic with
      | Expr.RLnone -> fprintf fmt "@[<hov 2>RLnone@]"
      | Expr.RLpv pvs -> fprintf fmt "@[<hov 2>RLpv(%a)@]"
          print_pvsymbol pvs
      | Expr.RLls ls -> fprintf fmt "@[<hov 2>RLls(%a)@]"
          print_lsymbol ls
      | Expr.RLlemma -> fprintf fmt "@[<hov 2>RLlemma@]"

and print_lsymbol : Term.lsymbol Pp.pp =
  fun fmt ls ->
    fprintf fmt "@[<hov 2>lsymbol{ls_name=%a,@ ls_args=%a,@ ls_value=%a,@ ls_constr=%a,@ ls_proj=%a}@]"
      print_ident ls.Term.ls_name
      (print_list print_ty) ls.Term.ls_args
      (print_option print_ty) ls.Term.ls_value
      print_int ls.Term.ls_constr
      print_bool ls.Term.ls_proj

and print_pvsymbol : Ity.pvsymbol Pp.pp =
  fun fmt pvs ->
    fprintf fmt "@[<hov 2>pvsymbol{pv_vs=%a,@ pv_ity=%a,@ pv_ghost=%a}@]"
      print_vsymbol pvs.Ity.pv_vs
      print_ity pvs.Ity.pv_ity
      print_bool pvs.Ity.pv_ghost

and print_vsymbol : Term.vsymbol Pp.pp =
  fun fmt vs ->
    fprintf fmt "@[<hov 2>vsymbol{vs_name=%a,@ vs_ty=%a}@]"
      print_ident vs.Term.vs_name
      print_ty vs.Term.vs_ty

and print_ident : Ident.ident Pp.pp =
  fun fmt i ->
    fprintf fmt "@[<hov 2>ident{id_string=%a,@ id_attrs=%a}@]"
      print_string i.Ident.id_string
      (print_list print_attribute) (Ident.Sattr.elements i.Ident.id_attrs)

and print_attribute : Ident.attribute Pp.pp =
  fun fmt attr ->
    fprintf fmt "@[<hov 2>attribute{attr_string=%a}@]"
      print_string attr.Ident.attr_string

and print_ty : Ty.ty Pp.pp =
  fun fmt ty ->
    fprintf fmt "@[<hov 2>ty{ty_node=%a}@]"
      print_ty_node ty.Ty.ty_node

and print_ty_node : Ty.ty_node Pp.pp =
  fun fmt tyn -> match tyn with
    | Ty.Tyvar tv -> fprintf fmt "@[<hov 2>Tyvar(%a)@]"
        print_tvsymbol tv
    | Ty.Tyapp (ts, tyl) -> fprintf fmt "@[<hov 2>Tyapp(%a,@ %a)@]"
        print_tysymbol ts
        (print_list print_ty) tyl

and print_mask : Ity.mask Pp.pp =
  fun fmt mask ->
    ignore mask;
    fprintf fmt "@[<hov 2>(mask)@]"

and print_effect : Ity.effect Pp.pp =
  fun fmt eff ->
    ignore eff;
    fprintf fmt "@[<hov 2>(effect)@]"

and print_theory : Theory.theory Pp.pp =
  fun fmt th ->
    fprintf fmt "@[<hov 2>theory{th_name=%a,@ th_path=%a,@ th_decls=%a}@]"
      print_ident th.Theory.th_name
      (print_list print_string) th.Theory.th_path
      (print_list print_tdecl) th.Theory.th_decls

and print_tdecl : Theory.tdecl Pp.pp =
  fun fmt td ->
    fprintf fmt "@[<hov 2>tdecl{td_node=%a,@ td_tag=%a}@]"
      print_tdecl_node td.Theory.td_node
      print_int td.Theory.td_tag

and print_tdecl_node : Theory.tdecl_node Pp.pp =
  fun fmt tdn ->
    ignore tdn;
    fprintf fmt "@[<hov 2>(tdecl_node)@]" (* TODO *)

and print_term : Term.term Pp.pp =
  fun fmt t ->
    fprintf fmt "@[<hov 2>term{t_node=%a,@ t_ty=%a,@ t_attrs=%a}@]"
      print_term_node t.Term.t_node
      (print_option print_ty) t.Term.t_ty
      (print_list print_attribute) (Ident.Sattr.elements t.Term.t_attrs)

and print_term_node : Term.term_node Pp.pp =
  fun fmt tn ->
    ignore tn;
    fprintf fmt "@[<hov 2>(term_node)@]"

and print_meta_decl : Pdecl.meta_decl Pp.pp =
  fun fmt md ->
    ignore md;
    fprintf fmt "@[<hov 2>meta_decl(...)@]"

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
  let handle_module m = printf "%a@\n@." print_pmodule m in
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
