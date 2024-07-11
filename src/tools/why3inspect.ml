(* Goal : print pmodule ast *)

open Format
open Why3
open Wstdlib

let usage_msg = "<file>\nParse and type check the given file, then display the AST"
let opt_max_indent = ref 200
let opt_margin = ref 300
let opt_unfold_namespace = ref false
let opt_unfold_region = ref false
let opt_unfold_effect = ref false
let opt_unfold_term = ref false
let opt_unfold_expr = ref false
let opt_unfold_ty = ref false
let opt_unfold_ity = ref false
let opt_unfold_cty = ref false
let opt_unfold_pvsymbol = ref false
let opt_unfold_vsymbol = ref false
let opt_unfold_rsymbol = ref false
let opt_unfold_lsymbol = ref false
let opt_unfold_ident = ref false
let opt_unfold_attribute = ref false
let opt_file = ref None

let add_opt_file file = opt_file := Some file

let option_list =
  let open Getopt in
  [ KLong "max-indent", Hnd1 (AInt, fun i -> opt_max_indent := i),
    "<characters> set the maximimum indentation limit";
    KLong "margin", Hnd1 (AInt, fun i -> opt_margin := i),
    "<characters> set the print margin";
    KLong "unfold-namespace", Hnd0 (fun () -> opt_unfold_namespace := true),
    "unfold the body of `namespace`";
    KLong "unfold-term", Hnd0 (fun () -> opt_unfold_term := true),
    "unfold the body of `term_node`";
    KLong "unfold-expr", Hnd0 (fun () -> opt_unfold_expr := true),
    "unfold the body of `expr_node`";
    KLong "unfold-region", Hnd0 (fun () -> opt_unfold_region := true),
    "unfold the body of `region`";
    KLong "unfold-effect", Hnd0 (fun () -> opt_unfold_effect := true),
    "unfold the body of `effect`";
    KLong "unfold-ty", Hnd0 (fun () -> opt_unfold_ty := true),
    "unfold the body of `ty`";
    KLong "unfold-ity", Hnd0 (fun () -> opt_unfold_ity := true),
    "unfold the body of `ity`";
    KLong "unfold-cty", Hnd0 (fun () -> opt_unfold_cty := true),
    "unfold the body of `cty`";
    KLong "unfold-pvsymbol", Hnd0 (fun () -> opt_unfold_pvsymbol := true),
    "unfold the body of `pvsymbol`";
    KLong "unfold-vsymbol", Hnd0 (fun () -> opt_unfold_vsymbol := true),
    "unfold the body of `vsymbol`";
    KLong "unfold-rsymbol", Hnd0 (fun () -> opt_unfold_rsymbol := true),
    "unfold the body of `rsymbol`";
    KLong "unfold-lsymbol", Hnd0 (fun () -> opt_unfold_lsymbol := true),
    "unfold the body of `lsymbol`";
    KLong "unfold-ident", Hnd0 (fun () -> opt_unfold_ident := true),
    "unfold the body of `ident`";
    KLong "unfold-attribute", Hnd0 (fun () -> opt_unfold_attribute := true),
    "unfold the body of `attribute`"
  ]

let _, env =
  eprintf "why3inspect.ml: initialize args@.";
  Whyconf.Args.initialize option_list add_opt_file usage_msg

let print_string : string Pp.pp =
  fun fmt s -> fprintf fmt "\"%a\"" Pp.print_string s

and print_int : int Pp.pp =
  fun fmt i -> fprintf fmt "%d" i

and print_bool : bool Pp.pp =
  fun fmt b -> fprintf fmt "%b" b

and print_list : 'a Pp.pp -> 'a list Pp.pp =
  fun printer fmt l -> fprintf fmt "[@[<hv>%a@]]"
    (Pp.print_list Pp.comma printer) l

and print_option : 'a Pp.pp -> 'a option Pp.pp =
  fun printer fmt o -> fprintf fmt "<@[<hv>%a@]>"
    (Pp.print_option printer) o

and print_pair : 'a Pp.pp -> 'b Pp.pp -> ('a * 'b) Pp.pp =
  fun a_printer b_printer fmt (a, b) -> fprintf fmt "(@[<hv>%a,@ %a@])"
    a_printer a
    b_printer b

and print_binding : 'a Pp.pp -> 'b Pp.pp -> ('a * 'b) Pp.pp =
  fun a_printer b_printer fmt (a, b) -> fprintf fmt "(@[<hv>%a@ =>@ %a@])"
    a_printer a
    b_printer b

(***********************)
(* AST PRETTY PRINTING *)
(***********************)

let rec print_pmodule : Pmodule.pmodule Pp.pp =
  fun fmt m ->
    fprintf fmt "pmodule{@[<hv>mod_theory=%a,@ mod_units=%a,@ mod_export=%a,@ mod_known=%a,@ mod_local=%a,@ mod_used=%a@]}"
      print_theory m.Pmodule.mod_theory
      (print_list print_mod_unit) m.Pmodule.mod_units
      print_namespace m.Pmodule.mod_export
      (print_list print_ident) (Ident.Mid.keys m.Pmodule.mod_known)
      (print_list print_ident) (Ident.Sid.elements m.Pmodule.mod_local)
      (print_list print_ident) (Ident.Sid.elements m.Pmodule.mod_used)

and print_mod_unit : Pmodule.mod_unit Pp.pp =
  fun fmt mu ->
    match mu with
    | Pmodule.Udecl d -> fprintf fmt "Udecl(@[<hv>%a@])"
        print_pdecl d
    | Pmodule.Uuse _ -> fprintf fmt "Uuse(...)"
    | Pmodule.Uclone mi -> fprintf fmt "Uclone(@[<hv>%a@])"
        print_mod_inst mi
    | Pmodule.Umeta _ -> fprintf fmt "Umeta(...)"
    | Pmodule.Uscope (s, mul) -> fprintf fmt "Uscope(@[<hv>%a,@ %a@])"
        print_string s
        (print_list print_mod_unit) mul

and print_namespace_unfold : Pmodule.namespace Pp.pp =
  fun fmt ns ->
    fprintf fmt "namespace{@[<hv>ns_ts=%a,@ ns_ps=%a,@ ns_xs=%a,@ ns_ns=%a@]}"
      (print_list (print_binding print_string print_itysymbol)) (Mstr.bindings ns.Pmodule.ns_ts)
      (print_list (print_binding print_string print_prog_symbol)) (Mstr.bindings ns.Pmodule.ns_ps)
      (print_list (print_binding print_string print_xsymbol)) (Mstr.bindings ns.Pmodule.ns_xs)
      (print_list (print_binding print_string print_namespace)) (Mstr.bindings ns.Pmodule.ns_ns)

and print_namespace_fold : Pmodule.namespace Pp.pp =
  fun fmt _ ->
    fprintf fmt "namespace{...}"

and print_namespace : Pmodule.namespace Pp.pp =
  fun fmt ns ->
    (if !opt_unfold_namespace then print_namespace_unfold else print_namespace_fold) fmt ns

and print_mod_inst : Pmodule.mod_inst Pp.pp =
  fun fmt mi ->
    fprintf fmt "mod_inst{@[<hv>mi_mod=%a@]}"
      print_pmodule mi.Pmodule.mi_mod

and print_pdecl : Pdecl.pdecl Pp.pp =
  fun fmt pd ->
    fprintf fmt "pdecl{@[<hv>pd_node=%a,@ pd_pure=%a,@ pd_meta=%a,@ pd_syms=%a,@ pd_news=%a,@ pd_tag=%a@]}"
      print_pdecl_node pd.Pdecl.pd_node
      (print_list print_decl) pd.Pdecl.pd_pure
      (print_list print_meta_decl) pd.Pdecl.pd_meta
      (print_list print_ident) (Ident.Sid.elements pd.Pdecl.pd_syms)
      (print_list print_ident) (Ident.Sid.elements pd.Pdecl.pd_news)
      print_int pd.Pdecl.pd_tag

and print_pdecl_node : Pdecl.pdecl_node Pp.pp =
  fun fmt pdn -> match pdn with
    | Pdecl.PDtype idl -> fprintf fmt "PDtype(@[<hv>%a@])"
        (print_list print_its_defn) idl
    | Pdecl.PDlet ld -> fprintf fmt "PDlet(@[<hv>%a@])"
        print_let_defn ld
    | Pdecl.PDexn xs -> fprintf fmt "PDexn(@[<hv>%a@])"
        print_xsymbol xs
    | Pdecl.PDpure -> fprintf fmt "PDPure"

and print_its_defn : Pdecl.its_defn Pp.pp =
  fun fmt itd ->
    fprintf fmt "its_defn{@[<hv>itd_its=%a,@ itd_fields=%a,@ itd_constructors=%a,@ itd_invariant=%a,@ itd_witness=%a@]}"
      print_itysymbol itd.Pdecl.itd_its
      (print_list print_rsymbol) itd.Pdecl.itd_fields
      (print_list print_rsymbol) itd.Pdecl.itd_constructors
      (print_list print_term) itd.Pdecl.itd_invariant
      (print_option print_expr) itd.Pdecl.itd_witness

and print_let_defn : Expr.let_defn Pp.pp =
  fun fmt ld -> match ld with
    | Expr.LDvar (pv, e) -> fprintf fmt "LDvar(@[<hv>%a,@ %a@])"
        print_pvsymbol pv
        print_expr e
    | Expr.LDsym (rs, ce) -> fprintf fmt "LDsym(@[<hv>%a,@ %a@])"
        print_rsymbol rs
        print_cexp ce
    | Expr.LDrec rdl -> fprintf fmt "LDrec(@[<hv>%a@])"
        (print_list print_rec_defn) rdl

and print_expr_unfold : Expr.expr Pp.pp =
  fun fmt e ->
    fprintf fmt "expr{@[<hv>e_node=%a,@ e_ity=%a,@ e_mask=%a,@ e_effect=%a,@ e_attrs=%a@]}"
      print_expr_node e.Expr.e_node
      print_ity e.Expr.e_ity
      print_mask e.Expr.e_mask
      print_effect e.Expr.e_effect
      (print_list print_attribute) (Ident.Sattr.elements e.Expr.e_attrs)

and print_expr_fold : Expr.expr Pp.pp =
  fun fmt _ ->
    fprintf fmt "expr{...}"

and print_expr : Expr.expr Pp.pp =
  fun fmt e ->
    (if !opt_unfold_expr then print_expr_unfold else print_expr_fold) fmt e

and print_expr_node : Expr.expr_node Pp.pp =
  fun fmt en -> match en with
    | Expr.Evar pv -> fprintf fmt "Evar(@[<hv>%a@])"
        print_pvsymbol pv
    | Expr.Econst c -> fprintf fmt "Econst(@[<hv>%a@])"
        Constant.print_def c
    | Expr.Eexec (ce, cty) -> fprintf fmt "Eexec(@[<hv>%a,@ %a@])"
        print_cexp ce
        print_cty cty
    | Expr.Eassign asgl -> fprintf fmt "Eassign(@[<hv>%a@])"
        (print_list print_assign) asgl
    | Expr.Elet (ld, e) -> fprintf fmt "Elet(@[<hv>%a,@ %a@])"
        print_let_defn ld
        print_expr e
    | Expr.Eif (e_cond, e_if, e_else) -> fprintf fmt "Eif(@[<hv>%a,@ %a,@ %a@])"
        print_expr e_cond
        print_expr e_if
        print_expr e_else
    | Expr.Ematch (e, rbl, ebm) -> fprintf fmt "Ematch(@[<hv>%a,@ %a,@ %a@])"
        print_expr e
        (print_list print_reg_branch) rbl
        (print_list (print_binding print_xsymbol print_exn_branch)) (Ity.Mxs.bindings ebm)
    | Expr.Ewhile (e_cond, invl, varl, e_body) -> fprintf fmt "Ewhile(@[<hv>%a,@ %a,@ %a,@ %a@])"
        print_expr e_cond
        (print_list print_invariant) invl
        (print_list print_variant) varl
        print_expr e_body
    | Expr.Efor (pv0, bounds, pv1, invl, e) -> fprintf fmt "Efor(@[<hv>%a,@ %a,@ %a,@ %a,@ %a@])"
        print_pvsymbol pv0
        print_for_bounds bounds
        print_pvsymbol pv1
        (print_list print_invariant) invl
        print_expr e
    | Expr.Eraise (xs, e) -> fprintf fmt "Eraise(@[<hv>%a,@ %a@])"
        print_xsymbol xs
        print_expr e
    | Expr.Eexn (xs, e) -> fprintf fmt "Eexn(@[<hv>%a,@ %a@])"
        print_xsymbol xs
        print_expr e
    | Expr.Eassert (ak, t) -> fprintf fmt "Eassert(@[<hv>%a,@ %a@])"
        print_assertion_kind ak
        print_term t
    | Expr.Eghost e -> fprintf fmt "Eghost(@[<hv>%a@])"
        print_expr e
    | Expr.Epure t -> fprintf fmt "Epure(@[<hv>%a@])"
        print_term t
    | Expr.Eabsurd -> fprintf fmt "Eabsurd"

and print_cexp : Expr.cexp Pp.pp =
  fun fmt ce ->
    fprintf fmt "cexp{@[<hv>c_node=%a,@ c_cty=%a@]}"
      print_cexp_node ce.Expr.c_node
      print_cty ce.Expr.c_cty

and print_cexp_node : Expr.cexp_node Pp.pp =
  fun fmt cen -> match cen with
    | Expr.Capp (rs, pvl) -> fprintf fmt "Capp(@[<hv>%a,@ %a@])"
        print_rsymbol rs
        (print_list print_pvsymbol) pvl
    | Expr.Cpur (ls, pvl) -> fprintf fmt "Cpur(@[<hv>%a,@ %a@])"
        print_lsymbol ls
        (print_list print_pvsymbol) pvl
    | Expr.Cfun e -> fprintf fmt "Cfun(@[<hv>%a@])"
        print_expr e
    | Expr.Cany -> fprintf fmt "Cany"

and print_assign : Expr.assign Pp.pp =
  fun fmt asg ->
    let (pv_reg, rs_field, pv_val) = asg in
    fprintf fmt "assign(@[<hv>%a,@ %a,@ %a@])"
      print_pvsymbol pv_reg
      print_rsymbol rs_field
      print_pvsymbol pv_val

and print_reg_branch : Expr.reg_branch Pp.pp =
  fun fmt rb ->
    let (prog_pat, e) = rb in
    fprintf fmt "reg_branch(@[<hv>%a,@ %a@])"
      print_prog_pattern prog_pat
      print_expr e

and print_exn_branch : Expr.exn_branch Pp.pp =
  fun fmt eb ->
    let (pvl, e) = eb in
    fprintf fmt "exn_branch(@[<hv>%a,@ %a@])"
      (print_list print_pvsymbol) pvl
      print_expr e

and print_assertion_kind : Expr.assertion_kind Pp.pp =
  fun fmt ak -> match ak with
    | Expr.Assert -> fprintf fmt "Assert"
    | Expr.Assume -> fprintf fmt "Assume"
    | Expr.Check -> fprintf fmt "Check"

and print_prog_pattern : Expr.prog_pattern Pp.pp =
  fun fmt p ->
    fprintf fmt "prog_pattern{[@<hv>pp_pat=%a,@ pp_ity=%a,@ pp_mask=%a,@ pp_fail=%a@]}"
      print_pattern p.Expr.pp_pat
      print_ity p.Expr.pp_ity
      print_mask p.Expr.pp_mask
      print_pat_ghost p.Expr.pp_fail

and print_pat_ghost : Expr.pat_ghost Pp.pp =
  fun fmt pg -> match pg with
    | Expr.PGfail -> fprintf fmt "PGfail"
    | Expr.PGlast -> fprintf fmt "PGlast"
    | Expr.PGnone -> fprintf fmt "PGnone"

and print_invariant : Expr.invariant Pp.pp =
  fun fmt iv ->
    fprintf fmt "invariant(@[<hv>%a@])"
      print_term iv

and print_rec_defn : Expr.rec_defn Pp.pp =
  fun fmt rd -> fprintf fmt "rec_defn{@[<hv>rec_sym=%a,@ rec_rsym=%a,@ rec_fun=%a,@ rec_varl=%a@]}"
    print_rsymbol rd.Expr.rec_sym
    print_rsymbol rd.Expr.rec_rsym
    print_cexp rd.Expr.rec_fun
    (print_list print_variant) rd.Expr.rec_varl

and print_variant : Expr.variant Pp.pp =
  fun fmt v ->
    let (t, ls_opt) = v in
    fprintf fmt "variant(@[<hv>%a,@ %a@])"
      print_term t
      (print_option print_lsymbol) ls_opt

and print_for_bounds : Expr.for_bounds Pp.pp =
  fun fmt fb ->
    let (pv0, dir, pv1) = fb in
    fprintf fmt "for_bounds(@[<hv>%a,@ %a,@ %a@])"
      print_pvsymbol pv0
      print_for_direction dir
      print_pvsymbol pv1

and print_for_direction : Expr.for_direction Pp.pp =
  fun fmt dir -> match dir with
    | Expr.To -> fprintf fmt "To"
    | Expr.DownTo -> fprintf fmt "DownTo"

and print_pattern : Term.pattern Pp.pp =
  fun fmt pat ->
    fprintf fmt "pattern{@[<hv>pat_node=%a,@ pat_vars=%a,@ pat_ty=%a@]}"
      print_pattern_node pat.Term.pat_node
      (print_list print_vsymbol) (Term.Svs.elements pat.Term.pat_vars)
      print_ty pat.Term.pat_ty

and print_pattern_node : Term.pattern_node Pp.pp =
  fun fmt pn -> match pn with
    | Term.Pwild -> fprintf fmt "Pwild"
    | Term.Pvar vs -> fprintf fmt "Pvar(@[<hv>%a@])"
        print_vsymbol vs
    | Term.Papp (ls, patl) -> fprintf fmt "Papp(@[<hv>%a,@ %a@])"
        print_lsymbol ls
        (print_list print_pattern) patl
    | Term.Por (pat0, pat1) -> fprintf fmt "Por(@[<hv>%a,@ %a@])"
        print_pattern pat0
        print_pattern pat1
    | Term.Pas (pat, vs) -> fprintf fmt "Pas(@[<hv>%a,@ %a@])"
        print_pattern pat
        print_vsymbol vs

and print_decl : Decl.decl Pp.pp =
  fun fmt d ->
    fprintf fmt "decl{@[<hv>d_node=%a,@ d_news=%a@]}"
      print_decl_node d.Decl.d_node
      (print_list print_ident) (Ident.Sid.elements d.Decl.d_news)

and print_decl_node : Decl.decl_node Pp.pp =
  fun fmt dn -> match dn with
    | Decl.Dtype ts -> fprintf fmt "Dtype(@[<hv>%a@])"
        print_tysymbol ts
    | Decl.Ddata ddl -> fprintf fmt "Ddata(@[<hv>%a@])"
        (print_list print_data_decl) ddl
    | Decl.Dparam ls -> fprintf fmt "Dparam(@[<hv>%a@])"
        print_lsymbol ls
    | Decl.Dlogic ldl -> fprintf fmt "Dlogic(@[<hv>%a@])"
        (print_list print_logic_decl) ldl
    | Decl.Dind il -> fprintf fmt "Dind(@[<hv>%a@])"
        print_ind_list il
    | Decl.Dprop pd -> fprintf fmt "Dprop(@[<hv>%a@])"
        print_prop_decl pd

and print_data_decl : Decl.data_decl Pp.pp =
  fun fmt dd ->
    let (ts, cl) = dd in
    fprintf fmt "data_decl(@[<hv>%a,@ %a@])"
      print_tysymbol ts
      (print_list print_constructor) cl

and print_constructor : Decl.constructor Pp.pp =
  fun fmt c ->
    let (ls, ls_optl) = c in
    fprintf fmt "constructor(@[<hv>%a,@ %a@])"
      print_lsymbol ls
      (print_list (print_option print_lsymbol)) ls_optl

and print_logic_decl : Decl.logic_decl Pp.pp =
  fun fmt ld ->
    let (ls, lsd) = ld in
    fprintf fmt "logic_decl(@[<hv>%a,@ %a@])"
      print_lsymbol ls
      print_ls_defn lsd

and print_ls_defn : Decl.ls_defn Pp.pp =
  fun fmt lsd ->
    let (vsl, t) = Decl.open_ls_defn lsd in
    fprintf fmt "ls_defn(@[<hv>%a,@ %a@])"
      (print_list print_vsymbol) vsl
      print_term t

and print_ind_list : Decl.ind_list Pp.pp =
  fun fmt il ->
    let (i_sign, idl) = il in
    fprintf fmt "ind_list(@[<hv>%a,@ %a@])"
      print_ind_sign i_sign
      (print_list print_ind_decl) idl

and print_ind_sign : Decl.ind_sign Pp.pp =
  fun fmt i_sign -> match i_sign with
    | Decl.Ind -> fprintf fmt "Ind"
    | Decl.Coind -> fprintf fmt "Coind"

and print_ind_decl : Decl.ind_decl Pp.pp =
  fun fmt id ->
    let (ls, pr_t_l) = id in
    fprintf fmt "ind_decl(@[<hv>%a,@ %a@])"
      print_lsymbol ls
      (print_list (print_pair print_prsymbol print_term)) pr_t_l

and print_prop_decl : Decl.prop_decl Pp.pp =
  fun fmt pd ->
    let (pk, pr, t) = pd in
    fprintf fmt "prop_decl(@[<hv>%a,@ %a,@ %a@])"
      print_prop_kind pk
      print_prsymbol pr
      print_term t

and print_prop_kind : Decl.prop_kind Pp.pp =
  fun fmt pk -> match pk with
    | Decl.Plemma -> fprintf fmt "Plemma"
    | Decl.Paxiom -> fprintf fmt "Paxiom"
    | Decl.Pgoal -> fprintf fmt "Pgoal"

and print_prsymbol : Decl.prsymbol Pp.pp =
  fun fmt pr ->
    fprintf fmt "prsymbol{@[<hv>pr_name=%a@]}"
      print_ident pr.Decl.pr_name

and print_xsymbol : Ity.xsymbol Pp.pp =
  fun fmt xs ->
    fprintf fmt "xsymbol{@[<hv>xs_name=%a,@ xs_ity=%a,@ xs_mask=%a@]}"
      print_ident xs.Ity.xs_name
      print_ity xs.Ity.xs_ity
      print_mask xs.Ity.xs_mask

and print_rsymbol_unfold : Expr.rsymbol Pp.pp =
  fun fmt rs ->
    fprintf fmt "rsymbol{@[<hv>rs_name=%a,@ rs_cty=%a,@ rs_logic=%a,@ rs_field=%a@]}"
      print_ident rs.Expr.rs_name
      print_cty rs.Expr.rs_cty
      print_rs_logic rs.Expr.rs_logic
      (print_option print_pvsymbol) rs.Expr.rs_field

and print_rsymbol_fold : Expr.rsymbol Pp.pp =
  fun fmt rs ->
    fprintf fmt "%s" rs.Expr.rs_name.Ident.id_string

and print_rsymbol : Expr.rsymbol Pp.pp =
  fun fmt rs ->
    (if !opt_unfold_rsymbol then print_rsymbol_unfold else print_rsymbol_fold) fmt rs

and print_itysymbol : Ity.itysymbol Pp.pp =
  fun fmt its ->
    fprintf fmt "itysymbol{@[<hv>its_ts=%a,@ its_nonfree=%a,@ its_private=%a,@ its_mutable=%a,@ \
      its_fragile=%a,@ its_mfields=%a,@ its_ofields=%a,@ its_regions=%a,@ its_def=%a@]}"
      print_tysymbol its.Ity.its_ts
      print_bool its.Ity.its_nonfree
      print_bool its.Ity.its_private
      print_bool its.Ity.its_mutable
      print_bool its.Ity.its_fragile
      (print_list print_pvsymbol) its.Ity.its_mfields
      (print_list print_pvsymbol) its.Ity.its_ofields
      (print_list print_region) its.Ity.its_regions
      (print_type_def print_ity) its.Ity.its_def

and print_region_unfold : Ity.region Pp.pp =
  fun fmt reg ->
    fprintf fmt "region{@[<hv>reg_name=%a,@ reg_its=%a,@ reg_args=%a,@ reg_regs=%a@]}"
      print_ident reg.Ity.reg_name
      print_itysymbol reg.Ity.reg_its
      (print_list print_ity) reg.Ity.reg_args
      (print_list print_ity) reg.Ity.reg_regs

and print_region_fold : Ity.region Pp.pp =
  fun fmt _ ->
    fprintf fmt "region{...}"

and print_region : Ity.region Pp.pp =
  fun fmt reg ->
    (if !opt_unfold_region then print_region_unfold else print_region_fold) fmt reg

and print_type_def : 'a Pp.pp -> 'a Ty.type_def Pp.pp =
  fun printer fmt def -> match def with
    | Ty.NoDef -> fprintf fmt "NoDef"
    | Ty.Alias a -> fprintf fmt "Alias(@[<hv>%a@])"
        printer a
    | Ty.Range _ -> assert false
    | Ty.Float _ -> assert false

and print_tysymbol : Ty.tysymbol Pp.pp =
  fun fmt ts ->
    fprintf fmt "tysymbol{@[<hv>ts_name=%a,@ ts_args=%a@]}"
      print_ident ts.Ty.ts_name
      (print_list print_tvsymbol) ts.Ty.ts_args

and print_tvsymbol : Ty.tvsymbol Pp.pp =
  fun fmt tv ->
    fprintf fmt "tvsymbol{@[<hv>tv_name=%a@]}"
      print_ident tv.Ty.tv_name

and print_ity_unfold : Ity.ity Pp.pp =
  fun fmt ity ->
    fprintf fmt "ity{@[<hv>ity_node=%a,@ ity_pure=%a@]}"
      print_ity_node ity.Ity.ity_node
      print_bool ity.Ity.ity_pure

and print_ity_fold : Ity.ity Pp.pp =
  fun fmt _ ->
    fprintf fmt "ity{...}"

and print_ity : Ity.ity Pp.pp =
  fun fmt ity ->
    (if !opt_unfold_ity then print_ity_unfold else print_ity_fold) fmt ity

and print_ity_node : Ity.ity_node Pp.pp =
  fun fmt ityn -> match ityn with
    | Ity.Ityreg reg -> fprintf fmt "Ityreg(@[<hv>%a@])"
        print_region reg
    | Ity.Ityapp (its, ityl0, ityl1) -> fprintf fmt "Ityapp(@[<hv>%a,@ %a,@ %a@])"
        print_itysymbol its
        (print_list print_ity) ityl0
        (print_list print_ity) ityl1
    | Ity.Ityvar tv -> fprintf fmt "Ityvar(@[<hv>%a@])"
        print_tvsymbol tv

and print_cty_unfold : Ity.cty Pp.pp =
  fun fmt cty ->
    fprintf fmt "cty{@[<hv>cty_args=%a,@ cty_pre=%a,@ cty_post=%a,@ cty_xpost=%a,@ cty_oldies=%a,@ cty_effect=%a,@ cty_result=%a,@ cty_mask=%a@]}"
      (print_list print_pvsymbol) cty.Ity.cty_args
      (print_list print_pre) cty.Ity.cty_pre
      (print_list print_post) cty.Ity.cty_post
      (print_list print_xsymbol) (Ity.Mxs.keys cty.Ity.cty_xpost)
      (print_list print_pvsymbol) (Ity.Mpv.keys cty.Ity.cty_oldies)
      print_effect cty.Ity.cty_effect
      print_ity cty.Ity.cty_result
      print_mask cty.Ity.cty_mask

and print_cty_fold : Ity.cty Pp.pp =
  fun fmt _ ->
    fprintf fmt "cty{...}"

and print_cty : Ity.cty Pp.pp =
  fun fmt cty ->
    (if !opt_unfold_cty then print_cty_unfold else print_cty_fold) fmt cty

and print_pre : Ity.pre Pp.pp =
  fun fmt pre ->
    fprintf fmt "pre(@[<hv>%a@])"
      print_term pre

and print_post : Ity.post Pp.pp =
  fun fmt post ->
    fprintf fmt "post(@[<hv>%a@])"
      print_term post

and print_rs_logic : Expr.rs_logic Pp.pp =
  fun fmt logic ->
    match logic with
      | Expr.RLnone -> fprintf fmt "RLnone"
      | Expr.RLpv pvs -> fprintf fmt "RLpv(@[<hv>%a@])"
          print_pvsymbol pvs
      | Expr.RLls ls -> fprintf fmt "RLls(@[<hv>%a@])"
          print_lsymbol ls
      | Expr.RLlemma -> fprintf fmt "RLlemma"

and print_lsymbol_unfold : Term.lsymbol Pp.pp =
  fun fmt ls ->
    fprintf fmt "lsymbol{@[<hv>ls_name=%a,@ ls_args=%a,@ ls_value=%a,@ ls_constr=%a,@ ls_proj=%a@]}"
      print_ident ls.Term.ls_name
      (print_list print_ty) ls.Term.ls_args
      (print_option print_ty) ls.Term.ls_value
      print_int ls.Term.ls_constr
      print_bool ls.Term.ls_proj

and print_lsymbol_fold : Term.lsymbol Pp.pp =
  fun fmt ls ->
    fprintf fmt "%s" ls.Term.ls_name.Ident.id_string

and print_lsymbol : Term.lsymbol Pp.pp =
  fun fmt ls ->
    (if !opt_unfold_lsymbol then print_lsymbol_unfold else print_lsymbol_fold) fmt ls

and print_pvsymbol_unfold : Ity.pvsymbol Pp.pp =
  fun fmt pv ->
    fprintf fmt "pvsymbol{@[<hv>pv_vs=%a,@ pv_ity=%a,@ pv_ghost=%a@]}"
      print_vsymbol pv.Ity.pv_vs
      print_ity pv.Ity.pv_ity
      print_bool pv.Ity.pv_ghost

and print_pvsymbol_fold : Ity.pvsymbol Pp.pp =
  fun fmt pv ->
    fprintf fmt "%s" pv.Ity.pv_vs.Term.vs_name.Ident.id_string

and print_pvsymbol : Ity.pvsymbol Pp.pp =
  fun fmt pv ->
    (if !opt_unfold_pvsymbol then print_pvsymbol_unfold else print_pvsymbol_fold) fmt pv

and print_vsymbol_unfold : Term.vsymbol Pp.pp =
  fun fmt vs ->
    fprintf fmt "vsymbol{@[<hv>vs_name=%a,@ vs_ty=%a@]}"
      print_ident vs.Term.vs_name
      print_ty vs.Term.vs_ty

and print_vsymbol_fold : Term.vsymbol Pp.pp =
  fun fmt vs ->
    fprintf fmt "%s" vs.Term.vs_name.Ident.id_string

and print_vsymbol : Term.vsymbol Pp.pp =
  fun fmt vs ->
    (if !opt_unfold_vsymbol then print_vsymbol_unfold else print_vsymbol_fold) fmt vs

and print_ident_unfold : Ident.ident Pp.pp =
  fun fmt i ->
    fprintf fmt "ident{@[<hv>id_string=%a,@ id_attrs=%a,@ id_tag=%a@]}"
      print_string i.Ident.id_string
      (print_list print_attribute) (Ident.Sattr.elements i.Ident.id_attrs)
      print_int (Weakhtbl.tag_hash i.Ident.id_tag)

and print_ident_fold : Ident.ident Pp.pp =
  fun fmt i ->
    fprintf fmt "%s" i.Ident.id_string

and print_ident : Ident.ident Pp.pp =
  fun fmt i ->
    (if !opt_unfold_ident then print_ident_unfold else print_ident_fold) fmt i

and print_attribute_unfold : Ident.attribute Pp.pp =
  fun fmt attr ->
    fprintf fmt "attribute{@[<hv>attr_string=%a@]}"
      print_string attr.Ident.attr_string

and print_attribute_fold : Ident.attribute Pp.pp =
  fun fmt attr ->
    fprintf fmt "%a" Pp.print_string attr.Ident.attr_string

and print_attribute : Ident.attribute Pp.pp =
  fun fmt attr ->
    (if !opt_unfold_attribute then print_attribute_unfold else print_attribute_fold) fmt attr

and print_ty_unfold : Ty.ty Pp.pp =
  fun fmt ty ->
    fprintf fmt "ty{@[<hv>ty_node=%a@]}"
      print_ty_node ty.Ty.ty_node

and print_ty_fold : Ty.ty Pp.pp =
  fun fmt _ ->
    fprintf fmt "ty{...}"

and print_ty : Ty.ty Pp.pp =
  fun fmt ty ->
    (if !opt_unfold_ty then print_ty_unfold else print_ty_fold) fmt ty

and print_ty_node : Ty.ty_node Pp.pp =
  fun fmt tyn -> match tyn with
    | Ty.Tyvar tv -> fprintf fmt "Tyvar(@[<hv>%a@])"
        print_tvsymbol tv
    | Ty.Tyapp (ts, tyl) -> fprintf fmt "Tyapp(@[<hv>%a,@ %a@])"
        print_tysymbol ts
        (print_list print_ty) tyl

and print_mask : Ity.mask Pp.pp =
  fun fmt mask -> match mask with
    | Ity.MaskVisible -> fprintf fmt "MaskVisible"
    | Ity.MaskTuple ml -> fprintf fmt "MaskTuple(@[<hv>%a@])"
        (print_list print_mask) ml
    | Ity.MaskGhost -> fprintf fmt "MaskGhost"

and print_effect_unfold : Ity.effect Pp.pp =
  fun fmt eff ->
    let eff_writes_printer = print_list (print_binding print_region (print_list print_pvsymbol)) in
    let eff_writes = List.map
      (fun (reg, pvss) -> reg, Ity.Spv.elements pvss)
      (Ity.Mreg.bindings eff.Ity.eff_writes) in
    fprintf fmt "effect{@[<hv>eff_reads=%a,@ eff_writes=%a,@ eff_taints=%a,@ eff_covers=%a,@ eff_resets=%a,@ \
      eff_raises=%a,@ eff_spoils=%a,@ eff_oneway=%a,@ eff_ghost=%a@]}"
      (print_list print_pvsymbol) (Ity.Spv.elements eff.Ity.eff_reads)
      eff_writes_printer eff_writes
      (print_list print_region) (Ity.Sreg.elements eff.Ity.eff_taints)
      (print_list print_region) (Ity.Sreg.elements eff.Ity.eff_covers)
      (print_list print_region) (Ity.Sreg.elements eff.Ity.eff_resets)
      (print_list print_xsymbol) (Ity.Sxs.elements eff.Ity.eff_raises)
      (print_list print_tvsymbol) (Ty.Stv.elements eff.Ity.eff_spoils)
      print_oneway eff.Ity.eff_oneway
      print_bool eff.Ity.eff_ghost

and print_effect_fold : Ity.effect Pp.pp =
  fun fmt _ ->
    fprintf fmt "effect{...}"

and print_effect : Ity.effect Pp.pp =
  fun fmt eff ->
    (if !opt_unfold_effect then print_effect_unfold else print_effect_fold) fmt eff

and print_oneway : Ity.oneway Pp.pp =
  fun fmt ow -> match ow with
    | Ity.Total -> fprintf fmt "Total"
    | Ity.Partial -> fprintf fmt "Partial"
    | Ity.Diverges -> fprintf fmt "Diverges"

and print_theory : Theory.theory Pp.pp =
  fun fmt th ->
    fprintf fmt "theory{@[<hv>th_name=%a,@ th_path=%a,@ th_decls=%a@]}"
      print_ident th.Theory.th_name
      (print_list print_string) th.Theory.th_path
      (print_list print_tdecl) th.Theory.th_decls

and print_tdecl : Theory.tdecl Pp.pp =
  fun fmt td ->
    fprintf fmt "tdecl{@[<hv>td_node=%a@]}"
      print_tdecl_node td.Theory.td_node

and print_tdecl_node : Theory.tdecl_node Pp.pp =
  fun fmt tdn -> match tdn with
    | Theory.Decl d -> fprintf fmt "Decl(@[<hv>%a@])"
        print_decl d
    | Theory.Use _ -> fprintf fmt "Use(...)"
    | Theory.Clone _ -> fprintf fmt "Clone(...)"
    | Theory.Meta _ -> fprintf fmt "Meta(...)"

and print_term_unfold : Term.term Pp.pp =
  fun fmt t ->
    fprintf fmt "term{@[<hv>t_node=%a,@ t_ty=%a,@ t_attrs=%a@]}"
      print_term_node t.Term.t_node
      (print_option print_ty) t.Term.t_ty
      (print_list print_attribute) (Ident.Sattr.elements t.Term.t_attrs)

and print_term_fold : Term.term Pp.pp =
  fun fmt _ ->
    fprintf fmt "term{...}"

and print_term : Term.term Pp.pp =
  fun fmt t ->
    (if !opt_unfold_term then print_term_unfold else print_term_fold) fmt t

and print_term_node : Term.term_node Pp.pp =
  fun fmt tn -> match tn with
    | Term.Tvar vs -> fprintf fmt "Tvar(@[<hv>%a@])"
        print_vsymbol vs
    | Term.Tconst c -> fprintf fmt "Tconst(@[<hv>%a@])"
        Constant.print_def c
    | Term.Tapp (ls, tl) -> fprintf fmt "Tapp(@[<hv>%a,@ %a@])"
        print_lsymbol ls
        (print_list print_term) tl
    | Term.Tif (t_cond, t_if, t_else) -> fprintf fmt "Tif(@[<hv>%a,@ %a,@ %a@])"
        print_term t_cond
        print_term t_if
        print_term t_else
    | Term.Tlet (t, tb) -> fprintf fmt "Tlet(@[<hv>%a,@ %a@])"
        print_term t
        print_term_bound tb
    | Term.Tcase (t, tbl) -> fprintf fmt "Tcase(@[<hv>%a,@ %a@])"
        print_term t
        (print_list print_term_branch) tbl
    | Term.Teps tb -> fprintf fmt "Teps(@[<hv>%a@])"
        print_term_bound tb
    | Term.Tquant (q, tq) -> fprintf fmt "Tquant(@[<hv>%a,@ %a@])"
        print_quant q
        print_term_quant tq
    | Term.Tbinop (op, t0, t1) -> fprintf fmt "Tbinop(@[<hv>%a,@ %a,@ %a@])"
        print_binop op
        print_term t0
        print_term t1
    | Term.Tnot t -> fprintf fmt "Tnot(@[<hv>%a@])"
        print_term t
    | Term.Ttrue -> fprintf fmt "Ttrue"
    | Term.Tfalse -> fprintf fmt "Tfalse"

and print_term_bound : Term.term_bound Pp.pp =
  fun fmt tb ->
    let (vs, t) = Term.t_open_bound tb in
    fprintf fmt "term_bound(@[<hv>%a,@ %a@])"
      print_vsymbol vs
      print_term t

and print_term_branch : Term.term_branch Pp.pp =
  fun fmt tb ->
    let (pat, t) = Term.t_open_branch tb in
    fprintf fmt "term_branch(@[<hv>%a,@ %a@])"
      print_pattern pat
      print_term t

and print_quant : Term.quant Pp.pp =
  fun fmt q -> match q with
    | Term.Tforall -> fprintf fmt "Tforall"
    | Term.Texists -> fprintf fmt "Texists"

and print_term_quant : Term.term_quant Pp.pp =
  fun fmt tq ->
    let (vsl, trigger, t) = Term.t_open_quant tq in
    fprintf fmt "term_quant(@[<hv>%a,@ %a,@ %a@])"
      (print_list print_vsymbol) vsl
      print_trigger trigger
      print_term t

and print_trigger : Term.trigger Pp.pp =
  fun fmt trigger ->
    fprintf fmt "trigger(@[<hv>%a@])"
      (print_list (print_list print_term)) trigger

and print_binop : Term.binop Pp.pp =
  fun fmt op -> match op with
    | Term.Tand -> fprintf fmt "Tand"
    | Term.Tor -> fprintf fmt "Tor"
    | Term.Timplies -> fprintf fmt "Timplies"
    | Term.Tiff -> fprintf fmt "Tiff"

and print_meta_decl : Pdecl.meta_decl Pp.pp =
  fun fmt _ ->
    fprintf fmt "meta_decl(...)"

and print_prog_symbol : Pmodule.prog_symbol Pp.pp =
  fun fmt ps -> match ps with
    | Pmodule.PV pv -> fprintf fmt "PV(@[<hv>%a@])"
        print_pvsymbol pv
    | Pmodule.RS rs -> fprintf fmt "RS(@[<hv>%a@])"
        print_rsymbol rs
    | Pmodule.OO srs -> fprintf fmt "OO(@[<hv>%a@])"
        (print_list print_rsymbol) (Expr.Srs.elements srs)

let handle_no_file () =
  Whyconf.Args.exit_with_usage usage_msg

let handle_file file =
  let handle_module m = printf "%a@." print_pmodule m in
  let (modules, _) = Env.read_file Pmodule.mlw_language env file in
  let modules = Mstr.values modules in
  printf "FILE: %s@\n@." file;
  List.iter handle_module modules

let () =
  eprintf "why3inspect.ml: handle input file@.";
  Format.safe_set_geometry ~max_indent:!opt_max_indent ~margin:!opt_margin;
  try
    match !opt_file with
      | None -> handle_no_file ()
      | Some file -> handle_file file
  with e when not (Debug.test_flag Debug.stack_trace) ->
    eprintf "%a@." Exn_printer.exn_printer e;
    exit 1
