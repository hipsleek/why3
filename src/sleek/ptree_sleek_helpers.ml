open Wstdlib
open Ptree_sleek

exception IncompatibleComponent

let drop_sleek_in_spec { sp_pre; sp_post; sp_xpost; sp_reads; sp_writes; sp_alias;
                         sp_variant; sp_checkrw; sp_diverge; sp_partial; _ } =
  Ptree.{ sp_pre; sp_post; sp_xpost; sp_reads; sp_writes; sp_alias;
          sp_variant; sp_checkrw; sp_diverge; sp_partial }

let rec drop_sleek_in_expr { expr_desc; expr_loc } =
  let expr_desc_no_sleek = drop_sleek_in_expr_desc expr_desc in
  Ptree.{ expr_desc = expr_desc_no_sleek; expr_loc }

and drop_sleek_in_expr_desc = function
  | Eref -> Ptree.Eref
  | Etrue -> Ptree.Etrue
  | Efalse -> Ptree.Efalse
  | Econst c -> Ptree.Econst c
  | Eident qid -> Ptree.Eident qid
  | Easref qid -> Ptree.Easref qid
  | Eidapp (qid, exprs) ->
      let exprs_no_sleek = List.map drop_sleek_in_expr exprs in
      Ptree.Eidapp (qid, exprs_no_sleek)
  | Eapply (expr0, expr1) ->
      let expr0_no_sleek = drop_sleek_in_expr expr0 in
      let expr1_no_sleek = drop_sleek_in_expr expr1 in
      Ptree.Eapply (expr0_no_sleek, expr1_no_sleek)
  | Einfix (expr0, qid, expr1) ->
      let expr0_no_sleek = drop_sleek_in_expr expr0 in
      let expr1_no_sleek = drop_sleek_in_expr expr1 in
      Ptree.Einfix (expr0_no_sleek, qid, expr1_no_sleek)
  | Einnfix (expr0, qid, expr1) ->
      let expr0_no_sleek = drop_sleek_in_expr expr0 in
      let expr1_no_sleek = drop_sleek_in_expr expr1 in
      Ptree.Einnfix (expr0_no_sleek, qid, expr1_no_sleek)
  | Elet (id, ghost, kind, expr0, expr1) ->
      let expr0_no_sleek = drop_sleek_in_expr expr0 in
      let expr1_no_sleek = drop_sleek_in_expr expr1 in
      Ptree.Elet (id, ghost, kind, expr0_no_sleek, expr1_no_sleek)
  | Erec (defs, expr) ->
      let defs_no_sleek = List.map drop_sleek_in_fundef defs in
      let expr_no_sleek = drop_sleek_in_expr expr in
      Ptree.Erec (defs_no_sleek, expr_no_sleek)
  | Efun (binders, ty_opt, pat, mask, spec, expr) ->
      let spec_no_sleek = drop_sleek_in_spec spec in
      let expr_no_sleek = drop_sleek_in_expr expr in
      Ptree.Efun (binders, ty_opt, pat, mask, spec_no_sleek, expr_no_sleek)
  | Eany (params, kind, ty_opt, pat, mask, spec) ->
      let spec_no_sleek = drop_sleek_in_spec spec in
      Ptree.Eany (params, kind, ty_opt, pat, mask, spec_no_sleek)
  | Etuple (exprs) ->
      let exprs_no_sleek = List.map drop_sleek_in_expr exprs in
      Ptree.Etuple exprs_no_sleek
  | Erecord fields ->
      let fields_no_sleek = List.map drop_sleek_in_field fields in
      Ptree.Erecord fields_no_sleek
  | Eupdate (expr, fields) ->
      let expr_no_sleek = drop_sleek_in_expr expr in
      let fields_no_sleek = List.map drop_sleek_in_field fields in
      Ptree.Eupdate (expr_no_sleek, fields_no_sleek)
  | Eassign assignments ->
      let drop_sleek (expr0, qid_opt, expr1) =
        let expr0_no_sleek = drop_sleek_in_expr expr0 in
        let expr1_no_sleek = drop_sleek_in_expr expr1 in
        expr0_no_sleek, qid_opt, expr1_no_sleek
      in
      let assignments_no_sleek = List.map drop_sleek assignments in
      Ptree.Eassign assignments_no_sleek
  | Esequence (expr0, expr1) ->
      let expr0_no_sleek = drop_sleek_in_expr expr0 in
      let expr1_no_sleek = drop_sleek_in_expr expr1 in
      Ptree.Esequence (expr0_no_sleek, expr1_no_sleek)
  | Eif (expr0, expr1, expr2) ->
      let expr0_no_sleek = drop_sleek_in_expr expr0 in
      let expr1_no_sleek = drop_sleek_in_expr expr1 in
      let expr2_no_sleek = drop_sleek_in_expr expr2 in
      Ptree.Eif (expr0_no_sleek, expr1_no_sleek, expr2_no_sleek)
  | Ewhile (expr0, inv, var, expr1) ->
      let expr0_no_sleek = drop_sleek_in_expr expr0 in
      let expr1_no_sleek = drop_sleek_in_expr expr1 in
      Ptree.Ewhile (expr0_no_sleek, inv, var, expr1_no_sleek)
  | Eand (expr0, expr1) ->
      let expr0_no_sleek = drop_sleek_in_expr expr0 in
      let expr1_no_sleek = drop_sleek_in_expr expr1 in
      Ptree.Eand (expr0_no_sleek, expr1_no_sleek)
  | Eor (expr0, expr1) ->
      let expr0_no_sleek = drop_sleek_in_expr expr0 in
      let expr1_no_sleek = drop_sleek_in_expr expr1 in
      Ptree.Eor (expr0_no_sleek, expr1_no_sleek)
  | Enot expr ->
      let expr_no_sleek = drop_sleek_in_expr expr in
      Ptree.Enot expr_no_sleek
  | Ematch (expr, reg_branches, exn_branches) ->
      let drop_sleek_in_reg_branch (pat, expr) =
        pat, drop_sleek_in_expr expr
      in
      let drop_sleek_in_exn_branch (qid, pat_opt, expr) =
        qid, pat_opt, drop_sleek_in_expr expr
      in
      let expr_no_sleek = drop_sleek_in_expr expr in
      let reg_branches_no_sleek = List.map drop_sleek_in_reg_branch reg_branches in
      let exn_branches_no_sleek = List.map drop_sleek_in_exn_branch exn_branches in
      Ptree.Ematch (expr_no_sleek, reg_branches_no_sleek, exn_branches_no_sleek)
  | Eabsurd -> Ptree.Eabsurd
  | Epure term -> Ptree.Epure term
  | Eidpur qid -> Ptree.Eidpur qid
  | Eraise (qid, expr_opt) ->
      let expr_opt_no_sleek = Option.map drop_sleek_in_expr expr_opt in
      Ptree.Eraise (qid, expr_opt_no_sleek)
  | Eexn (id, ty, mask, expr) ->
      let expr_no_sleek = drop_sleek_in_expr expr in
      Ptree.Eexn (id, ty, mask, expr_no_sleek)
  | Eoptexn (id, mask, expr) ->
      let expr_no_sleek = drop_sleek_in_expr expr in
      Ptree.Eoptexn (id, mask, expr_no_sleek)
  | Efor (id, expr0, dir, expr1, inv, expr2) ->
      let expr0_no_sleek = drop_sleek_in_expr expr0 in
      let expr1_no_sleek = drop_sleek_in_expr expr1 in
      let expr2_no_sleek = drop_sleek_in_expr expr2 in
      Ptree.Efor (id, expr0_no_sleek, dir, expr1_no_sleek, inv, expr2_no_sleek)
  | Eassert (kind, term) ->
      Ptree.Eassert (kind, term)
  | Escope (qid, expr) ->
      let expr_no_sleek = drop_sleek_in_expr expr in
      Ptree.Escope (qid, expr_no_sleek)
  | Elabel (id, expr) ->
      let expr_no_sleek = drop_sleek_in_expr expr in
      Ptree.Elabel (id, expr_no_sleek)
  | Ecast (expr, ty) ->
      let expr_no_sleek = drop_sleek_in_expr expr in
      Ptree.Ecast (expr_no_sleek, ty)
  | Eghost expr ->
      let expr_no_sleek = drop_sleek_in_expr expr in
      Ptree.Eghost expr_no_sleek
  | Eattr (attr, expr) ->
      let expr_no_sleek = drop_sleek_in_expr expr in
      Ptree.Eattr (attr, expr_no_sleek)
  | Esleek (_, { expr_desc; _ }) ->
      drop_sleek_in_expr_desc expr_desc

and drop_sleek_in_fundef (id, ghost, kind, binders, pty, pattern, mask, spec, expr) =
  let spec_no_sleek = drop_sleek_in_spec spec in
  let expr_no_sleek = drop_sleek_in_expr expr in
  id, ghost, kind, binders, pty, pattern, mask, spec_no_sleek, expr_no_sleek

and drop_sleek_in_field (qid, expr) =
  let expr_no_sleek = drop_sleek_in_expr expr in
  qid, expr_no_sleek

let drop_sleek_in_type_decl { td_loc; td_ident; td_params; td_vis;
                              td_mut; td_inv; td_wit; td_def } =
  let wit_no_sleek = Option.map drop_sleek_in_expr td_wit in
  Ptree.{ td_loc; td_ident; td_params;
          td_vis; td_mut; td_inv;
          td_wit = wit_no_sleek; td_def }

let rec drop_sleek_in_decl = function
  | Dtype decls -> Ptree.Dtype (List.map drop_sleek_in_type_decl decls)
  | Dlogic decls -> Ptree.Dlogic decls
  | Dind (sign, decls) -> Ptree.Dind (sign, decls)
  | Dprop (kind, id, term) -> Ptree.Dprop (kind, id, term)
  | Dlet (id, ghost, kind, expr) ->
      let expr_no_sleek = drop_sleek_in_expr expr in
      Ptree.Dlet (id, ghost, kind, expr_no_sleek)
  | Drec defs ->
      Ptree.Drec (List.map drop_sleek_in_fundef defs)
  | Dexn (id, ty, mask) -> Ptree.Dexn (id, ty, mask)
  | Dmeta (id, args) -> Ptree.Dmeta (id, args)
  | Dcloneexport (pos, qid, subst) -> Ptree.Dcloneexport (pos, qid, subst)
  | Duseexport qid -> Ptree.Duseexport qid
  | Dcloneimport (pos, bol, qid, id_opt, substs) ->
      Ptree.Dcloneimport (pos, bol, qid, id_opt, substs)
  | Duseimport (pos, bol, qids) -> Ptree.Duseimport (pos, bol, qids)
  | Dimport qid -> Ptree.Dimport qid
  | Dscope (pos, bol, id, decls) ->
      let decls_no_sleek = List.filter_map drop_sleek_in_decl_opt decls in
      Ptree.Dscope (pos, bol, id, decls_no_sleek)
  | Dsleek _ -> raise IncompatibleComponent

and drop_sleek_in_decl_opt decl =
  try Some (drop_sleek_in_decl decl) with IncompatibleComponent -> None

let drop_sleek_in_mlw_file = function
  | Decls decls ->
      let decls_no_sleek = List.filter_map drop_sleek_in_decl_opt decls in
      Ptree.Decls decls_no_sleek
  | Modules mods ->
      let drop_sleek (id, decls) =
        let decls_no_sleek = List.filter_map drop_sleek_in_decl_opt decls in
        id, decls_no_sleek
      in
      Ptree.Modules (List.map drop_sleek mods)

(* how to transform the while loop?
   - traverse throught the AST structure of the file
   - for each while loop, what do we do?
   - first, we take the condition, and the body, and create a new function to accomodate
     the logic
   - second, we replace the entire while loop with a function call
   - finally, we append the new function into the parse tree (somewhere), possibly at the
   - front so that we can use it?
 *)

let unit_pty = PTtuple []

let empty_spec = {
  sp_sleek   = [];
  sp_pre     = [];
  sp_post    = [];
  sp_xpost   = [];
  sp_reads   = [];
  sp_writes  = [];
  sp_alias   = [];
  sp_variant = [];
  sp_checkrw = false;
  sp_diverge = false;
  sp_partial = false;
}

let anchor_attr = Ident.create_attribute "sleek:spec_anchor"

let anchor_attrs = [anchor_attr]

let ptree_anchor_attrs = [ATstr anchor_attr]

let mk_ident ?attrs ?loc s = Ptree_helpers.ident ?attrs ?loc s

let mk_qualid l = Ptree_helpers.qualid l

let mk_pat ?loc desc = Ptree_helpers.pat ?loc desc

let mk_expr ?(loc=Loc.dummy_position) desc = { expr_desc = desc; expr_loc = loc }

let mk_generator (f : int -> string) : unit -> string =
  let counter = ref 0 in fun () -> let i = !counter in counter := succ i; f i

let new_proxy_fun = mk_generator (fun i -> "__proxy_fun_" ^ string_of_int i ^ "__")

let new_anchor = mk_generator (fun i -> "__anchor_" ^ string_of_int i ^ "__")

let todo () = failwith "todo!"

type preprocess_env = {
  mutable specs : sleek_spec list Mstr.t
}

let rec preprocess_expr env { expr_desc; expr_loc } : expr =
  let expr_desc = preprocess_expr_desc env expr_desc in
  { expr_desc; expr_loc }

and preprocess_expr_desc env = function
  | Ewhile (cond_e, _, _, body_e) ->
      let cond_e = preprocess_expr env cond_e in
      let body_e = preprocess_expr env body_e in
      (* TODO: add loc *)
      let id = mk_ident (new_proxy_fun ()) in
      let unit_e = mk_expr (Etuple []) in
      let call_e = mk_expr (Eidapp (Qident id, [unit_e])) in
      let body_e = mk_expr (Esequence (body_e, call_e)) in
      let ifte_e = mk_expr (Eif (cond_e, body_e, unit_e)) in
      let binders = [Loc.dummy_position, None, false, Some unit_pty] in
      let pat = mk_pat Pwild in
      let def : fundef = id, false, Expr.RKnone, binders, None, pat, Ity.MaskVisible, empty_spec, ifte_e in
      Erec ([def], call_e)
  | Esleek (specs, e) ->
      let id_name = new_anchor () in
      let id = mk_ident ~attrs:ptree_anchor_attrs id_name in
      let unit_e = mk_expr (Etuple []) in
      env.specs <- Mstr.add id_name specs env.specs;
      Elet (id, false, Expr.RKnone, unit_e, preprocess_expr env e)
  | Eidapp (qid, el) ->
      let el = List.map (preprocess_expr env) el in
      Eidapp (qid, el)
  | Eapply (e0, e1) ->
      let e0 = preprocess_expr env e0 in
      let e1 = preprocess_expr env e1 in
      Eapply (e0, e1)
  | Einfix (e0, id, e1) ->
      let e0 = preprocess_expr env e0 in
      let e1 = preprocess_expr env e1 in
      Einfix (e0, id, e1)
  | Einnfix (e0, id, e1) ->
      let e0 = preprocess_expr env e0 in
      let e1 = preprocess_expr env e1 in
      Einnfix (e0, id, e1)
  | Elet (id, ghost, kind, e0, e1) ->
      let e0 = preprocess_expr env e0 in
      let e1 = preprocess_expr env e1 in
      Elet (id, ghost, kind, e0, e1)
  | Erec (defs, e) ->
      let defs = List.map (preprocess_fundef env) defs in
      let e = preprocess_expr env e in
      Erec (defs, e)
  | Efun (binders, pty_opt, pat, mask, spec, e) ->
      Efun (binders, pty_opt, pat, mask, spec, preprocess_expr env e)
  | Etuple el ->
      Etuple (List.map (preprocess_expr env) el)
  | Erecord fields ->
      Erecord (List.map (preprocess_field env) fields)
  | Eupdate (e, fields) ->
      let e = preprocess_expr env e in
      let fields = List.map (preprocess_field env) fields in
      Eupdate (e, fields)
  | Eassign asgs ->
      let preprocess_asg (e0, qid_opt, e1) =
        let e0 = preprocess_expr env e0 in
        let e1 = preprocess_expr env e1 in
        e0, qid_opt, e1
      in
      Eassign (List.map preprocess_asg asgs)
  | Esequence (e0, e1) ->
      let e0 = preprocess_expr env e0 in
      let e1 = preprocess_expr env e1 in
      Esequence (e0, e1)
  | Eif (e0, e1, e2) ->
      let e0 = preprocess_expr env e0 in
      let e1 = preprocess_expr env e1 in
      let e2 = preprocess_expr env e2 in
      Eif (e0, e1, e2)
  | Eand (e0, e1) ->
      let e0 = preprocess_expr env e0 in
      let e1 = preprocess_expr env e1 in
      Eand (e0, e1)
  | Eor (e0, e1) ->
      let e0 = preprocess_expr env e0 in
      let e1 = preprocess_expr env e1 in
      Eor (e0, e1)
  | Enot e ->
      Enot (preprocess_expr env e)
  | Ematch (e, reg_branches, exn_branches) ->
      let preprocess_reg_branch (pat, e) = pat, preprocess_expr env e in
      let preprocess_exn_branch (qid, pat_opt, e) = qid, pat_opt, preprocess_expr env e in
      let reg_branches = List.map preprocess_reg_branch reg_branches in
      let exn_branches = List.map preprocess_exn_branch exn_branches in
      Ematch (e, reg_branches, exn_branches)
  | Eraise (qid, e_opt) ->
      Eraise (qid, Option.map (preprocess_expr env) e_opt)
  | Eexn (id, ty, mask, e) ->
      Eexn (id, ty, mask, preprocess_expr env e)
  | Eoptexn (id, mask, e) ->
      Eoptexn (id, mask, preprocess_expr env e)
  | Escope (qid, e) ->
      Escope (qid, preprocess_expr env e)
  | Elabel (id, e) ->
      Elabel (id, preprocess_expr env e)
  | Ecast (e, pty) ->
      Ecast (preprocess_expr env e, pty)
  | Eattr (attr, e) ->
      Eattr (attr, preprocess_expr env e)
  | Efor _ ->
      todo ()
  | Eref
  | Etrue
  | Efalse
  | Econst _
  | Eident _
  | Easref _
  | Eany _
  | Eabsurd
  | Epure _
  | Eassert _
  | Eghost _
  | Eidpur _ as expr_desc ->
      expr_desc

and preprocess_field env (qid, e) = qid, preprocess_expr env e

and preprocess_fundef env (id, ghost, kind, binders, pty_opt, pat, mask, spec, e) =
  id, ghost, kind, binders, pty_opt, pat, mask, spec, preprocess_expr env e

let preprocess_decl env = function
  | Dlet (id, ghost, kind, expr) ->
      Dlet (id, ghost, kind, preprocess_expr env expr)
  | Drec defs ->
      Drec (List.map (preprocess_fundef env) defs)
  | _ as decl ->
      decl

let preprocess_decl_list env (decls : decl list) : decl list =
  List.map (preprocess_decl env) decls

let preprocess_mlw_file env = function
  | Decls decls ->
      Decls (preprocess_decl_list env decls)
  | Modules mods ->
      let preprocess_mod (id, decls) = id, preprocess_decl_list env decls in
      Modules (List.map preprocess_mod mods)

let preprocess_mlw_file mlw_file =
  let env = { specs = Mstr.empty } in
  let mlw_file = preprocess_mlw_file env mlw_file in
  mlw_file, env.specs
