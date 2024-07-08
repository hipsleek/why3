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
