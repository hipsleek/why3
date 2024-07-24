open Wstdlib
open Ptree_sleek
open Hipsleek_api
open Sleek_name_mangling

let todo () = failwith "not implemented!"

let union_all (ms : 'a Mstr.t list) : 'a Mstr.t =
  List.fold_left Mstr.set_union Mstr.empty ms

let rec gather_spec_in_mlw_file : mlw_file -> string list Mstr.t = function
  | Decls decls ->
      union_all (List.map gather_spec_in_decl decls)
  | Modules [_, decls] ->
      union_all (List.map gather_spec_in_decl decls)
  | _ ->
      (* for now, we only support a single modules *)
      raise (Invalid_argument "only support a single module at the moment!")

and gather_spec_in_decl = function
  | Dlet ({ id_str; _ }, false, _, { expr_desc = Efun (_, _, _, _, spec, _); _ }) ->
      (* question: does rk_kind matter? *)
      (* currently we just gather the spec without doing any extra transformation *)
      (* problem: binders may have no type information. We may need to do type inference on this binder? *)
      (* can we instead just collect the string into a map, put it somewhere else, then type check the file, and then come back and complete
         the declaration? *)
      Mstr.singleton id_str spec.sp_sleek
  | Drec defs ->
      union_all (List.map gather_spec_in_fundef defs)
  | _ -> Mstr.empty

and gather_spec_in_fundef = function
  | ({ id_str; _ }, false, _, _, _, _, _, spec, _) ->
      Mstr.singleton id_str spec.sp_sleek
  | _ -> Mstr.empty


type spec_map = (Sleekapi.sf * Sleekapi.param list) Mstr.t


let rec compile_spec_in_pmodule specs Pmodule.{ mod_units; _ } : spec_map =
  union_all (List.map (compile_spec_in_mod_unit specs) mod_units)

and compile_spec_in_mod_unit specs = function
  | Pmodule.Udecl pdecl -> compile_spec_in_pdecl specs pdecl
  | _ -> Mstr.empty

and compile_spec_in_pdecl specs = function
  | Pdecl.{ pd_node = PDlet Expr.LDsym (rs, { c_node = Cfun _; _ }); _ } ->
      let name = rs.Expr.rs_name.Ident.id_string in
      let spec = Mstr.find name specs in
      let spec = String.concat "\n" spec in
      let params = gather_params rs.Expr.rs_cty in
      let spec = Sleekapi.spec_decl name spec params in
      Mstr.singleton name (spec, params)
  | _ -> Mstr.empty

and gather_params Ity.{ cty_args } =
  List.map gather_param cty_args

and gather_param pv =
  let name = string_of_pvsymbol pv in
  (* TODO: detect the actual type of the argument *)
  let ty = Sleekapi.Int in
  Sleekapi.{ param_type = ty; param_name = name; param_mod = RefMod }


let rec gather_data_decl_in_mlw_file = function
  | Decls decls
  | Modules [_, decls] ->
      List.iter gather_data_decl_in_decl decls
  | _ ->
      raise (Invalid_argument "only support a single module at the moment!")

and gather_data_decl_in_decl = function
  | Dtype ty_decls ->
      List.iter gather_data_decl_in_type_decl ty_decls
  | _ -> ()

and gather_data_decl_in_type_decl = function
  | { td_ident = { id_str; _ }; td_def = TDrecord fields; _ } ->
      let fields = List.map gather_field fields in
      Sleekapi.data_decl_cons id_str fields
  | _ -> ()

and gather_field { f_ident = { id_str; _ }; f_pty; _ } =
  (* TODO: change this later *)
  let ty = Sleekapi.Int in
  ty, id_str

let approximate_option_with_null _ = todo ()

let rec gather_logic_decl_in_mlw_file = function
  | Decls decls
  | Modules [_, decls] ->
    List.iter gather_logic_decl_in_decl decls
  | _ ->
    raise (Invalid_argument "only support a single module at the moment!")

and gather_logic_decl_in_decl = function
  | Dsleek decl -> Sleekapi.top_level_decl decl
  | _ -> ()


let rec forward_on_expr spec_map ctx Expr.{ e_node } =
  Format.eprintf "ctx=%s@." (Sleekapi.Printer.string_of_lfe ctx);
  forward_on_expr_node spec_map ctx e_node

and forward_on_expr_node spec_map ctx = function
  | Expr.Evar pv ->
      let name = string_of_pvsymbol pv in
      (* TODO: get the correct type of this pvsymbol *)
      (* TODO: mangle the name of this symbol, as normalization introduces
         lots of symbols with the same string name but with different ids *)
      let ty = Sleekapi.Int in
      Format.eprintf "Evar_pv=%s@." name;
      Sleekapi.upd_result_with_var ctx ty name
  | Expr.Econst c ->
      begin match c with
        | Constant.ConstInt int_const ->
            (* TODO: what if this is a very big integer? *)
            let i = Number.to_small_integer int_const in
            Format.eprintf "Evar_i=%d@." i;
            Sleekapi.upd_result_with_int ctx i
        | _ -> todo ()
      end
  | Expr.Eexec ({ c_node; _ }, cty) ->
      let apply_pre_post name args =
        let args_name = List.map string_of_pvsymbol args in
        Format.eprintf "Exec_name=%s@." name;
        List.iter (Format.eprintf "Exec_arg=%s@.") args_name;
        let spec, params = Mstr.find name spec_map in
        Format.eprintf "Exec_spec=%s@." (Sleekapi.Printer.string_of_sf spec);
        let new_ctx = Sleekapi.check_pre_post ctx spec false params args_name in
        Option.get new_ctx
      in
      let get_field rs args =
        if List.length args <> 1 then
          raise (Invalid_argument "field access should have a single argument");
        let field_name = string_of_rsymbol rs in
        let target = List.hd args in
        let target_name = string_of_pvsymbol target in
        let typ = typ_of_pvsymbol target in
        Sleekapi.data_field_read ctx typ target_name field_name
      in
      begin match c_node with
        | Expr.Capp (Expr.{ rs_field = Some _; _ } as rs, args) ->
            get_field rs args
        | Expr.Capp (rs, args) ->
            apply_pre_post (string_of_rsymbol rs) args
        | Expr.Cpur (ls, args) ->
            apply_pre_post (string_of_lsymbol ls) args
        | _ -> todo ()
      end
  | Expr.Elet (LDvar (pv, epv), e) ->
      let name = string_of_pvsymbol pv in
      let ty = Sleekapi.Int in
      let ctx = forward_on_expr spec_map ctx epv in
      Format.eprintf "Elet_pv=%s@." name;
      let ctx = Sleekapi.add_assign_to_ctx ctx ty name in
      forward_on_expr spec_map ctx e
  | Expr.Elet (defn, e) -> todo ()
  | Expr.Eif (if_e, then_e, else_e) ->
      let ctx = forward_on_expr spec_map ctx if_e in
      let proxy_var : string = new_proxy_var () in
      let ctx = Sleekapi.add_assign_to_ctx ctx Sleekapi.Bool proxy_var in
      let then_ctx = Sleekapi.add_cond_to_ctx ctx proxy_var true in
      let else_ctx = Sleekapi.add_cond_to_ctx ctx proxy_var false in
      let then_ctx = forward_on_expr spec_map then_ctx then_e in
      let else_ctx = forward_on_expr spec_map else_ctx else_e in
      Sleekapi.disj_of_ctx then_ctx else_ctx
  | Expr.Eassign asgl ->
      let set_field ctx (target, field, var) =
        let target_name = string_of_pvsymbol target in
        let field_name = string_of_rsymbol field in
        let var_name = string_of_pvsymbol var in
        let typ = typ_of_pvsymbol target in
        Sleekapi.data_field_update ctx typ target_name field_name var_name
      in
      List.fold_left set_field ctx asgl
  | Expr.Efor _ -> todo ()
  | Expr.Ewhile _ -> todo ()
  | _ -> todo ()


(* We need to walk forward *)
(* Now, how to we walk forward? What do we accepts as our argument? *)
let verify_function (specs : spec_map) = function
  | Pmodule.Udecl Pdecl.{ pd_node = PDlet Expr.LDsym (rs, { c_node = Cfun expr; _ }); _ } ->
    (* rs is the function name *)
    (* ce is the function body *)
    (* the type of the result and of function arguments us stored in rs.cty *)
    (* not sure whether there is any difference between the ctype of the rssymbol and the cnode? *)
    let name = string_of_rsymbol rs in
    let spec, params = Mstr.find name specs in
    (* initialize the context, somehow *)
    let ctx = Sleekapi.init_ctx spec params in
    let ctx = forward_on_expr specs ctx expr in
    (* this is the final context. Then check *)
    Sleekapi.check_entail_post ctx spec params
  | _ -> true


let verify_module (specs : spec_map) Pmodule.{ mod_units } =
  List.for_all (verify_function specs) mod_units
