open Wstdlib
open Ptree_sleek
open Hipsleek_api

let todo () = failwith "not implemented!"

(*
let add_spec file = assert false

let add_data _ = assert false
let add_lemma _ = assert false
let add_predicate _ = assert false
let to_sleek_context _ = assert false
*)


let union_all (ms : 'a Mstr.t list) : 'a Mstr.t =
  List.fold_left Mstr.set_union Mstr.empty ms

let string_of_ident (id : Ident.ident) : string =
  id.Ident.id_string

let string_of_pvsymbol (pv : Ity.pvsymbol) : string =
  string_of_ident (pv.Ity.pv_vs.Term.vs_name)

let string_of_rsymbol (rs : Expr.rsymbol) : string =
  string_of_ident (rs.Expr.rs_name)


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
  Sleekapi.{ param_type = ty; param_name = name; param_mod = CopyMod }


let rec gather_data_decl_in_mlw_file = function
  | Decls decls ->
      List.iter gather_data_decl_in_decl decls
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
      (* convert the fields into string *)
      (* and then push that into the sleek api *)
      (* TODO: handle error *)
      Sleekapi.data_decl id_str fields
  | _ -> ()

and gather_field { f_ident = { id_str; _ }; f_pty; _ } =
  (* TODO: change this later *)
  let ty = Sleekapi.Int in
  ty, id_str


let rec gather_logic_decl_in_mlw_file = function
  | Decls decls ->
    List.iter gather_data_decl_in_decl decls
  | Modules [_, decls] ->
    List.iter gather_data_decl_in_decl decls
  | _ ->
    raise (Invalid_argument "only support a single module at the moment!")

and gather_logic_decl_in_decl = function
  | Dsleek decl -> Sleekapi.top_level_decl decl
  | _ -> ()


let rec forward_on_expr ctx Expr.{ e_node } =
  forward_on_expr_node ctx e_node

and forward_on_expr_node ctx = function
  | Expr.Evar pv ->
      let name = string_of_pvsymbol pv in
      (* TODO: get the correct type of this pvsymbol *)
      let ty = Sleekapi.Int in
      Sleekapi.upd_result_with_var ctx ty name
  | Expr.Econst c ->
      begin match c with
        | Constant.ConstInt int_const ->
            (* TODO: what if this is a very big integer? *)
            let i = Number.to_small_integer int_const in
            Sleekapi.upd_result_with_int ctx i
        | _ -> todo ()
      end
  | Expr.Eexec ({ c_node; _ }, cty) ->
      let _ = match c_node with
        | Expr.Capp (rs, args) ->
            (* we need to check pre-post with this rs-symbol *)
            (* TODO: what for the arith core from the API *)
            todo ()
        | Expr.Cpur (ls, args) -> todo ()
        | _ -> todo ()
      in todo ()
      (* function call, pre-post, then then continue *)
  | Expr.Elet (LDvar (pv, epv), e) ->
      let name = string_of_pvsymbol pv in
      let ty = Sleekapi.Int in
      let ctx = forward_on_expr ctx epv in
      let ctx = Sleekapi.add_assign_to_ctx ctx ty name in
      forward_on_expr ctx e
  | Expr.Elet (defn, e) -> todo ()
  | Expr.Eif (cond, if_e, else_e) -> todo ()
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
    let ctx = forward_on_expr ctx expr in
    (* this is the final context. Then check *)
    Sleekapi.check_entail_post ctx spec params
  | _ -> true


let verify_module (specs : spec_map) Pmodule.{ mod_units } =
  List.for_all (verify_function specs) mod_units
