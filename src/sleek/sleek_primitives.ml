open Wstdlib
open Hipsleek_api
open Sleek_name_mangling

let register_constant_primitives () : Forward_verification.spec_map =
  let constants = [
    "True", "requires true ensures res;";
    "False", "requires true ensures !res;";
  ] in
  let make_spec (name, form) = name, (Sleekapi.spec_decl name form [], []) in
  let constants = List.map make_spec constants in
  Mstr.of_list constants

let register_unary_int_primitives () : Forward_verification.spec_map =
  let param_x = Sleekapi.{ param_type = Int; param_mod = RefMod; param_name = "x" } in
  let params = [param_x] in
  let add_spec spec_map op =
    let name = mangle_string ("prefix " ^ op) in
    let form = "requires true ensures res = " ^ op ^ "x;" in
    let spec = Sleekapi.spec_decl name form params in
    Mstr.add name (spec, params) spec_map
  in
  let un_ops = ["-"] in
  List.fold_left add_spec Mstr.empty un_ops

let register_binary_int_int_primitives () : Forward_verification.spec_map =
  let param_x = Sleekapi.{ param_type = Int; param_mod = RefMod; param_name = "x" } in
  let param_y = Sleekapi.{ param_x with param_name = "y" } in
  let params = [param_x; param_y] in
  let add_spec spec_map op =
    let name = mangle_string ("infix " ^ op) in
    let form = "requires true ensures res = x " ^ op ^ " y;" in
    let spec = Sleekapi.spec_decl name form params in
    Mstr.add name (spec, params) spec_map
  in
  let bin_ops = ["+"; "-"; "*"; "<"; ">"; "="] in
  List.fold_left add_spec Mstr.empty bin_ops

let register_primitives () =
  (* arithmetic core *)
  let primitives = [
    register_constant_primitives ();
    register_unary_int_primitives ();
    register_binary_int_int_primitives ()
  ] in
  List.fold_left Mstr.set_union Mstr.empty primitives
