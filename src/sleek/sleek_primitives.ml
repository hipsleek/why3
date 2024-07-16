open Wstdlib
open Hipsleek_api

let todo () = failwith "todo"

let register_binary_int_int_primitives () : Forward_verification.spec_map =
  let param_x = Sleekapi.{ param_type = Int; param_mod = RefMod; param_name = "x" } in
  let param_y = Sleekapi.{ param_x with param_name = "y" } in
  let params = [param_x; param_y] in
  let add_spec spec_map op =
    let name = "infix " ^ op in
    let form = "requires true ensures res = x " ^ op ^ " y;" in
    let spec = Sleekapi.spec_decl name form params in
    Mstr.add name (spec, params) spec_map
  in
  let bin_ops = ["+"; "-"; "*"; "<"; ">"; "="] in
  List.fold_left add_spec Mstr.empty bin_ops

let register_primitives () =
  (* arithmetic core *)
  register_binary_int_int_primitives ()
