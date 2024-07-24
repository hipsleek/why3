(* how to mangle name then? *)
(* for symbol '+', we mangle it to? *)
(* for space, we mangle it using underscore *)
open Hipsleek_api

let mangle_char buf = function
  | '!' | '#' | '%' | '&' | '*' | '+' | '-' | '.' | '/' | ':' | '<' | '=' | '>'
  | '?' | '@' | '^' | '|' | ' ' | '$' as c ->
    let s = "_" ^ string_of_int (Char.code c) in
    Buffer.add_string buf s
  | c ->
    Buffer.add_char buf c

let mangle_string_buf s =
  let buf = Buffer.create 16 in
  String.iter (mangle_char buf) s;
  buf

let mangle_string s =
  Buffer.contents (mangle_string_buf s)

let string_of_ident Ident.{ id_string; id_attrs; id_tag } =
  let buf = mangle_string_buf id_string in
  if Ident.Sattr.mem Ident.proxy_attr id_attrs then begin
    let tag = Weakhtbl.tag_hash id_tag in
    Buffer.add_string buf "_proxy_";
    Buffer.add_string buf (string_of_int tag)
  end;
  Buffer.contents buf

let string_of_pvsymbol (pv : Ity.pvsymbol) : string =
  string_of_ident pv.Ity.pv_vs.Term.vs_name

let string_of_rsymbol (rs : Expr.rsymbol) : string =
  string_of_ident rs.Expr.rs_name

let string_of_lsymbol (ls : Term.lsymbol) : string =
  string_of_ident ls.Term.ls_name

let rec typ_of_ty Ty.{ ty_node } : Sleekapi.typ =
  typ_of_ty_node ty_node

and typ_of_ty_node = function
  | Ty.Tyapp ({ ts_name = Ident.{ id_string; _ }; _ }, []) ->
    begin match id_string with
      | "int" -> Sleekapi.Int
      | "bool" -> Sleekapi.Bool
      (* | "float" -> Sleekapi.Float *)
      | _ -> Sleekapi.Named id_string
    end
  | _ ->
    raise (Invalid_argument "no support for this type!")

let typ_of_vsymbol Term.{ vs_ty } : Sleekapi.typ =
  typ_of_ty vs_ty

let typ_of_pvsymbol Ity.{ pv_vs; _ } : Sleekapi.typ =
  typ_of_vsymbol pv_vs

let new_proxy_var =
  let counter = ref 0 in
  fun () ->
    let count = !counter in
    counter := count + 1;
    "o_sleek_proxy_" ^ (string_of_int count)
