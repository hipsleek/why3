open Apron

module type ABSTRACT_DOMAIN = sig
  type man
  type t
  type env
  val create_manager: unit -> man
  val bottom: man -> env -> t
  val top: man -> env -> t
  val canonicalize: man -> t -> unit
  val is_bottom: man -> t -> bool
  val is_leq: man -> t -> t -> bool
  val join: man -> t -> t -> t
  val join_list: man -> t list -> t
  val widening: man -> t -> t -> t
  val print: Format.formatter -> t -> unit
  val push_label: man -> env -> int -> t -> t
end

module type DOMAIN = sig
  include ABSTRACT_DOMAIN with type env = Environment.t
  val meet_lincons_array: man -> t -> Lincons1.earray -> t
  val forget_array: man -> t -> Var.t array -> bool -> t
  val assign_linexpr: man -> t -> Var.t -> Linexpr1.t -> t option -> t
  val to_lincons_array: man -> t -> Lincons1.earray
  val to_term: Env.env -> Pmodule.pmodule -> man -> t -> (Var.t -> Term.term) -> Term.term
  val get_linexpr: man -> t -> Var.t -> ((Coeff.t * Var.t) list * Coeff.t) option
  val hash: man -> t -> int
  val is_eq: man -> t -> t -> bool
end

module type TERM_DOMAIN = sig
  include ABSTRACT_DOMAIN with type env = unit
  val forget_var: man -> Term.vsymbol -> t -> t
  val forget_term: man -> Term.term -> t -> t
  val forget_region: man -> Ity.region -> unit Ity.Mpv.t -> t -> t
  val meet_term: man -> Term.term -> t -> t
  val add_variable_to_env: man -> Ity.pvsymbol -> unit
  val add_lvariable_to_env: man -> Term.vsymbol -> unit
  val to_term: man -> t -> Term.term
  val update_possible_substitutions: man -> unit
end

module Make_from_apron(M:sig
    type man
    type env
    type t
    val create_manager: unit -> man
  end) = struct
  type man = M.man
  type env = M.env
  type t = M.t
  let create_manager = M.create_manager


  module A = Abstract1

  let bottom = A.bottom
  let top = A.top
  let canonicalize _ _ = ()
  let is_bottom = A.is_bottom
  let is_leq = A.is_leq
  let join = A.join
  let join_list m t = A.join_array m (Array.of_list t)
  let widening = A.widening
  let print = A.print
  let meet_lincons_array = A.meet_lincons_array
  let forget_array = A.forget_array
  let assign_linexpr = A.assign_linexpr
  let hash = A.hash
  let is_eq = A.is_eq
                         
  exception Cannot_be_expressed

  type coeff =
    | CNone
    | CPos of Term.term
    | CMinus of Term.term
    | CMinusOne
    | COne

  let to_term env pmod = 
    let open Term in
    let th_int = Env.read_theory env ["int"] "Int" in
    let int_tys = Theory.(ns_find_ts th_int.th_export ["int"]) in
    let ty_int = (Ty.ty_app int_tys []) in
    let int_zero = t_const Number.(ConstInt (int_const_dec "0")) in
    let int_one = t_const Number.(ConstInt (int_const_dec "1")) in
    let int_le = Theory.(ns_find_ls th_int.th_export ["infix <="]) in
    let int_lt = Theory.(ns_find_ls th_int.th_export ["infix <"]) in
    let int_add =  begin fun a ->
      match a with
      | [a; b] ->
        if t_equal a int_zero then b
        else if t_equal b int_zero then a
        else fs_app Theory.(ns_find_ls th_int.th_export ["infix +"]) [a; b] ty_int
      | _ -> assert false
    end in
    let int_minus_u = fun a -> fs_app Theory.(ns_find_ls th_int.th_export ["prefix -"]) a ty_int in
    let int_minus = begin fun a ->
      match a with
      | [a; b] ->
        if t_equal a int_zero then int_minus_u [b]
        else if t_equal b int_zero then a
        else fs_app Theory.(ns_find_ls th_int.th_export ["infix -"]) [a; b] ty_int
      | _ -> assert false
    end
    in
    let int_mult = fun a -> fs_app Theory.(ns_find_ls th_int.th_export ["infix *"]) a ty_int in

    let rec int_of_s s =
      let open Scalar in
      match s with
      | Float f -> 
        let i = int_of_float f in
        assert (float_of_int i = f);
        i
      | Mpqf t ->
        int_of_s (Float (Mpqf.to_float t))
      | Mpfrf t ->
        int_of_s (Float (Mpfr.to_float t))
    in

    let coeff_to_term = function
      | Coeff.Scalar(s) ->
        let i = int_of_s s in
        let s = string_of_int (abs i) in
        let n = Number.int_const_dec s in
        let n = Number.ConstInt n in

        if i = 1 then
          COne
        else if i = -1 then
          CMinusOne
        else if i > 0 then
          CPos (t_const n)
        else if i < 0 then
          CMinus (t_const n)
        else
          CNone
      | Coeff.Interval(_) -> raise Cannot_be_expressed
    in

    let lincons_to_term l variable_mapping =
      let open Ty in
      let term = ref int_zero in
      Lincons1.iter (fun c v ->
          match coeff_to_term c with
          | CPos c ->
            let v = variable_mapping v in
            term := int_add [!term; int_mult [c; v]];
          | COne ->
            let v = variable_mapping v in
            term := int_add [!term; v];
          | CMinusOne ->
            let v = variable_mapping v in
            term := int_minus [!term; v];
          | CMinus c ->
            let v = variable_mapping v in
            term := int_minus [!term; int_mult [c; v]];
          | CNone -> ()
        ) l;
      let c = coeff_to_term (Lincons1.get_cst l) in
      let term = match c with
        | CNone -> !term
        | CPos c ->
          int_add [c; !term]
        | CMinus c ->
          int_minus [!term;c]
        | COne ->
          int_add [int_one; !term]
        | CMinusOne ->
          int_minus [!term; int_one]
      in
      match Lincons1.get_typ l with
      | Lincons1.EQ -> ps_app ps_equ [term; int_zero]
      | Lincons1.SUP -> ps_app int_lt [int_zero; term]
      | Lincons1.SUPEQ -> ps_app int_le [int_zero; term;]
      | Lincons1.DISEQ ->  t_not (ps_app ps_equ [term; int_zero])
    in

    let lincons_array_to_term l variable_mapping =
      let n = Lincons1.array_length l in
      let t = ref (Term.t_true) in
      for i = 0 to n - 1 do
        t := t_and_simp !t (lincons_to_term (Lincons1.array_get l i) variable_mapping);
      done;
      !t
    in

    let domain_to_term man d variable_mapping =
      let a = Abstract1.to_lincons_array man d in
      lincons_array_to_term a variable_mapping
    in domain_to_term

  let push_label _ _ _ t = t

  let to_lincons_array = Abstract1.to_lincons_array


  let get_linexpr man d v =
    let cons = to_lincons_array man d in
    let n = Lincons1.array_length cons in
    let vars = ref None in
    for i = 0 to n - 1 do
      let l = Lincons1.array_get cons i in
      if Lincons1.get_typ l = Lincons1.EQ then
        begin
          let found = ref false in
          let var_list = ref [] in
          let neg = ref false in
          Lincons1.iter (fun c v' ->
              if v <> v' then
                var_list := (c, v') :: !var_list
              else match c with
                | Coeff.Scalar(s) ->
                  neg := Scalar.equal_int s 1;
                  if Scalar.equal_int s 1 || Scalar.equal_int s (-1) then
                    found := true;
                | _ -> ()) l;
          if !found then
            begin
              if !neg then
                var_list := List.map (fun (c, v) ->
                    Coeff.neg c, v) !var_list;
              vars := Some (!var_list, if !neg then
                              Coeff.neg @@ Lincons1.get_cst l
                            else
                              Lincons1.get_cst l)
            end
        end
    done;
    !vars

end

module Polyhedra = Make_from_apron(struct
  type man = Polka.strict Polka.t Manager.t
  type env = Environment.t
  type t = Polka.strict Polka.t Abstract1.t
  let create_manager = Polka.manager_alloc_strict
  end)

module Box = Make_from_apron(struct
  type man = Box.t Manager.t
  type env = Environment.t
  type t = Box.t Abstract1.t
  let create_manager = Box.manager_alloc
  end)

module Oct = Make_from_apron(struct
  type man = Oct.t Manager.t
  type env = Environment.t
  type t = Oct.t Abstract1.t
  let create_manager = Oct.manager_alloc
  end)
