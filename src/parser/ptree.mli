type attr = ATstr of Ident.attribute | ATpos of Loc.position
type ident = { id_str : string; id_ats : attr list; id_loc : Loc.position; }
type qualid = Qident of ident | Qdot of qualid * ident
type pty =
    PTtyvar of ident
  | PTtyapp of qualid * pty list
  | PTtuple of pty list
  | PTref of pty list
  | PTarrow of pty * pty
  | PTscope of qualid * pty
  | PTparen of pty
  | PTpure of pty
type ghost = bool
type pattern = { pat_desc : pat_desc; pat_loc : Loc.position; }
and pat_desc =
    Pwild
  | Pvar of ident
  | Papp of qualid * pattern list
  | Prec of (qualid * pattern) list
  | Ptuple of pattern list
  | Pas of pattern * ident * ghost
  | Por of pattern * pattern
  | Pcast of pattern * pty
  | Pscope of qualid * pattern
  | Pparen of pattern
  | Pghost of pattern
type binder = Loc.position * ident option * ghost * pty option
type param = Loc.position * ident option * ghost * pty
type term = { term_desc : term_desc; term_loc : Loc.position; }
and term_desc =
    Ttrue
  | Tfalse
  | Tconst of Constant.constant
  | Tident of qualid
  | Tasref of qualid
  | Tidapp of qualid * term list
  | Tapply of term * term
  | Tinfix of term * ident * term
  | Tinnfix of term * ident * term
  | Tbinop of term * Dterm.dbinop * term
  | Tbinnop of term * Dterm.dbinop * term
  | Tnot of term
  | Tif of term * term * term
  | Tquant of Dterm.dquant * binder list * term list list * term
  | Teps of ident * pty * term
  | Tattr of attr * term
  | Tlet of ident * term * term
  | Tcase of term * (pattern * term) list
  | Tcast of term * pty
  | Ttuple of term list
  | Trecord of (qualid * term) list
  | Tupdate of term * (qualid * term) list
  | Tscope of qualid * term
  | Tat of term * ident
type invariant = term list
type variant = (term * qualid option) list
type pre = term
type post = Loc.position * (pattern * term) list
type xpost = Loc.position * (qualid * (pattern * term) option) list
type spec = {
  sp_pre : pre list;
  sp_post : post list;
  sp_xpost : xpost list;
  sp_reads : qualid list;
  sp_writes : term list;
  sp_alias : (term * term) list;
  sp_variant : variant;
  sp_checkrw : bool;
  sp_diverge : bool;
  sp_partial : bool;
}
type expr = { expr_desc : expr_desc; expr_loc : Loc.position; }
and expr_desc =
    Eref
  | Etrue
  | Efalse
  | Econst of Constant.constant
  | Eident of qualid
  | Easref of qualid
  | Eidapp of qualid * expr list
  | Eapply of expr * expr
  | Einfix of expr * ident * expr
  | Einnfix of expr * ident * expr
  | Elet of ident * ghost * Expr.rs_kind * expr * expr
  | Erec of fundef list * expr
  | Efun of binder list * pty option * pattern * Ity.mask * spec * expr
  | Eany of param list * Expr.rs_kind * pty option * pattern * Ity.mask *
      spec
  | Etuple of expr list
  | Erecord of (qualid * expr) list
  | Eupdate of expr * (qualid * expr) list
  | Eassign of (expr * qualid option * expr) list
  | Esequence of expr * expr
  | Eif of expr * expr * expr
  | Ewhile of expr * invariant * variant * expr
  | Eand of expr * expr
  | Eor of expr * expr
  | Enot of expr
  | Ematch of expr * reg_branch list * exn_branch list
  | Eabsurd
  | Epure of term
  | Eidpur of qualid
  | Eraise of qualid * expr option
  | Eexn of ident * pty * Ity.mask * expr
  | Eoptexn of ident * Ity.mask * expr
  | Efor of ident * expr * Expr.for_direction * expr * invariant * expr
  | Eassert of Expr.assertion_kind * term
  | Escope of qualid * expr
  | Elabel of ident * expr
  | Ecast of expr * pty
  | Eghost of expr
  | Eattr of attr * expr
and reg_branch = pattern * expr
and exn_branch = qualid * pattern option * expr
and fundef =
    ident * ghost * Expr.rs_kind * binder list * pty option * pattern *
    Ity.mask * spec * expr
type field = {
  f_loc : Loc.position;
  f_ident : ident;
  f_pty : pty;
  f_mutable : bool;
  f_ghost : bool;
}
type type_def =
    TDalias of pty
  | TDalgebraic of (Loc.position * ident * param list) list
  | TDrecord of field list
  | TDrange of BigInt.t * BigInt.t
  | TDfloat of int * int
type visibility = Public | Private | Abstract
type type_decl = {
  td_loc : Loc.position;
  td_ident : ident;
  td_params : ident list;
  td_vis : visibility;
  td_mut : bool;
  td_inv : invariant;
  td_wit : expr option;
  td_def : type_def;
}
type logic_decl = {
  ld_loc : Loc.position;
  ld_ident : ident;
  ld_params : param list;
  ld_type : pty option;
  ld_def : term option;
}
type ind_decl = {
  in_loc : Loc.position;
  in_ident : ident;
  in_params : param list;
  in_def : (Loc.position * ident * term) list;
}
type metarg =
    Mty of pty
  | Mfs of qualid
  | Mps of qualid
  | Max of qualid
  | Mlm of qualid
  | Mgl of qualid
  | Mval of qualid
  | Mstr of string
  | Mint of int
type clone_subst =
    CStsym of qualid * ident list * pty
  | CSfsym of qualid * qualid
  | CSpsym of qualid * qualid
  | CSvsym of qualid * qualid
  | CSxsym of qualid * qualid
  | CSprop of Decl.prop_kind
  | CSaxiom of qualid
  | CSlemma of qualid
  | CSgoal of qualid
type decl =
    Dtype of type_decl list
  | Dlogic of logic_decl list
  | Dind of Decl.ind_sign * ind_decl list
  | Dprop of Decl.prop_kind * ident * term
  | Dlet of ident * ghost * Expr.rs_kind * expr
  | Drec of fundef list
  | Dexn of ident * pty * Ity.mask
  | Dmeta of ident * metarg list
  | Dcloneexport of Loc.position * qualid * clone_subst list
  | Duseexport of qualid
  | Dcloneimport of Loc.position * bool * qualid * ident option *
      clone_subst list
  | Duseimport of Loc.position * bool * (qualid * ident option) list
  | Dimport of qualid
  | Dscope of Loc.position * bool * ident * decl list
val sexp_of_mlw_file : 'a -> 'b
val mlw_file_of_sexp : 'a -> 'b
type mlw_file = Modules of (ident * decl list) list | Decls of decl list
