open Hipsleek_api

val mangle_string : string -> string

val string_of_ident : Ident.ident -> string

val string_of_pvsymbol : Ity.pvsymbol -> string

val string_of_rsymbol : Expr.rsymbol -> string

val string_of_lsymbol : Term.lsymbol -> string

val typ_of_vsymbol : Term.vsymbol -> Sleekapi.typ

val typ_of_pvsymbol : Ity.pvsymbol -> Sleekapi.typ

val new_proxy_var : unit -> string
