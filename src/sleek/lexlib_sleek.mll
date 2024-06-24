{
  exception UnterminatedSleekSpec

  let () = Exn_printer.register (fun fmt e -> match e with
    | UnterminatedSleekSpec -> Format.pp_print_string fmt "unterminated sleek spec"
    | _ -> raise e)

  let loc lb = Loc.extract (lb.lex_start_p, lb.lex_curr_p)

  (* TODO: implement proper support for utf8
  let utf8_extra_bytes = function
    | '\000'..'\127' -> 0
    | '\192'..'\223' -> 1
    | '\224'..'\239' -> 2
    | '\240'..'\247' -> 3
    | _ -> -1

  let adjust_pos lexbuf n =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with pos_cnum = pos.pos_cnum - n };
    lexbuf.lex_abs_pos <- lexbuf.lex_abs_pos - n
  *)
}

(* this should be quite similar to string *)
rule sleek_spec buf = parse
  | "*)"
      { Buffer.contents buf }
  | eof
      { raise (Loc.Located (loc lexbuf, UnterminatedSleekSpec)) }
  | _ as c
      { (* let n = utf8_extra_bytes c in
           if n > 0 then adjust_pos lexbuf n; *)
        Buffer.add_char buf c;
        sleek_spec buf lexbuf }

{
  let sleek_spec lexbuf =
    sleek_spec (Buffer.create 128) lexbuf
}
