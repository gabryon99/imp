{
    open Parser

    let create_hashtable size init =
        let tbl = Hashtbl.create size in
        List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
        tbl

    let keywords_table = create_hashtable 9 [
        ("true",        BOOL(true));
        ("false",       BOOL(false));
        ("if",          K_IF);
        ("print",       K_PRINT);
        ("read",        K_READ);
        ("else",        K_ELSE);
        ("while",       K_WHILE);
        ("do",          K_DO);
        ("end",         K_END);
        ("skip",         K_SKIP);
    ]
}

let digit = ['0' - '9']
let dec_number = digit+

let identifier = ['a' - 'z' 'A' - 'Z']['0' - '9' 'a' - 'z' 'A' - 'Z' '_']*

rule line = parse
    | ([^'\n']* '\n') as line
        (* Normal case: one line, no eof. *)
        { Some line, true }
    | eof
        (* Normal case: no data, eof. *)
        { None, false }
    | ([^'\n']+ as line) eof
        (* Special case: some data but missing '\n', then eof.
        Consider this as the last line, and add the missing '\n'. *)
        { Some (line ^ "\n"), false }

and next_token = parse

    | eof       { 
        EOF 
    }

    | '\n'
    | [' ']     { next_token lexbuf }

    | dec_number as num { 
        let n = int_of_string num in INT(n)
     }

    | identifier as id {
                try
                    let token = Hashtbl.find keywords_table id in
                    token
                with Not_found ->
                    LOC(id)
    }

    | '%'                 { comment lexbuf }

    | ":="                { ASSIGN }
    | '('                 { L_PAREN }
    | ')'                 { R_PAREN }

    (* Logical operators *)
    | ['!']               { NOT }
    | "&&"                { AND_AND }
    | "||"                { OR_OR }

    (* Comparsion operators *)
    | '>'                 { GT }
    | '<'                 { LT }
    | "="                 { EQ }
    | ">="                { GTE }
    | "<="                { LTE }

    (* Math operators *)
    | '+'                 { PLUS }
    | '-'                 { MINUS }
    | '*'                 { TIMES }

    | ';'                 { SEMICOLON }
and comment = parse
    | '\n'                { next_token lexbuf }
    | _                   { comment lexbuf}