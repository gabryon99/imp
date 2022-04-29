(* Semantics rule from "The Formal Semantics of a Programming Language - Glynn Winskell" *)

let rec eval_aexp sigma = function
  | Ast.Read ->
    let line = input_line stdin in
    int_of_string line
  | Ast.ILiteral(n) -> (n)
  | Ast.Variable(name) -> (sigma name)
  | Ast.Sum(e0, e1) -> let n0 = eval_aexp sigma e0 in let n1 = eval_aexp sigma e1 in (n0 + n1)
  | Ast.Sub(e0, e1) -> let n0 = eval_aexp sigma e0 in let n1 = eval_aexp sigma e1 in (n0 - n1)
  | Ast.Times(e0, e1) -> let n0 = eval_aexp sigma e0 in let n1 = eval_aexp sigma e1 in (n0 * n1)

let rec eval_bexp sigma = function
  | Ast.BLiteral(true) -> true
  | Ast.BLiteral(false) -> false
  | Ast.Eq(a0, a1) -> let n = eval_aexp sigma a0 in let m = eval_aexp sigma a1 in (n = m)
  | Ast.Lte(a0, a1) -> let n = eval_aexp sigma a0 in let m = eval_aexp sigma a1 in (n <= m)
  | Ast.Lt(a0, a1) -> let n = eval_aexp sigma a0 in let m = eval_aexp sigma a1 in (n < m)
  | Ast.Gte(a0, a1) -> let n = eval_aexp sigma a0 in let m = eval_aexp sigma a1 in (n >= m)
  | Ast.Gt(a0, a1) -> let n = eval_aexp sigma a0 in let m = eval_aexp sigma a1 in (n > m)
  | Ast.Not(b) -> not (eval_bexp sigma b)
  | Ast.And(e0, e1) -> let b0 = eval_bexp sigma e0 in let b1 = eval_bexp sigma e1 in (b0 && b1)
  | Ast.Or(e0, e1) -> let b0 = eval_bexp sigma e0 in let b1 = eval_bexp sigma e1 in (b0 || b1)

let rec eval_com sigma = function
  | Ast.Skip -> sigma
  | Ast.Print(a0) ->
      let n = eval_aexp sigma a0 in
      let _ = Printf.printf "%d\n" n in 
      sigma
  | Ast.Assign(loc, a0) ->
    let m = eval_aexp sigma a0 in
    Ast.update_state loc m sigma
  | Ast.Seq(c0, c1) -> 
    let sigma'' = eval_com sigma c0 in
    let sigma' = eval_com sigma'' c1 in sigma'
  | Ast.IfThen(guard, bthen, belse) ->
      let bguard = eval_bexp sigma guard in
      begin
        match bguard with
        | true -> eval_com sigma bthen
        | false -> eval_com sigma belse
      end
  | Ast.While(guard, bwhile) as w ->
      let bguard = eval_bexp sigma guard in
      begin
        match bguard with
        | true ->
          let sigma'' = eval_com sigma bwhile in
          eval_com sigma'' w
        | false -> sigma
      end

let exec_prog (line : string) (sigma: Ast.state) : Ast.state =
  try
    let linebuf = Lexing.from_string line in
    let prog = Parser.prog Lexer.next_token linebuf in
    eval_com sigma prog
  with Failure(s) ->
      Printf.fprintf stderr "[error] %s\n" s;
      exit 0

let read_file filename = 
    let file = open_in filename in
    let file_length = in_channel_length file in 
    let buff = Bytes.create file_length in
    really_input file buff 0 file_length;
    close_in file;
    Bytes.to_string buff

let () =
  let initial_state = Ast.new_state () in
  let file_content = (read_file "playground/fibo.imp") in
  let _ = exec_prog file_content initial_state in
  ()