type imp_loc = string [@@deriving show, ord, eq]


type imp_aexp = 
  | Read
  | ILiteral  of int 
  | Variable  of imp_loc 
  | Sum       of imp_aexp * imp_aexp 
  | Sub       of imp_aexp * imp_aexp 
  | Times     of imp_aexp * imp_aexp
[@@deriving show, ord, eq]

type imp_bexp = 
  | BLiteral of bool 
  | Eq  of imp_aexp * imp_aexp 
  | Lte of imp_aexp * imp_aexp 
  | Lt  of imp_aexp * imp_aexp 
  | Gte of imp_aexp * imp_aexp 
  | Gt  of imp_aexp * imp_aexp
  | Not of imp_bexp
  | And of imp_bexp * imp_bexp
  | Or  of imp_bexp * imp_bexp
[@@deriving show, ord, eq]

type imp_com = 
  | Skip 
  | Print   of imp_aexp 
  | Assign  of imp_loc * imp_aexp 
  | Seq     of imp_com * imp_com 
  | IfThen  of imp_bexp * imp_com * imp_com
  | While   of imp_bexp * imp_com
[@@deriving show, ord, eq]
  
type program = imp_com

type state = imp_loc -> int

let new_state (_: unit): state = 
  let empty_sigma (_: imp_loc) = 0 (* Initially we assume each variable is set to 0 *)
  in empty_sigma

let update_state (id: imp_loc) (v: int) (sigma: state): state =
  let updated_sigma (x: imp_loc) = if x = id then v else sigma x 
  in updated_sigma