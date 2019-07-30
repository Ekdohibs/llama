type position = {
  start_pos : Lexing.position ;
  end_pos : Lexing.position ;
}

type 'a with_loc = {
  loc : position ;
  txt : 'a ;
}

type rec_flag = NonRecursive | Recursive

type expr = expr_desc with_loc
and expr_desc =
  | EVar of string with_loc
  | ECall of expr * expr option list
  | EVal of string with_loc * rec_flag * expr * expr
  | EDef of string with_loc * rec_flag * string with_loc list * expr * expr
  | ESequence of expr * expr
  | EReturn of expr
  | EBreak
  | EContinue
  | EFor of string with_loc * expr * expr
  | EWhile of expr * expr
  | EIf of expr * expr * expr option
  | EList of expr list
  | EArray of expr list
  | ETuple of expr list
  | EUnit

type decl = decl_desc with_loc
and decl_desc =
  | DVal of string with_loc * rec_flag * expr
  | DDef of string with_loc * rec_flag * string with_loc list * expr

type program = decl list
