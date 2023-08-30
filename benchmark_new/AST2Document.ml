(* Converting an AST to a PPrint document. *)

open PPrint
open AST

let lparen = lparen ^^ ifflat empty space
let rparen = ifflat empty hardline ^^ rparen
let add    = space ^^ plus ^^ break 1
let sub    = space ^^ minus ^^ break 1
let mul    = space ^^ star ^^ break 1
let div    = space ^^ slash ^^ break 1

let[@inline] const i =
  utf8format "%d" i

let[@inline] paren d =
  nest 2 (lparen ^^ d) ^^ rparen

let rec factor e =
  group begin match e with
  | EConst i              -> const i
  | EUnOp (UNeg, e)       -> minus ^^ break 0 ^^ factor e
  | _                     -> paren (expr e)
  end

and term e =
  group begin match e with
  | EBinOp (e1, BMul, e2) -> term e1 ^^ mul ^^ factor e2
  | EBinOp (e1, BDiv, e2) -> term e1 ^^ div ^^ factor e2
  | _                     -> factor e
  end

and expr e =
  group begin match e with
  | EBinOp (e1, BAdd, e2) -> expr e1 ^^ add ^^ term e2
  | EBinOp (e1, BSub, e2) -> expr e1 ^^ sub ^^ term e2
  | _                     -> term e
  end

and main       : AST.main -> document = function
  | e                     -> expr e ^^ hardline
