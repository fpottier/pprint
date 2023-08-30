(******************************************************************************)
(*                                                                            *)
(*                                    PPrint                                  *)
(*                                                                            *)
(*                        François Pottier, Inria Paris                       *)
(*                              Nicolas Pouillard                             *)
(*                                                                            *)
(*         Copyright 2007-2022 Inria. All rights reserved. This file is       *)
(*        distributed under the terms of the GNU Library General Public       *)
(*        License, with an exception, as described in the file LICENSE.       *)
(*                                                                            *)
(******************************************************************************)

(* Unary operators. *)

type unop =
  | UNeg

(* Binary operators. *)

type binop =
  | BAdd
  | BSub
  | BMul
  | BDiv

(* Expressions. *)

type expr =
  | EConst of int
  | EUnOp of unop * expr
  | EBinOp of expr * binop * expr

type main =
  expr
