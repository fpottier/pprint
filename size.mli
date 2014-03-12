(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: size.mli,v 1.5 2008-07-21 14:53:06 filliatr Exp $ i*)

(* Sizes of ocaml values (in their memory representation). 
   Sizes are given in words ([size_w]), bytes ([size_b]) or kilobytes
   ([size_kb]), in a system-independent way. *)

val size_w : 'a -> int

val size_b : 'a -> int

val size_kb : 'a -> int
