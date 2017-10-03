(*i $Id: size.mli,v 1.5 2008-07-21 14:53:06 filliatr Exp $ i*)

(* Sizes of ocaml values (in their memory representation). 
   Sizes are given in words ([size_w]), bytes ([size_b]) or kilobytes
   ([size_kb]), in a system-independent way. *)

val size_w : 'a -> int

val size_b : 'a -> int

val size_kb : 'a -> int
