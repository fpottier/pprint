(**************************************************************************)
(*                                                                        *)
(*  PPrint                                                                *)
(*                                                                        *)
(*  Francois Pottier, INRIA Paris-Rocquencourt                            *)
(*  Nicolas Pouillard, IT University of Copenhagen                        *)
(*                                                                        *)
(*  Copyright 2007-2014 INRIA. All rights reserved. This file is          *)
(*  distributed under the terms of the CeCILL-C license, as described     *)
(*  in the file LICENSE.                                                  *)
(*                                                                        *)
(**************************************************************************)

(* ------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------- *)

(* A uniform interface for output channels. *)

module type OUTPUT = sig
  type channel
  val char: channel -> char -> unit
  val substring: channel -> string -> int (* offset *) -> int (* length *) -> unit
end

(* ------------------------------------------------------------------------- *)

(* Three implementations of the above interface, respectively based on output
   channels, memory buffers, and formatters. This compensates for the fact
   that ocaml's standard library does not allow creating an output channel
   that feeds into a memory buffer (a regrettable omission). *)

module ChannelOutput : OUTPUT with type channel = out_channel = struct
  type channel = out_channel
  let char = output_char
  let substring = output
end

module BufferOutput : OUTPUT with type channel = Buffer.t = struct
  type channel = Buffer.t
  let char = Buffer.add_char
  let substring = Buffer.add_substring
end

module FormatterOutput : OUTPUT with type channel = Format.formatter = struct
  type channel = Format.formatter
  let char = Format.pp_print_char
  let substring fmt = fst (Format.pp_get_formatter_output_functions fmt ())
end

(* ------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------- *)

(* Here is the algebraic data type of documents. It is analogous to Daan
   Leijen's version, but the binary constructor [Union] is replaced with
   the unary constructor [Group], and the constant [Line] is replaced with
   more general constructions, namely [IfFlat], which provides alternative
   forms depending on the current flattening mode, and [HardLine], which
   represents a newline character, and causes a failure in flattening mode. *)

type document =

  (* [Empty] is the empty document. *)

  | Empty

  (* [Char c] is a document that consists of the single character [c]. We
     enforce the invariant that [c] is not a newline character. *)

  | Char of char

  (* [String s] is a document that consists of just the string [s]. We
     assume, but do not check, that this string does not contain a newline
     character. [String] is a special case of [FancyString], which takes up
     less space in memory. *)

  | String of string

  (* [FancyString (s, ofs, len, apparent_length)] is a (portion of a) string
     that may contain fancy characters: color escape characters, UTF-8 or
     multi-byte characters, etc. Thus, the apparent length (which corresponds
     to what will be visible on screen) differs from the length (which is a
     number of bytes, and is reported by [String.length]). We assume, but do
     not check, that fancystrings do not contain a newline character. *)

  | FancyString of string * int * int * int

  (* [Blank n] is a document that consists of [n] blank characters. *)

  | Blank of int

    (* When in flattening mode, [IfFlat (d1, d2)] turns into the document
       [d1]. When not in flattening mode, it turns into the document [d2]. *)

  | IfFlat of document * document

  (* When in flattening mode, [HardLine] causes a failure, which requires
     backtracking all the way until the stack is empty. When not in flattening
     mode, it represents a newline character, followed with an appropriate
     number of indentation. A common way of using [HardLine] is to only use it
     directly within the right branch of an [IfFlat] construct. *)

  | HardLine

  (* The following constructors store their space requirement. This is the
     document's apparent length, if printed in flattening mode. This
     information is computed in a bottom-up manner when the document is
     constructed. *)

  (* In other words, the space requirement is the number of columns that the
     document needs in order to fit on a single line. We express this value in
     the set of `integers extended with infinity', and use the value
     [infinity] to indicate that the document cannot be printed on a single
     line. *)

  (* Storing this information at [Group] nodes is crucial, as it allows us to
     avoid backtracking and buffering. *)

  (* Storing this information at other nodes allows the function [requirement]
     to operate in constant time. This means that the bottom-up computation of
     requirements takes linear time. *)

  (* [Cat (req, doc1, doc2)] is the concatenation of the documents [doc1] and
     [doc2]. The space requirement [req] is the sum of the requirements of
     [doc1] and [doc2]. *)

  | Cat of requirement * document * document

  (* [Nest (req, j, doc)] is the document [doc], in which the indentation
     level has been increased by [j], that is, in which [j] blanks have been
     inserted after every newline character. The space requirement [req] is
     the same as the requirement of [doc]. *)

  | Nest of requirement * int * document

  (* [Group (req, doc)] represents an alternative: it is either a flattened
     form of [doc], in which occurrences of [Group] disappear and occurrences
     of [IfFlat] resolve to their left branch, or [doc] itself. The space
     requirement [req] is the same as the requirement of [doc]. *)

  | Group of requirement * document

  (* [Align (req, doc)] increases the indentation level to reach the current
     column.  Thus, the document [doc] is rendered within a box whose upper
     left corner is the current position. The space requirement [req] is the
     same as the requirement of [doc]. *)

  | Align of requirement * document

and requirement =
    int (* with infinity *)

(* ------------------------------------------------------------------------- *)

(* Infinity is encoded as [max_int]. *)

let infinity : requirement =
  max_int

(* Addition of integers with infinity. *)

let (++) (x : requirement) (y : requirement) : requirement =
  if x = infinity || y = infinity then
    infinity
  else
    x + y

(* Comparison between an integer with infinity and a normal integer. *)

let (<==) (x : requirement) (y : int) =
  x <= y

(* ------------------------------------------------------------------------- *)

(* Retrieving or computing the space requirement of a document. *)

let rec requirement = function
  | Empty ->
      0
  | Char _ ->
      1
  | String s ->
      String.length s
  | FancyString (_, _, _, len)
  | Blank len ->
      len
  | IfFlat (doc1, _) ->
      (* In flattening mode, the requirement of [ifflat x y] is just the
         requirement of its flat version, [x]. *)
      (* The smart constructor [ifflat] ensures that [IfFlat] is never nested
         in the left-hand side of [IfFlat], so this recursive call is not a
         problem; the function [requirement] has constant time complexity. *)
      requirement doc1
  | HardLine ->
      (* A hard line cannot be printed in flattening mode. *)
      infinity
  | Cat (req, _, _)
  | Nest (req, _, _)
  | Group (req, _) 
  | Align (req, _) ->
      (* These nodes store their requirement -- which is computed when the
         node is constructed -- so as to allow us to answer in constant time
         here. *)
      req

(* ------------------------------------------------------------------------- *)

(* The above algebraic data type is not exposed to the user. Instead, we
   expose the following functions. These functions construct a raw document
   and compute its requirement, so as to obtain a document. *)

let empty =
  Empty

let char c =
  assert (c <> '\n');
  Char c

let space =
  char ' '

let string s =
  String s

let fancysubstring s ofs len apparent_length =
  if len = 0 then
    empty
  else
    FancyString (s, ofs, len, apparent_length)

let substring s ofs len =
  fancysubstring s ofs len len

let fancystring s apparent_length =
  fancysubstring s 0 (String.length s) apparent_length

(* The following function was stolen from [Batteries]. *)
let utf8_length s =
  let rec length_aux s c i =
    if i >= String.length s then c else
    let n = Char.code (String.unsafe_get s i) in
    let k =
      if n < 0x80 then 1 else
      if n < 0xe0 then 2 else
      if n < 0xf0 then 3 else 4
    in
    length_aux s (c + 1) (i + k)
  in
  length_aux s 0 0

let utf8string s =
  fancystring s (utf8_length s)

let hardline =
  HardLine

let blank n =
  match n with
  | 0 ->
      empty
  | 1 ->
      space
  | _ ->
      Blank n

let ifflat doc1 doc2 =
  (* Avoid nesting [IfFlat] in the left-hand side of [IfFlat], as this
     is redundant. *)
  match doc1 with
  | IfFlat (doc1, _)
  | doc1 ->
      IfFlat (doc1, doc2)

let internal_break i =
  ifflat (blank i) hardline

let break0 =
  internal_break 0

let break1 =
  internal_break 1

let break i =
  match i with
  | 0 ->
      break0
  | 1 ->
      break1
  | _ ->
      internal_break i

let (^^) x y =
  match x, y with
  | Empty, _ ->
      y
  | _, Empty ->
      x
  | _, _ ->
      Cat (requirement x ++ requirement y, x, y)

let nest i x =
  assert (i >= 0);
  Nest (requirement x, i, x)

let group x =
  let req = requirement x in
  (* Minor optimisation: an infinite requirement dissolves a group. *)
  if req = infinity then
    x
  else
    Group (req, x)

let align x =
  Align (requirement x, x)

(* ------------------------------------------------------------------------- *)

(* The pretty rendering algorithm: preliminary declarations. *)

(* The renderer is supposed to behave exactly like Daan Leijen's, although its
   implementation is quite radically different, and simpler. Our documents are
   constructed eagerly, as opposed to lazily. This means that we pay a large
   space overhead, but in return, we get the ability of computing information
   bottom-up, as described above, which allows to render documents without
   backtracking or buffering. *)

(* The renderer maintains the following state. This state is partly mutable.
   It is never duplicated; it is just threaded through. *)

type 'channel state = {

    (* The line width and ribbon width. *)

    width: int;
    ribbon: int;

    (* The output channel. *)

    channel: 'channel;

    (* The last indent. This is the number of blanks that were printed at the
       beginning of the current line. *)

    mutable last_indent: int;

    (* The current column. *)

    mutable column: int;

  }

(* The renderer is written in tail-recursive style. Its explicit continuation
   can be viewed as a sequence of pending calls to [run]. *)

type cont =
  | KNil
  | KCons of int * bool * document * cont

(* ------------------------------------------------------------------------- *)

(* The pretty rendering algorithm. *)

(* The renderer is parameterized over an implementation of output channels. *)

module Renderer (Output : OUTPUT) = struct

  type channel =
      Output.channel

  type dummy =
      document
  type document =
      dummy

  (* Printing blank space (indentation characters). *)

  let blank_length =
    80

  let blank_buffer =
    String.make blank_length ' '

  let rec blanks channel n =
    if n <= 0 then
      ()
    else if n <= blank_length then
      Output.substring channel blank_buffer 0 n
    else begin
      Output.substring channel blank_buffer 0 blank_length;
      blanks channel (n - blank_length)
    end

  (* This function expresses the following invariant: if we are in flattening
     mode, then we must be within bounds, i.e. the width and ribbon width
     constraints must be respected. *)

  let ok state flatten : bool =
    not flatten ||
    state.column <= state.width && state.column <= state.last_indent + state.ribbon

  (* The renderer. *)

  let rec run (state : channel state) (indent : int) (flatten : bool) (doc : document) (cont : cont) : unit =
    match doc with

    | Empty ->
	continue state cont

    | Char c ->
        Output.char state.channel c;
        state.column <- state.column + 1;
        (* assert (ok state flatten); *)
        continue state cont

    | String s ->
        let len = String.length s in
        Output.substring state.channel s 0 len;
        state.column <- state.column + len;
        (* assert (ok state flatten); *)
        continue state cont

    | FancyString (s, ofs, len, apparent_length) ->
        Output.substring state.channel s ofs len;
        state.column <- state.column + apparent_length;
        (* assert (ok state flatten); *)
        continue state cont

    | Blank n ->
        blanks state.channel n;
        state.column <- state.column + n;
        (* assert (ok state flatten); *)
        continue state cont

    | HardLine ->
        (* We cannot be in flattening mode, because a hard line has an [infinity]
           requirement, and we attempt to render a group in flattening mode only
           if this group's requirement is met. *)
        assert (not flatten);
        (* Emit a newline character, followed by the prescribed amount of
           indentation. Update the current state to record how many indentation
           characters were printed and to to reflect the new column number. *)
	Output.char state.channel '\n';
	blanks state.channel indent;
	state.column <- indent;
	state.last_indent <- indent;
        continue state cont

    | IfFlat (doc1, doc2) ->
        (* Pick an appropriate sub-document, based on the current flattening
           mode. *)
        run state indent flatten (if flatten then doc1 else doc2) cont

    | Cat (_, doc1, doc2) ->
        (* Push the second document onto the continuation. *)
        run state indent flatten doc1 (KCons (indent, flatten, doc2, cont))

    | Nest (_, j, doc) ->
	run state (indent + j) flatten doc cont

    | Group (req, doc) ->
        (* If we already are in flattening mode, stay in flattening mode; we
           are committed to it. If we are not already in flattening mode, we
           have a choice of entering flattening mode. We enter this mode only
           if we know that this group fits on this line without violating the
           width or ribbon width constraints. Thus, we never backtrack. *)
        let flatten =
          flatten ||
          let column = state.column ++ req in
          column <== state.width && column <== state.last_indent + state.ribbon
        in
        run state indent flatten doc cont

    | Align (_, doc) ->
        (* Get the current column. *)
        let k = state.column in
        (* Get the last indent. *)
        let i = state.last_indent in
        (* Act as [Nest (_, k - i, doc)]. *)
        run state (indent + k - i) flatten doc cont

  and continue state = function
    | KNil ->
        ()
    | KCons (indent, flatten, doc, cont) ->
        run state indent flatten doc cont

  (* This is the renderer's main entry point. *)

  let pretty rfrac width channel doc =
    run {
      width = width;
      ribbon = max 0 (min width (truncate (float_of_int width *. rfrac)));
      channel = channel;
      last_indent = 0;
      column = 0
    } 0 false doc KNil

(* ------------------------------------------------------------------------- *)

(* The compact rendering algorithm. *)

  let compact channel doc =

    let rec scan = function
      | Empty ->
	  ()
      | Char c ->
	  Output.char channel c
      | String s ->
	  Output.substring channel s 0 (String.length s)
      | FancyString (s, ofs, len, apparent_length) ->
	  Output.substring channel s ofs len
      | Blank n ->
	  blanks channel n
      | HardLine ->
	  Output.char channel '\n'
      | Cat (_, doc1, doc2) ->
	  scan doc1;
	  scan doc2
      | IfFlat (doc, _)
      | Nest (_, _, doc)
      | Group (_, doc)
      | Align (_, doc) ->
	  scan doc
    in

    scan doc

end

(* ------------------------------------------------------------------------- *)

(* Instantiating the renderers for the three kinds of output channels. *)

module ToChannel =
  Renderer(ChannelOutput)

module ToBuffer =
  Renderer(BufferOutput)

module ToFormatter =
  Renderer(FormatterOutput)

