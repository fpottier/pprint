(**************************************************************************)
(*                                                                        *)
(*  PPrint                                                                *)
(*                                                                        *)
(*  Francois Pottier, INRIA Paris-Rocquencourt                            *)
(*  Nicolas Pouillard, IT University of Copenhagen                        *)
(*                                                                        *)
(*  Copyright 2007-2013 INRIA. All rights reserved. This file is          *)
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

type raw_document =

    (* [Empty] is the empty document. *)

  | Empty

    (* [Char c] is a document that consists of the single character [c]. We
       enforce the invariant that [c] is not a newline character. *)

  | Char of char

    (* [String (s, ofs, len)] is a document that consists of the portion of
       the string [s] delimited by the offset [ofs] and the length [len]. We
       assume, but do not check, that this portion does not contain a newline
       character. *)

  | String of string * int * int

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

  | IfFlat of raw_document * raw_document

  (* When in flattening mode, [HardLine] causes a failure, which requires
     backtracking all the way until the stack is empty. When not in flattening
     mode, it represents a newline character, followed with an appropriate
     number of indentation. A common way of using [HardLine] is to only use it
     directly within the right branch of an [IfFlat] construct. *)

  | HardLine

  (* [Cat doc1 doc2] is the concatenation of the documents [doc1] and
     [doc2]. *)

  | Cat of raw_document * raw_document

    (* [Nest (j, doc)] is the document [doc], in which the indentation level
       has been increased by [j], that is, in which [j] blanks have been
       inserted after every newline character. *)

  | Nest of int * raw_document

    (* [Group doc] represents an alternative: it is either a flattened form of
       [doc], in which occurrences of [Group] disappear and occurrences of
       [IfFlat] resolve to their left branch, or [doc] itself. *)

  | Group of document

  (* TEMPORARY make [align] primitive *)

(* We distinguish between raw documents, as defined above, and documents,
   which carry additional information. This information is computed in a
   bottom-up manner when the document is constructed. *)

(* Crucially, the argument carried by [Group] is a document, as opposed to
   a raw document. This means that this additional information is stored
   at every [Group] node, where it allows us to avoid backtracking and
   buffering. *)

and document = {
  (* The underlying raw document. *)
  raw: raw_document;
  (* The apparent length of this document, if printed in flattening mode.
     In other words, this is the number of columns that this document
     needs in order to fit on a single line. We express this value in
     the set of `integers extended with infinity', and use the value
     [infinity] to indicate that this document cannot be printed on a
     single line. *)
  requirement: int; (* with infinity *)
}

(* ------------------------------------------------------------------------- *)

(* Infinity is encoded as [max_int]. *)

let infinity =
  max_int

(* Addition of integers with infinity. *)

let (++) x y =
  if x = infinity || y = infinity then
    infinity
  else
    x + y

(* ------------------------------------------------------------------------- *)

(* The above algebraic data type is not exposed to the user. Instead, we
   expose the following functions. These functions construct a raw document
   and compute its requirement, so as to obtain a document. *)

let empty = {
  raw = Empty;
  requirement = 0
}

let char c = {
  raw = (assert (c <> '\n'); Char c);
  requirement = 1
}

let space =
  char ' '

let substring s ofs len =
  if len = 0 then
    empty
  else {
    raw = String (s, ofs, len);
    requirement = len
  }

let string s =
  substring s 0 (String.length s)

let fancysubstring s ofs len apparent_length =
  if len = 0 then
    empty
  else {
    raw = FancyString (s, ofs, len, apparent_length);
    requirement = apparent_length
  }

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

let hardline = {
  raw = HardLine;
  requirement = infinity
    (* a hard line cannot be printed in flattening mode *)
}

let blank n =
  match n with
  | 0 ->
      empty
  | 1 ->
      space
  | _ ->
      {
        raw = Blank n;
        requirement = n
      }

let ifflat x y = {
  raw = IfFlat (x.raw, y.raw);
  requirement = x.requirement
    (* in flattening mode, the requirement of [ifflat x y] is just the
       requirement of its flat version, [x] *)
}

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
  match x.raw, y.raw with
  | Empty, _ ->
      y
  | _, Empty ->
      x
  | _, _ ->
      {
        raw = Cat (x.raw, y.raw);
        requirement = x.requirement ++ y.requirement
      }

let nest i x = {
  raw = (assert (i >= 0); Nest (i, x.raw));
  requirement = x.requirement
    (* [Nest] is ignored in flattening mode *)
}

let group x = {
  raw = Group x;
  requirement = x.requirement
    (* [Group] is ignored in flattening mode *)
}

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
  | KCons of int * bool * raw_document * cont

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

  let rec run (state : channel state) (indent : int) (flatten : bool) (doc : raw_document) (cont : cont) : unit =
    match doc with

    | Empty ->
	continue state cont

    | Char c ->
        Output.char state.channel c;
        state.column <- state.column + 1;
        (* assert (ok state flatten); *)
        continue state cont

    | String (s, ofs, len) ->
        Output.substring state.channel s ofs len;
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

    | Cat (doc1, doc2) ->
        (* Push the second document onto the continuation. *)
        run state indent flatten doc1 (KCons (indent, flatten, doc2, cont))

    | Nest (j, doc) ->
	run state (indent + j) flatten doc cont

    | Group doc ->
        (* If we already are in flattening mode, stay in flattening mode; we
           are committed to it. If we are not already in flattening mode, we
           have a choice of entering flattening mode. We enter this mode only
           if we know that this group fits on this line without violating the
           width or ribbon width constraints. Thus, we never backtrack. *)
        let flatten =
          flatten ||
          let column = state.column ++ doc.requirement in
          column <= state.width && column <= state.last_indent + state.ribbon
        in
        run state indent flatten doc.raw cont

  and continue state = function
    | KNil ->
        ()
    | KCons (indent, flatten, doc, cont) ->
        run state indent flatten doc cont

  (* This is the renderer's main entry point. *)

  let pretty rfrac width channel { raw = doc } =
    run {
      width = width;
      ribbon = max 0 (min width (truncate (float_of_int width *. rfrac)));
      channel = channel;
      last_indent = 0;
      column = 0
    } 0 false doc KNil

(* ------------------------------------------------------------------------- *)

(* The compact rendering algorithm. *)

  let compact channel { raw = doc } =

    let rec scan = function
      | Empty ->
	  ()
      | Char c ->
	  Output.char channel c
      | String (s, ofs, len) ->
	  Output.substring channel s ofs len
      | FancyString (s, ofs, len, apparent_length) ->
	  Output.substring channel s ofs len
      | Blank n ->
	  blanks channel n
      | HardLine ->
	  Output.char channel '\n'
      | Cat (doc1, doc2) ->
	  scan doc1;
	  scan doc2
      | IfFlat (doc, _)
      | Nest (_, doc)
      | Group { raw = doc } ->
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

