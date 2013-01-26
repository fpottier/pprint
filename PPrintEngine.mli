(**************************************************************************)
(*  PPrint                                                                *)
(*                                                                        *)
(*  Authors: François Pottier and Nicolas Pouillard,                      *)
(*           INRIA Paris-Rocquencourt                                     *)
(*  Version: 20100311                                                     *)
(*                                                                        *)
(*  The copyright to this code is held by Institut National de Recherche  *)
(*  en Informatique et en Automatique (INRIA). All rights reserved. This  *)
(*  file is distributed under the license CeCILL-C (see file LICENSE).    *)
(*                                                                        *)
(**************************************************************************)

(* ------------------------------------------------------------------------- *)

(** {4 Low-level combinators for building documents.} *)

(** Documents must be built in memory before they are rendered. This may seem
    costly, but it is a simple approach, and works well. *)

(** This is the abstract type of documents. *)
type document

(** The following basic (low-level) combinators allow constructing documents. *)

(** [empty] is the empty document. *)
val empty: document

(** [char c] is a document that consists of the single character [c]. This
    character must not be a newline. *)
val char: char -> document

(** [text s] is a document that consists of the string [s]. This string must
    not contain a newline. *)
val text: string -> document

(** [substring s ofs len] is a document that consists of the portion of the
    string [s] delimited by the offset [ofs] and the length [len]. This
    portion must contain a newline. *)
val substring: string -> int -> int -> document

(** [fancytext s] is a document that consists of the string [s]. This string
    must not contain a newline. The string may contain fancy characters: color
    escape characters, UTF-8 or multi-byte characters, etc. Thus, its apparent
    length (which measures how many columns the text will take up on screen)
    differs from its length in bytes. *)
val fancytext: string -> int -> document

(** [fancysubstring s ofs len apparent_length] is a document that consists of
    the portion of the string [s] delimited by the offset [ofs] and the length
    [len]. This portion must contain a newline. The string may contain fancy
    characters. *)
val fancysubstring : string -> int -> int -> int -> document

(** [utf8text s] is a document that consists of the UTF-8-encoded string [s].
    This string must not contain a newline. *)
val utf8text: string -> document

(** [blank n] is a document that consists of [n] blank characters. *)
val blank: int -> document

(** [doc1 ^^ doc2] is the concatenation of the documents [doc1] and [doc2]. *)
val (^^): document -> document -> document

(** [nest j doc] is the document [doc], in which the indentation level has
    been increased by [j], that is, in which [j] blanks have been inserted
    after every newline character. Read this again: indentation is inserted
    after every newline character. No indentation is inserted at the beginning
    of the document. *)
val nest: int -> document -> document

(** [group doc] encodes an alternative. If [doc] fits on a single line, then
    [group doc] is rendered on a single line, like [doc]. Otherwise, the group
    is dissolved, and [group doc] is rendered like [doc]. There might be
    further groups within [doc], whose presence will lead to further choices
    being explored. *)
val group: document -> document

(** [column f] is the document obtained by applying the function [f] to the
    current column number. This combinator allows making the construction of
    a document dependent on the current column number. *)
val column: (int -> document) -> document

(* [nesting f] is the document obtained by applying the function [f] to the
   current indentation level, that is, the number of indentation (blank)
   characters that were inserted at the beginning of the current line. *)
val nesting: (int -> document) -> document

(* ------------------------------------------------------------------------- *)

(** {4 Low-level combinators for alignment and indentation.} *)

val align: document -> document
val hang: int -> document -> document
val indent: int -> document -> document

(* ------------------------------------------------------------------------- *)

(** {4 High-level combinators for building documents.} *)

(** [break n] Puts [n] spaces in flat mode and a new line otherwise.
   Equivalent to: [ifflat (String.make n ' ') hardline] *)
val break: int -> document

(** [break0] equivalent to [break 0] *)
val break0: document

(** [break1] equivalent to [break 1] *)
val break1: document

val string: string -> document
val fancy: (string -> int) -> string -> document
val words: string -> document

val lparen: document
val rparen: document
val langle: document
val rangle: document
val lbrace: document
val rbrace: document
val lbracket: document
val rbracket: document
val squote: document
val dquote: document
val bquote: document
val semi: document
val colon: document
val comma: document
val space: document
val dot: document
val sharp: document
val backslash: document
val equals: document
val qmark: document
val tilde: document
val at: document
val percent: document
val dollar: document
val caret: document
val ampersand: document
val star: document
val plus: document
val minus: document
val underscore: document
val bang: document
val bar: document

val squotes: document -> document
val dquotes: document -> document
val bquotes: document -> document
val braces: document -> document
val parens: document -> document
val angles: document -> document
val brackets: document -> document

val fold: (document -> document -> document) -> document list -> document
val fold1: (document -> document -> document) -> document list -> document
val fold1map: (document -> document -> document) -> ('a -> document) -> 'a list -> document
val sepmap: document -> ('a -> document) -> 'a list -> document

val optional: ('a -> document) -> 'a option -> document

(** [prefix left right]
      Flat layout: [left] [right]
      Otherwise:   [left]
                     [right]
 *)
val prefix: string -> document -> document

(** [infix middle left right]
      Flat layout: [left] [middle] [right]
      Otherwise:   [left] [middle]
                     [right]
 *)
val infix: string -> document -> document -> document

(** [infix_com middle left right]
      Flat layout: [left][middle] [right]
      Otherwise:   [left][middle]
                     [right]
 *)
val infix_com: string -> document -> document -> document

(** [infix_dot middle left right]
      Flat layout: [left][middle][right]
      Otherwise: [left][middle]
                    [right]
 *)
val infix_dot: string -> document -> document -> document

(** [surround nesting break open_doc contents close_doc] *)
val surround: int -> document -> document -> document -> document -> document

(** [surround1 open_txt contents close_txt]
     Flat:      [open_txt][contents][close_txt]
     Otherwise: [open_txt]
                 [contents]
                [close_txt]
 *)
val surround1: string -> document -> string -> document

(** [surround2 open_txt contents close_txt]
     Flat:      [open_txt] [contents] [close_txt]
     Otherwise: [open_txt]
                  [contents]
                [close_txt]
 *)
val surround2: string -> document -> string -> document

(** [soft_surround nesting break open_doc contents close_doc] *)
val soft_surround: int -> document -> document -> document -> document -> document

(** [seq indent break empty_seq open_seq sep_seq close_seq contents] *)
val seq: int -> document -> document -> document -> document -> document ->
         document list -> document 

(** [seq1 open_seq sep_seq close_seq contents]
     Flat layout: [open_seq][contents][sep_seq]...[sep_seq][contents][close_seq]
     Otherwise:   [open_seq]
                   [contents][sep_seq]...[sep_seq][contents]
                  [close_seq]
 *)
val seq1: string -> string -> string -> document list -> document

(** [seq2 open_seq sep_seq close_seq contents]
     Flat layout: [open_seq] [contents][sep_seq]...[sep_seq][contents] [close_seq]
     Otherwise:   [open_seq]
                    [contents][sep_seq]...[sep_seq][contents]
                  [close_seq]
 *)
val seq2: string -> string -> string -> document list -> document

(** [group1 d] equivalent to [group (nest 1 d)] *)
val group1: document -> document

(** [group2 d] equivalent to [group (nest 2 d)] *)
val group2: document -> document

module Operators : sig
  val ( ^^ ) : document -> document -> document
  val ( !^ ) : string -> document
  val ( ^/^ ) : document -> document -> document
  val ( ^//^ ) : document -> document -> document
  val ( ^@^ ) : document -> document -> document
  val ( ^@@^ ) : document -> document -> document
end

(* ------------------------------------------------------------------------- *)

(** {4 Renderers that produce output in an output channel, in a memory buffer,
    or in a formatter channel.} *)

open PPrintRenderer

module Channel : RENDERER with type channel = out_channel and type document = document

module PpBuffer : RENDERER with type channel = Buffer.t and type document = document

module Formatter : RENDERER with type channel = Format.formatter and type document = document

(* ------------------------------------------------------------------------- *)

type constructor = string
type type_name = string
type record_field = string
type tag = int

(** A signature for value representations.
   This is compatible with the associated Camlp4 generator:
     Camlp4RepresentationGenerator *)

module type REPRESENTATION = sig
  (** The type of value representation *)
  type representation

  (** [variant type_name data_constructor_name tag arguments]
        Given information about the variant and its arguments,
        this function produces a new value representation. *)
  val variant : type_name -> constructor -> tag -> representation list -> representation

  (** [record type_name fields]
        Given a type name and a list of record fields, this function
        produces the value representation of a record. *)
  val record : type_name -> (record_field * representation) list -> representation

  (** [tuple arguments]
        Given a list of value representation this function produces
        a new value representation. *)
  val tuple : representation list -> representation

  (* ------------------------------------------------------------------------- *)

  (** {4 Value representation for primitive types.} *)

  val string : string -> representation
  val int : int -> representation
  val int32 : int32 -> representation
  val int64 : int64 -> representation
  val nativeint : nativeint -> representation
  val float : float -> representation
  val char : char -> representation
  val bool : bool -> representation
  val option : ('a -> representation) -> 'a option -> representation
  val list : ('a -> representation) -> 'a list -> representation
  val array : ('a -> representation) -> 'a array -> representation
  val ref : ('a -> representation) -> 'a ref -> representation

  (** Value representation for any other value. *)
  val unknown : type_name -> 'a -> representation
end

(** A signature for source printers. *)

module type DOCUMENT_REPRESENTATION =
  REPRESENTATION with type representation = document

module ML : DOCUMENT_REPRESENTATION

(** {4 Deprecated} *)
val line: document
val linebreak: document
val softline: document
val softbreak: document
