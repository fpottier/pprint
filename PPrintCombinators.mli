open PPrintEngine

(** A set of high-level combinators for building documents. *)

(** {1 Single characters} *)

(** The following constant documents consist of a single character. *)

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
val slash: document
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

(** {1 Delimiters} *)

(* The following combinators enclose a document within a pair of delimiters. *)

val squotes: document -> document
val dquotes: document -> document
val bquotes: document -> document
val braces: document -> document
val parens: document -> document
val angles: document -> document
val brackets: document -> document

(** {1 Repetition} *)

(** [twice doc] is the document obtained by concatenating two copies of
    the document [doc]. *)
val twice: document -> document

(** [repeat n doc] is the document obtained by concatenating [n] copies of
    the document [doc]. *)
val repeat: int -> document -> document

(** {1 Lists and options} *)

(** [concat docs] is the concatenation of the documents in the list [docs]. *)
val concat: document list -> document

(** [separate sep docs] is the concatenation of the documents in the list
    [docs]. The separator [sep] is inserted between every two adjacent
    documents. *)
val separate: document -> document list -> document

(** [concat_map f xs] is equivalent to [concat (List.map f xs)]. *)
val concat_map: ('a -> document) -> 'a list -> document

(** [separate_map sep f xs] is equivalent to [separate sep (List.map f xs)]. *)
val separate_map: document -> ('a -> document) -> 'a list -> document

(** [optional f None] is the empty document. [optional f (Some x)] is
    the document [f x]. *)
val optional: ('a -> document) -> 'a option -> document

(** {1 Text} *)

(** [lines s] is the list of documents obtained by splitting [s] at newline
    characters. The code that looks for newline characters is not UTF-8
    aware. *)
val lines: string -> document list

(** [arbitrary_text s] is equivalent to [separate (break 1) (lines s)].
    It is analogous to [text s], but is valid even if the string [s]
    contains newline characters. *)
val arbitrary_text: string -> document

(** [words s] is the list of documents obtained by splitting [s] at whitespace
    characters. The code that looks for whitespace characters is not UTF-8
    aware. *)
val words: string -> document list

(** [flow docs] separates the documents in the list [docs] with brekable
    spaces in such a way that a new line begins whenever a document does not
    fit on the current line. This is useful for typesetting free-flowing,
    ragged-right text. *)
val flow: document list -> document

(** {1 Alignment and indentation} *)

(** [align doc] increases the indentation level to reach the current
    column. Thus, this document will be rendered within a box whose
    upper left corner is the current position. *)
val align: document -> document

(* [hang n doc] is analogous to [align], but additionally indents
   all lines, except the first one, by [n]. Thus, the text in the
   box forms a hanging indent. *)
val hang: int -> document -> document

(* ------------------------------------------------------------------------- *)

(** {1 High-level combinators} *)

(** The document [prefix spacing left right] has the following flat layout: {[
left right
]}
and the following non-flat layout:
{[
left
  right
]}
The parameter [spacing] controls the number of spaces between [left] and [right]
when rendered flat.
 *)
val prefix: int -> document -> document -> document

(** The document [infix spacing middle left right] has the following non-flat layout: {[
left middle right
]}
and the following non-flat layout: {[
left middle
  right
]}
The parameter [spacing] controls the number of spaces between [left] and [middle]
(always) and between [middle] and [right] (when rendered flat).
*)
val infix: int -> document -> document -> document -> document

(** [infix_com middle left right]
      Flat layout: [left][middle] [right]
      Otherwise:   [left][middle]
                     [right]
 *)
val infix_com: document -> document -> document -> document

(** [surround nesting break open_doc contents close_doc] *)
val surround: int -> document -> document -> document -> document -> document

(** [surround1 open_txt contents close_txt]
     Flat:      [open_txt][contents][close_txt]
     Otherwise: [open_txt]
                 [contents]
                [close_txt]
 *)
val surround1: document -> document -> document -> document

(** [surround2 open_txt contents close_txt]
     Flat:      [open_txt] [contents] [close_txt]
     Otherwise: [open_txt]
                  [contents]
                [close_txt]
 *)
val surround2: document -> document -> document -> document

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
val seq1: document -> document -> document -> document list -> document

(** [seq2 open_seq sep_seq close_seq contents]
     Flat layout: [open_seq] [contents][sep_seq]...[sep_seq][contents] [close_seq]
     Otherwise:   [open_seq]
                    [contents][sep_seq]...[sep_seq][contents]
                  [close_seq]
 *)
val seq2: document -> document -> document -> document list -> document

(** {1 Short-hands} *)

(** [!^s] is a short-hand for [text s]. *)
val ( !^ ) : string -> document

(** [x ^/^ y] separates [x] and [y] with a breakable space.
    It is a short-hand for [x ^^ break 1 ^^ y]. *)
val ( ^/^ ) : document -> document -> document

(** [x ^//^ y] is a short-hand for [prefix 1 x y]. *)
val ( ^//^ ) : document -> document -> document

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

  (** {1 Value representation for primitive types} *)

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

