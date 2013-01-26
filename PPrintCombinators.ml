open PPrintEngine

(* ------------------------------------------------------------------------- *)

(* Predefined single-character documents. *)

let lparen          = char '('
let rparen          = char ')'
let langle          = char '<'
let rangle          = char '>'
let lbrace          = char '{'
let rbrace          = char '}'
let lbracket        = char '['
let rbracket        = char ']'
let squote          = char '\''
let dquote          = char '"'
let bquote          = char '`'
let semi            = char ';'
let colon           = char ':'
let comma           = char ','
let space           = char ' '
let dot             = char '.'
let sharp           = char '#'
let slash           = char '/'
let backslash       = char '\\'
let equals          = char '='
let qmark           = char '?'
let tilde           = char '~'
let at              = char '@'
let percent         = char '%'
let dollar          = char '$'
let caret           = char '^'
let ampersand       = char '&'
let star            = char '*'
let plus            = char '+'
let minus           = char '-'
let underscore      = char '_'
let bang            = char '!'
let bar             = char '|'

(* ------------------------------------------------------------------------- *)

(* Repetition. *)

let twice doc =
  doc ^^ doc

let repeat n doc =
  let rec loop n doc accu =
    if n = 0 then
      accu
    else
      loop (n - 1) doc (doc ^^ accu)
  in
  loop n doc empty

(* ------------------------------------------------------------------------- *)

(* Delimiters. *)

let enclose l r x   = l ^^ x ^^ r

let squotes         = enclose squote squote
let dquotes         = enclose dquote dquote
let bquotes         = enclose bquote bquote
let braces          = enclose lbrace rbrace
let parens          = enclose lparen rparen
let angles          = enclose langle rangle
let brackets        = enclose lbracket rbracket

(* ------------------------------------------------------------------------- *)

(* Some functions on lists. *)

(* A variant of [map] that keeps track of the element index. *)

let mapi (f : int -> 'a -> 'b) (xs : 'a list) : 'b list =
  let r = ref 0 in
  List.map (fun x ->
    let i = !r in
    r := i + 1;
    f i x
  ) xs

(* A variant of [fold_left] that keeps track of the element index. *)

let foldli (f : int -> 'b -> 'a -> 'b) (accu : 'b) (xs : 'a list) : 'b =
  let r = ref 0 in
  List.fold_left (fun accu x ->
    let i = !r in
    r := i + 1;
    f i accu x
  ) accu xs

(* ------------------------------------------------------------------------- *)

(* Working with lists of documents. *)

let concat docs =
  (* We take advantage of the fact that [^^] operates in constant
     time, regardless of the size of its arguments. The document
     that is constructed is essentially a reversed list (i.e., a
     tree that is biased towards the left). This is not a problem;
     when pretty-printing this document, the engine will descend
     along the left branch, pushing the nodes onto its stack as
     it goes down, effectively reversing the list again. *)
  List.fold_left (^^) empty docs

let separate separator docs =
  foldli (fun i x y ->
    if i = 0 then
      y
    else
      x ^^ separator ^^ y
  ) empty docs

(* ------------------------------------------------------------------------- *)

(* Text. *)

(* This variant of [String.index_from] returns an option. *)

let index_from s i c =
  try
    Some (String.index_from s i c)
  with Not_found ->
    None

(* [lines s] chops the string [s] into a list of lines, which are turned
   into documents. *)

let lines s =
  let rec chop accu i =
    match index_from s i '\n' with
    | Some j ->
        let accu = substring s i (j - i) :: accu in
	chop accu (j + 1)
    | None ->
        substring s i (String.length s - i) :: accu
  in
  List.rev (chop [] 0)

(* [words s] chops the string [s] into a list of words, which are turned
   into documents. *)

let words s =
  let n = String.length s in
  (* A two-state finite automaton. *)
  (* In this state, we have skipped at least one blank character. *)
  let rec skipping accu i = 
    if i = n then
      (* There was whitespace at the end. Drop it. *)
      accu
    else match s.[i] with
    | ' '
    | '\t'
    | '\n'
    | '\r' ->
        (* Skip more whitespace. *)
	skipping accu (i + 1)
    | _ ->
        (* Begin a new word. *)
	word accu i (i + 1)
  (* In this state, we have skipped at least one non-blank character. *)
  and word accu i j =
    if j = n then
      (* Final word. *)
      substring s i (j - i) :: accu
    else match s.[j] with
    | ' '
    | '\t'
    | '\n'
    | '\r' ->
        (* A new word has been identified. *)
        let accu = substring s i (j - i) :: accu in	
	skipping accu (j + 1)
    | _ ->
        (* Continue inside the current word. *)
	word accu i (j + 1)
  in
  List.rev (skipping [] 0)

let flow docs =
  foldli (fun i accu doc ->
    if i = 0 then
      doc
    else
      accu ^^
      (* This idiom allows beginning a new line if [doc] does not
	 fit on the current line. *)
      group (break 1 ^^ doc)
  ) empty docs

(* ------------------------------------------------------------------------- *)

(* Low-level combinators for alignment and indentation. *)

let align d =
  column (fun k ->
    nesting (fun i ->
      nest (k - i) d
    )
  )

let hang i d =
  align (nest i d)

let indent i d =
  hang i (blank i ^^ d)

(* High-level combinators. *)



let safe_text s =
  separate (break 1) (lines s)

let rec fold1 f docs =
   match docs with
   | [] ->
       empty
   | [ doc ] ->
       doc
   | doc :: docs ->
       f doc (fold1 f docs)

let rec fold1map f g docs =
   match docs with
   | [] ->
       empty
   | [ doc ] ->
       g doc
   | doc :: docs ->
       let doc = g doc in (* force left-to-right evaluation *)
       f doc (fold1map f g docs)

let sepmap sep g docs =
  fold1map (fun x y -> x ^^ sep ^^ y) g docs

let optional f = function
  | None ->
      empty
  | Some x ->
      f x

let group1 d = group (nest 1 d)
let group2 d = group (nest 2 d)

module Operators = struct
  let ( !^ ) = text
  let ( ^^ ) = ( ^^ )
  let ( ^/^ ) x y = x ^^ break 1 ^^ y
  let ( ^//^ ) x y = group (x ^^ nest 2 (break 1 ^^ y))
  let ( ^@^ ) x y = group (x ^^ break 1 ^^ y)
  let ( ^@@^ ) x y = group2 (x ^^ break 1 ^^ y)
end

let group_break1 = group (break 1) (* TEMPORARY *)

open Operators
let prefix op x = !^op ^//^ x
let infix op x y = (x ^^ space ^^ !^op) ^//^ y
let infix_dot op x y = group2 ((x ^^ !^op) ^^ break 0 ^^ y)
let infix_com op x y = x ^^ !^op ^^ group_break1 ^^ y
let surround n sep open_doc contents close_doc =
  group (open_doc ^^ nest n (sep ^^ contents) ^^ sep ^^ close_doc)
let surround1 open_txt contents close_txt =
  surround 1 (break 0) !^open_txt contents !^close_txt
let surround2 open_txt contents close_txt =
  surround 2 (break 1) !^open_txt contents !^close_txt

let soft_surround n sep open_doc contents close_doc =
  group (open_doc ^^ nest n (group sep ^^ contents) ^^
         group (sep ^^ close_doc))

let seq indent break empty_seq open_seq sep_seq close_seq = function
  | [] -> empty_seq
  | xs ->
      surround indent break
        open_seq (fold1 (fun x xs -> x ^^ sep_seq ^^ xs) xs) close_seq
let seq1 open_txt sep_txt close_txt =
  seq 1 (break 0) !^(open_txt ^ close_txt) !^open_txt (!^sep_txt ^^ break 1) !^close_txt
let seq2 open_txt sep_txt close_txt =
  seq 2 (break 1) !^(open_txt ^ close_txt) !^open_txt (!^sep_txt ^^ break 1) !^close_txt

let sprintf fmt = Printf.ksprintf safe_text fmt

type constructor = string
type type_name = string
type record_field = string
type tag = int

(* A signature for value representations.
   This is compatible with the associated Camlp4 generator:
     Camlp4RepresentationGenerator *)

module type REPRESENTATION = sig
  (* The type of value representation *)
  type representation

  (* [variant type_name data_constructor_name tag arguments]
        Given information about the variant and its arguments,
        this function produces a new value representation. *)
  val variant : type_name -> constructor -> tag -> representation list -> representation

  (* [record type_name fields]
        Given a type name and a list of record fields, this function
        produces the value representation of a record. *)
  val record : type_name -> (record_field * representation) list -> representation

  (* [tuple arguments]
        Given a list of value representation this function produces
        a new value representation. *)
  val tuple : representation list -> representation

  (* ------------------------------------------------------------------------- *)

  (* Value representation for primitive types. *)

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

  (* Value representation for any other value. *)
  val unknown : type_name -> 'a -> representation
end

module type DOCUMENT_REPRESENTATION =
  REPRESENTATION with type representation = document

(* please remove as soon as this will be available in ocaml *)
module MissingFloatRepr = struct
  let valid_float_lexeme s =
    let l = String.length s in
    let rec loop i =
      if i >= l then s ^ "." else
      match s.[i] with
      | '0' .. '9' | '-' -> loop (i+1)
      | _ -> s
    in loop 0

  let float_repres f =
    match classify_float f with
      FP_nan -> "nan"
    | FP_infinite ->
        if f < 0.0 then "neg_infinity" else "infinity"
    | _ ->
        let s1 = Printf.sprintf "%.12g" f in
        if f = float_of_string s1 then valid_float_lexeme s1 else
        let s2 = Printf.sprintf "%.15g" f in
        if f = float_of_string s2 then valid_float_lexeme s2 else
        Printf.sprintf "%.18g" f
end

module ML = struct
  type representation = document
  let tuple = seq1 "(" "," ")"
  let variant _ cons _ args =
    if args = [] then !^cons else !^cons ^^ tuple args
  let record _ fields =
    seq2 "{" ";" "}" (List.map (fun (k, v) -> infix "=" !^k v) fields)
  let option f = function
    | Some x -> !^"Some" ^^ tuple [f x]
    | None -> !^"None"
  let list f xs = seq2 "[" ";" "]" (List.map f xs)
  let array f xs = seq2 "[|" ";" "|]" (Array.to_list (Array.map f xs))
  let ref f x = record "ref" ["contents", f !x]
  let float f = safe_text (MissingFloatRepr.float_repres f)
  let int = sprintf "%d"
  let int32 = sprintf "%ld"
  let int64 = sprintf "%Ld"
  let nativeint = sprintf "%nd"
  let char = sprintf "%C"
  let bool = sprintf "%B"
  let string = sprintf "%S"
  let unknown tyname _ = sprintf "<abstr:%s>" tyname
end

(* The following definitions have been deprecated. We keep them as a
   comment to help users of old versions perform the transition.
let line            = break1
let linebreak       = break0
let softline        = group break1
let softbreak       = group break0
*)
