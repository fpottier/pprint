(* ------------------------------------------------------------------------- *)

(* The following signature is common to the old and new engines. *)

module type ENGINE = sig

  type document
  val empty: document
  val char: char -> document
  val string: string -> document
  val substring: string -> int -> int -> document
  val fancystring: string -> int -> document
  val fancysubstring : string -> int -> int -> int -> document
  val utf8string: string -> document
  val hardline: document
  val blank: int -> document
  val break: int -> document
  val (^^): document -> document -> document
  val nest: int -> document -> document
  val group: document -> document
  val ifflat: document -> document -> document

  module ToBuffer : PPrintRenderer.RENDERER
    with type channel = Buffer.t
     and type document = document

end

(* ------------------------------------------------------------------------- *)

(* We use our own abstract syntax of documents. We produce random documents
   in this syntax first, then (as part of the timed test) translate them to
   the engine's syntax. This allows timing the engine's document construction
   code too. *)

type mydoc =
  | MyEmpty
  | MyChar of char
  | MyString of string
  | MySubString of string * int * int
  | MyUtf8String of string
  | MyHardLine
  | MyBlank of int
  | MyBreak of int
  | MyCat of mydoc * mydoc
  | MyNest of int * mydoc
  | MyGroup of mydoc
  | MyIfFlat of mydoc * mydoc

(* ------------------------------------------------------------------------- *)

(* [measure v] measures the size of an OCaml value [v] in memory. *)

let measure v =
  String.length (Marshal.to_string v [ Marshal.No_sharing ])

(* ------------------------------------------------------------------------- *)

(* [split n] produces two numbers [n1] and [n2] comprised between [0] and [n]
   (inclusive) whose sum is [n]. *)

let split n =
  let n1 = Random.int (n + 1) in
  let n2 = n - n1 in
  n1, n2

(* [choose xs] randomly and uniformly chooses between the elements of the
   list [xs]. *)

let choose xs =
  List.nth xs (Random.int (List.length xs))

(* [choose xs] randomly and uniformly chooses between the elements of the
   array [xs]. *)

let choose xs =
  Array.unsafe_get xs (Random.int (Array.length xs))

(* [pick] is analogous, but each element comes with a relative integer weight. *)

let pick wxs =
  (* Compute the total weight. *)
  let weight = List.fold_left (fun weight (w, _) -> weight + w) 0 wxs in
  assert (weight > 0);
  (* Pick a random integer between 0 and the total weight. *)
  let i = Random.int weight in
  (* Find the corresponding element. *)
  let rec loop i wxs =
    match wxs with
    | [] ->
        assert false
    | (w, x) :: wxs ->
        if i < w then x else loop (i - w) wxs
  in
  loop i wxs

(* ------------------------------------------------------------------------- *)

(* A random document generator. *)

let leaf =
  [|
    MyChar 'c';
    MyString "hello";
    MySubString ("the cat", 4, 3);
    MyUtf8String "étoile";
    MyHardLine;
    MyBlank 2;
    MyBreak 2
  |]

let rec random (n : int) : mydoc =
  (* If the budget is 0, produce an empty document. *)
  if n = 0 then
    MyEmpty
  (* If the budget is 1, produce a leaf. *)
  else if n = 1 then
    choose leaf
  (* Otherwise, decrement the budget, and produce a node of nonzero
     arity, spending the rest of the budget on the children. *)
  else
    let n = n - 1 in
    Lazy.force (pick [
      10, lazy (let n1, n2 = split n in MyCat (random n1, random n2));
       2, lazy (MyNest (2, random n));
      10, lazy (MyGroup (random n));
       2, lazy (let n1, n2 = split n in MyIfFlat (random n1, random n2))
    ])

(* ------------------------------------------------------------------------- *)

(* Building documents for a particular engine. *)

module Build (E : ENGINE) = struct

  open E

  let rec build (doc : mydoc) : document =
    match doc with
    | MyEmpty ->
        empty
    | MyChar c ->
        char c
    | MyString s ->
        string s
    | MySubString (s, ofs, len) ->
        substring s ofs len
    | MyUtf8String s ->
        utf8string s
    | MyHardLine ->
        hardline
    | MyBlank b ->
        blank b
    | MyBreak b ->
        break b
    | MyCat (doc1, doc2) ->
        build doc1 ^^ build doc2
    | MyNest (i, doc) ->
        nest i (build doc)
    | MyGroup doc ->
        group (build doc)
    | MyIfFlat (doc1, doc2) ->
        ifflat (build doc1) (build doc2)

end

(* ------------------------------------------------------------------------- *)

(* The rendering parameters. *)

let rfrac =
  0.8

let width =
  80

(* ------------------------------------------------------------------------- *)

(* Testing an engine, alone. *)

module Test1 (E : ENGINE) = struct

  open E

  (* The size of the randomly generated documents. *)
  let n =
    1000

  (* The number of runs. *)
  let runs =
    10000

  let () =
    let module B = Build(E) in
    let s = ref 0 in
    for r = 1 to runs do
      let document = B.build (random n) in
      s := !s + measure document;
      let buffer = Buffer.create 32768 in
      ToBuffer.pretty rfrac width buffer document;
      let buffer = Buffer.create 32768 in
      ToBuffer.compact buffer document
    done;
    let average = float_of_int !s /. float_of_int runs in
    Printf.printf "Test 1: success. Average document size: %d bytes.\n%!" (truncate average)

end

(* ------------------------------------------------------------------------- *)

(* Testing two engines and comparing their output. *)

module Test2 (E1 : ENGINE) (E2 : ENGINE) = struct

  (* The size of the randomly generated documents. *)
  let n =
    1000

  (* The number of runs. *)
  let runs =
    10000

  let () =
    let module B1 = Build(E1) in
    let module B2 = Build(E2) in
    for r = 1 to runs do
      let document = random n in
      let document1 = B1.build document in
      let document2 = B2.build document in
      let buffer1 = Buffer.create 32768 in
      E1.ToBuffer.pretty rfrac width buffer1 document1;
      let buffer2 = Buffer.create 32768 in
      E2.ToBuffer.pretty rfrac width buffer2 document2;
      assert (Buffer.contents buffer1 = Buffer.contents buffer2)
    done;
    Printf.printf "Test 2: success.\n%!"

end

(* ------------------------------------------------------------------------- *)

(* Timing an engine, alone. *)

module Time1 (E : ENGINE) (D : sig val n: int val runs: int val docs : mydoc array end) = struct

  open E
  open D

  let gc =
    true

  let time f x =
    if gc then
      Gc.minor();
    let start = Unix.gettimeofday() in
    let y = f x in
    let finish = Unix.gettimeofday() in
    y, finish -. start

  let () =
    let module B = Build(E) in
    Printf.printf "Time: building documents...\n%!";
    let docs, duration = time (fun () -> Array.map B.build docs) () in
    Printf.printf "Time: built %d documents of size %d in %.2f seconds.\n%!" runs n duration;
    let buffer = Buffer.create 32768 in
    Printf.printf "Time: rendering documents...\n%!";
    let (), duration = time (fun () ->
      Array.iter (fun document ->
        ToBuffer.pretty rfrac width buffer document;
        Buffer.clear buffer
      ) docs
    ) () in
    Printf.printf "Time: rendered %d documents of size %d in %.2f seconds.\n%!" runs n duration

end

(* ------------------------------------------------------------------------- *)

(* Main. *)

let () =

  Printf.printf "Testing old engine...\n";
  let state = Random.get_state() in
  let module T = Test1(PPrintEngine) in
  Random.set_state state;
  Printf.printf "Testing new engine...\n";
  let module T = Test1(NewPPrintEngine) in

  Printf.printf "Comparing old and new engines...\n";
  let module T = Test2(PPrintEngine)(NewPPrintEngine) in

  (* The size of the randomly generated documents. *)
  let n = 10000 in
  (* The number of runs. *)
  let runs = 1000 in
  Printf.printf "Generating %d documents of size %d...\n%!" runs n;
  let module D = struct
    let n = n
    let runs = runs
    let docs = Array.init runs (fun _ -> random n)
  end in
  Printf.printf "Timing old engine...\n";
  let module T = Time1(PPrintEngine)(D) in
  Printf.printf "Timing new engine...\n";
  let module T = Time1(NewPPrintEngine)(D) in
  ()

