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

module Generator (E : ENGINE) = struct

  open E

  let rec random (n : int) : document =
    (* If the budget is 0, produce an empty document. *)
    if n = 0 then
      empty
    (* If the budget is 1, produce a leaf. *)
    else if n = 1 then
      choose [
        char 'c';
        string "hello";
        substring "the cat" 4 3;
        utf8string "étoile";
        hardline;
        blank 2;
        break 2
      ]
    (* Otherwise, decrement the budget, and produce a node of nonzero
       arity, spending the rest of the budget on the children. *)
    else
      let n = n - 1 in
      Lazy.force (pick [
        10, lazy (let n1, n2 = split n in random n1 ^^ random n2);
         2, lazy (nest 2 (random n));
        10, lazy (group (random n));
         2, lazy (let n1, n2 = split n in ifflat (random n1) (random n2))
      ])

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
    1000

  let () =
    let module R = Generator(E) in
    for r = 1 to runs do
      let document = R.random n in
      let buffer = Buffer.create 32768 in
      ToBuffer.pretty rfrac width buffer document;
      let buffer = Buffer.create 32768 in
      ToBuffer.compact buffer document
    done;
    Printf.printf "Test 1: success.\n%!"

end

(* ------------------------------------------------------------------------- *)

(* Testing two engines and comparing their output. *)

module Test2 (E1 : ENGINE) (E2 : ENGINE) = struct

  (* The size of the randomly generated documents. *)
  let n =
    1000

  (* The number of runs. *)
  let runs =
    1000

  let () =
    let module R1 = Generator(E1) in
    let module R2 = Generator(E2) in
    for r = 1 to runs do
      (* The two engines have distinct abstract types of documents.
         This creates a difficulty: we must produce the same random
         document in two different forms. We achieve this by saving
         and setting the state of the random generator. *)
      let state = Random.get_state() in
      let document1 = R1.random n in
      Random.set_state state;
      let document2 = R2.random n in
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

module Time1 (E : ENGINE) = struct

  open E

  (* The size of the randomly generated documents. *)
  let n =
    10000

  (* The number of runs. *)
  let runs =
    10000

  let () =
    let module R = Generator(E) in
    Printf.printf "Time 1: generating %d documents of size %d...\n%!" runs n;
    let docs = Array.init runs (fun _ -> R.random n) in
    let buffer = Buffer.create 32768 in
    let start = Unix.gettimeofday() in
    Array.iter (fun document ->
      ToBuffer.pretty rfrac width buffer document;
      Buffer.clear buffer
    ) docs;
    let finish = Unix.gettimeofday() in
    Printf.printf "Time 1: rendered %d documents of size %d in %.2f seconds.\n%!" runs n (finish -. start)

end

(* ------------------------------------------------------------------------- *)

(* Main. *)

let () =
  let module T = Test1(NewPPrintEngine) in
  let module T = Test2(PPrintEngine)(NewPPrintEngine) in
  let module T = Time1(PPrintEngine) in
  let module T = Time1(NewPPrintEngine) in
  ()

