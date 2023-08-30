open Core_bench
let memoize = Fix.Memoize.Int.memoize

(* -------------------------------------------------------------------------- *)

(* Random generation of abstract syntax trees. *)

module Generate = struct

  open AST

  let uneg e =
    EUnOp (UNeg, e)

  let ebinop op (e1, e2) =
    EBinOp (e1, op, e2)

  let pay s =
    assert (s > 0);
    s - 1

  let split s =
    assert (s >= 0);
    let s1 = Random.int (s + 1) in
    let s2 = s - s1 in
    s1, s2

  let rec expr (s : int) : expr =
    if s = 0 then
      EConst 0
    else
      let s = pay s in
      let i = Random.int 5 in
      if i = 4 then
        EUnOp (UNeg, expr s)
      else
        let s1, s2 = split s in
        let op = List.nth [BAdd; BSub; BMul; BDiv] i in
        EBinOp (expr s1, op, expr s2)

  let main (s : int) : main =
    expr s

end

(* -------------------------------------------------------------------------- *)

(* Each benchmark is run at the following tree sizes. *)

let args =
  [10; 100; 1_000; 10_000; 100_000; 1_000_000; 3_000_000]

(* -------------------------------------------------------------------------- *)

(* Generating ASTs. *)

let generation =
  let name = "Generating AST" in
  Bench.Test.create_indexed ~name ~args @@ fun s ->
  Core.Staged.stage (fun () -> ignore (Generate.main s))

(* After [Generate.main] has been benchmarked, a memoized version of
   it can be used, so we spend less time preparing data for the next
   benchmarks. *)

let make_ast =
  memoize Generate.main

(* -------------------------------------------------------------------------- *)

(* Converting ASTs to PPrint documents. *)

let conversion =
  let name = "Constructing document" in
  Bench.Test.create_indexed ~name ~args @@ fun s ->
  let ast = make_ast s in
  Core.Staged.stage (fun () -> ignore (AST2Document.main ast))

let make_doc =
  memoize @@ fun s ->
  make_ast s
  |> AST2Document.main

(* -------------------------------------------------------------------------- *)

(* Rendering PPrint documents (in memory). *)

let format document : string =
  let b = Buffer.create 1024 in
  PPrint.ToBuffer.pretty 0.8 80 b document;
  Buffer.contents b

let formatting =
  let name = "Rendering document" in
  Bench.Test.create_indexed ~name ~args @@ fun s ->
  let document = make_doc s in
  Core.Staged.stage (fun () -> ignore (format document))

(* -------------------------------------------------------------------------- *)

(* Running the benchmarks. *)

let run_all_benchmarks () =
  Command_unix.run (Bench.make_command [
    generation;
    conversion;
    formatting;
  ])

(* -------------------------------------------------------------------------- *)

(* Main. *)

let () =
  run_all_benchmarks()
