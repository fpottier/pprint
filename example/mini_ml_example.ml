
type const = Int of int | String of string

type pattern = Any | Var of string | Const of const

type expr =
  | Const of const
  | Var of string
  | Let of string * expr * expr
  | Match of expr * (pattern * expr) list

let small_test =
  Let ("x",
       Match (Var "free", [ Const (String "foo"), Const (Int 0)
                          ; Any, Const (Int 42) ]),
       Var "x"
      )

let test =
  Let ("some_longer_variable_name",
       Match (Var "free", [ Const (String "foo"), Const (Int 0)
                          ; Any, Const (Int 42) ]),
       Var "some_longer_variable_name"
      )

let large_test =
  Let (
    "x",
    Match (
      Var "free",
      [ Const (String "foo bar lol"), Const (String "ok")
      ; Const (String "bar baz test"), Const (String "ko")
      ; Var "otherwise_a_very_long_var_name",
        Var "otherwise_a_very_long_var_name"
      ; Var "otherwise_a_very_very_very_very_very_very_very_long_var_name",
        Var "otherwise_a_very_very_very_very_very_very_very_long_var_name" ]
    ),
    Var "x"
  )

open PPrint

let print_const = function
  | Int i -> string (string_of_int i)
  | String s -> dquote ^^ string s ^^ dquote

let print_var s = string s

let rec print_let binding_name bound_expr body =
  (* TODO(explain): if I don't add this group around everything, lets are
     never printed on one line, even tiny ones... *)
  group (
    group (string "let" ^/^ string binding_name ^/^ string "=") ^^
    (nest 2 (break 1 ^^ group (print_expr bound_expr)))
    ^/^ string "in" ^/^ group (print_expr body)
  )

and print_match arg cases =
  group (
    string "match" ^^
    nest 2 (break 1 ^^ group (print_expr arg)) ^/^
    string "with"
  ) ^^
  ifflat space (hardline ^^ string "| ") ^^
  separate_map (break 1 ^^ string "| ") print_case cases

and print_case (pat, exp) =
  let open PPrint in
  nest 2 
    (prefix 2 1
       (group ((print_pattern pat) ^/^ (string "->")))
       (print_expr exp))

and print_pattern = function
  | Any -> underscore
  | Var s -> print_var s
  | Const c -> print_const c

and print_expr = function
  | Const c -> print_const c
  | Var s -> print_var s
  | Let (name, e1, e2) -> print_let name e1 e2
  | Match (arg, cases) -> print_match arg cases

let () =
  PPrint.ToChannel.pretty 10. 60 stdout (print_expr small_test);
  print_newline ();
  print_newline ();
  PPrint.ToChannel.pretty 10. 60 stdout (print_expr test);
  print_newline ();
  print_newline ();
  PPrint.ToChannel.pretty 10. 60 stdout (print_expr large_test);
  print_newline ();
  flush stdout
