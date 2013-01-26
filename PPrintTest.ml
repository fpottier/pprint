open PPrint

let d : document =
  group (
    group (
      text "coucou" ^^ hardline ^^
	text "my friend"
    ) ^^ break 1 ^^ text "oh"
  )

let () =
  ToChannel.pretty 0.8 80 stdout d;
  flush stdout
