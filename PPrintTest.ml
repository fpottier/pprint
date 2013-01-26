open PPrint

let d : document =
  group (
    group (
      text "coucou" ^^ hardline ^^
	text "my friend"
    ) ^^ break 1 ^^ words "oh my god this is supposed to be an incredibly long text that I will have to break"
  )

let () =
  ToChannel.pretty 0.8 80 stdout d;
  flush stdout
