open Core.Std

let () =
  Command.basic ~summary:"Parse and display JSON"
    Command.Spec.(empty +> anon ("filename" %: file))
    Load_parse.loop
  |> Command.run
