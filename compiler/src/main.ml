open Core.Std

let () =
  Command.basic ~summary:"Parse and display JSON"
    Command.Spec.(empty 
      +> flag "-v" no_arg ~doc:"Verbose"
      +> anon ("filename" %: file))
    Load_parse.loop
  |> Command.run
