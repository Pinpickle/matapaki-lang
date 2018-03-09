let () =
  Core.Std.Command.basic ~summary:"Parse and display JSON"
    Core.Std.Command.Spec.(empty 
      +> flag "-v" no_arg ~doc:"Verbose"
      +> anon ("filename" %: file))
    Load_parse.loop
  |> Core.Std.Command.run
