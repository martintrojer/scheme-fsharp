// mtscheme
// Copyright (c) 2010 Martin Trojer <martin.trojer@gmail.com>

namespace mtscheme

module runner = 

   open System
   open mtscheme.helper
   open mtscheme.interpreter
   open mtscheme.parser

   let rec commandLoop (env, res) = 
      match res with
      | Value(Number(v))  -> printfn "%4.2f\n> " v
      | Value(Name(v))    -> printfn "%s\n> " v
      | Value(Boolean(v)) -> printfn "%b\n> " v
      | List(l)           -> printfn "%s" (listToStr l)
      | _                 -> printfn "null\n> "
      try Console.ReadLine() 
         |> List.ofSeq 
         |> parse 
         |> List.head 
         |> (eval env)
         |> commandLoop
      with ex -> commandLoop (env, Value(Name(ex.Message)))

   [<EntryPoint>]
   let main args =
        
      "(display \"mtscheme v0.1\")" |> parse |> List.head |> (eval globalEnv) |> commandLoop

      0