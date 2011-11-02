
// mtscheme
// Copyright (c) 2010 Martin Trojer <martin.trojer@gmail.com>

namespace mtscheme

module interpreter =
   open System
   
   // --- mtscheme types --------------------------------

   type ValueType =
      | Number of Double
      | Boolean of bool
      | Name of String

   type ListType =
      | LValue of ValueType
      | LList of ListType list

   type Expression =
      | NullExpr
      | Combination of Expression list
      | List of ListType list
      | Function of ListType list * Expression list
      | Procedure of (Map<string,Expression> list -> Expression list -> Map<string,Expression> list * Expression)
      | Symbol of string
      | Value of ValueType

   // --- environment helpers --------------------------------

   let setEnv str expr (env:Map<string,Expression> list) =
      match env with
      | head::tail    -> List.append [head.Add(str, expr)] tail
      | []            -> failwith "empty environment"
            
   let expandEnv (env:Map<string,Expression> list) =
      List.append [Map.empty<string,Expression>] env

   let rec lookup (env:Map<string,Expression> list) s =
      match env with
      | []        -> None
      | h::t      -> 
         match (h.TryFind s) with
         | Some(e)       -> Some(e)
         | None          -> lookup t s

   // --- eval / apply --------------------------------

   let rec eval (env:Map<string,Expression> list) expr =
      match expr with
      | NullExpr              ->  failwith "invalid interpreter state"
      | Combination([])       ->  failwith "invalid combination"
      | Combination(h::t)     -> 
         match eval env h with
         | (nenv, Procedure(f))          -> apply f t env
         | (nenv, Function(args, code))  -> 
            let newenv =
               try List.fold2 bindArg (expandEnv nenv) args t
               with ex -> failwith "invalid number of arguments"
            evalExprs newenv code
         | (nenv, expr)                  -> (nenv, expr)
      | Procedure(f)          -> (env, Procedure(f))
      | Function(args, code)  -> failwith "invalid function call"
      | Value(v)              -> (env, Value(v))
      | List(v)               -> (env, List(v))
      | Symbol(s)             ->
         match lookup env s with
         | Some(e)   -> (env, e)
         | None      -> failwith (sprintf "unbound symbol '%A'" s)

   and apply f args env = f env args

   // --- expression helpers --------------------------------

   // bind arguments in new env
   and bindArg env arg expr = 
      match arg with
      | LValue(Name(n))   ->
            setEnv n (snd(eval env expr)) env
      | _         -> failwith "invalid argument"                        

   // Eval a list of expressions, return the value of the last one
   and evalExprs env combs = 
      match combs with
      | []        -> (env, NullExpr)
      | h :: t    ->
         let nenv, res = eval env h
         match t.Length with
         | 0     -> (nenv, res)
         | 1     -> eval nenv t.Head
         | _     -> evalExprs nenv t
