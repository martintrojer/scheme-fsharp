// mtscheme
// Copyright (c) 2010 Martin Trojer <martin.trojer@gmail.com>

namespace mtscheme

module helper = 

   open interpreter

   // --- Generic helpers ----------------------------
    
   let aritFun op env comb = 
      let aritFun' op env = function
         | []        -> failwith "arithmetric error"
         | e :: t    ->
            match snd(eval env e) with
            | Value(Number(v)) ->
               t |> List.fold(fun acc expr -> 
                  match snd(eval env expr) with 
                  | Value(Number(a))  -> op acc a
                  | _          -> failwith "arithmetric error") v
            | _         -> failwith "arithmetric error"

      (env, Value(Number(aritFun' op env comb)))

   let combFun op env comb =
      let rec combFun' op env comb =
         match comb with
         | []        -> true
         | _ :: []   -> true
         | e1 :: e2 :: t   ->
            match (snd(eval env e1), snd(eval env e2)) with
               | (Value(Number(a)), Value(Number(b)))  -> 
                  (op a b) && (combFun' op env t)
               | _         -> failwith "comparison error"

      (env, Value(Boolean(combFun' op env comb)))

   // build a list of LValues from a Combination of Symbols
   let rec buildList combs =
      match combs with
      | []              -> []
      | Symbol(v) :: t  -> LValue(Name(v)) :: buildList t
      | Value(v) :: t   -> LValue(v) :: buildList t
      | _               -> failwith "invalid define arguments"

   let listToStr listExpr =
      let rec listToStr' listExpr =
         match listExpr with
         | []                       -> ""
         | LValue(Number(n)) :: t   ->
            sprintf "%f; %s" n (listToStr' t)
         | LValue(Boolean(b)) :: t  ->
            sprintf "%b; %s" b (listToStr' t)
         | LValue(Name(n)) :: t     ->
            sprintf "%s; %s" n (listToStr' t)
         | LList(l) :: t            -> 
            sprintf "[%s]; %s" (listToStr' l) (listToStr' t)

      sprintf "[%s]" (listToStr' listExpr)

   // --- Scheme primitive functions -------------------------
    
   let _if env combs =
      match combs with
      | condexpr :: expr1 :: expr2 :: []    ->
         match snd(eval env condexpr) with
         | Value(Boolean(cond))  ->
            if cond then eval env expr1
            else eval env expr2
         | _   -> failwith "invalid if arguments"
      | condexpr :: expr1 :: []    ->
         match snd(eval env condexpr) with
         | Value(Boolean(cond))  ->
            if cond then eval env expr1
            else (env, NullExpr)
         | _   -> failwith "invalid if arguments"
      | _   -> failwith "invalid if arguments"
                
   let _cond env combs =
      let mutable res = false, (env, NullExpr)
      match combs with
      // look strange, but we have to lazilly evaluate the cond combination one after another
      | Combination(comb1) :: Combination(comb2) :: Combination(elsecomb) :: []    ->
         match comb1 with
         | cond :: expr :: []    ->
            match snd(eval env cond) with
            | Value(Boolean(true))  ->
               res <- true, eval env expr
            | _   -> ()
         | _   -> failwith "invalid cond arguments"
         match comb2 with
         | cond :: expr :: []    ->
            match snd(eval env cond) with
            | Value(Boolean(true))  ->
               if not <| fst res then
                  res <- true, eval env expr
            | _   -> ()
         | _   -> failwith "invalid cond arguments"
         match elsecomb with
         | Symbol("else") :: expr :: [] ->
            if not <| fst res then
               res <- true, eval env expr
         | _   -> failwith "invalid cond arguments"
      | _   -> failwith "invalid cond arguments"
      snd res

   let _define env combs = 
      let getString' comb =
         match comb with
         | Symbol(n)         -> n
         | Value(Name(n))    -> n
         | _   -> failwith "invalid define arguments"
      match combs with
      // variable definition (lambda 'values' fall into this category)
      | Symbol(n) :: expr :: []     ->
         let nenv, res = eval env expr                           
         (setEnv n res nenv, NullExpr)
      // function definition
      | Combination(names) :: code ->
         let funcname = getString' names.Head
         let args = buildList names.Tail
         (setEnv funcname (Function(args, code)) env, NullExpr)
      | _   -> failwith "invalid define arguments"

   let _lambda env combs = 
      match combs with
      | Combination(args) :: code   ->
         let args = buildList args
         (env, Function(args, code))
      | _   -> failwith "invalid lambda arguments"

   let _let env combs =
      // set multiple variables
      let rec setEnvs' env exprs =
         match exprs with
         | []                        -> env
         | Combination(comb) :: t    ->
            match comb with
            | Symbol(n) :: expr :: []  ->
               let newenv = setEnv n (snd(eval env expr)) env
               setEnvs' newenv t
            | _   -> failwith "invalid let arguments"
         | _   -> failwith "invalid let arguments"
      let newenv = (expandEnv env)
      match combs with
      | Symbol(n) :: expr :: []  ->
         (setEnv n (snd(eval env expr)) newenv, NullExpr)
      | Combination(values) :: Combination(code) :: []   ->
         let newenv = setEnvs' env values
         eval newenv (Combination(code))
      | _   -> failwith "invalid let arguments"
                
   let _begin env combs =
      evalExprs (expandEnv env) combs
        
   let _list env combs = 
      let rec _list' combs =
         match combs with
         | []        -> []
         | h :: t    ->
            match snd(eval env h) with
            | Value(v)  -> LValue(v) :: _list' t
            | List(l)   -> LList(l) :: _list' t
            | _   -> failwith "invalid list arguments"
      (env, List(_list' combs))

   let _cons env combs =
      match combs with
      | e1 :: e2 :: []    ->
         let res1 = snd(eval env e1)
         let res2 = snd(eval env e2)
         match (snd(eval env e1), snd(eval env e2)) with
         | (Value(v1), Value(v2))    ->
            (env, List([LValue(v1); LValue(v2)]))
         | (Value(v1), List(l2))     ->
            (env, List(LValue(v1)::l2 ))
         | (List(l1), Value(v2))     ->
            (env, List(l1 @ [LValue(v2)] ))
         | (List(l1), List(l2))      ->
            (env, List( l1 @ l2))
         | (Value(v1), NullExpr)     ->
            (env, List([LValue(v1)]))
         | (List(l1), NullExpr)     ->
            (env, List(l1))
         | _   -> failwith "invalid cons arguments"
      | _   -> failwith "invalid cons arguments"

   let _append env combs =
      let rec _append' combs =
         match combs with
         | []        -> []
         | h :: t    ->
            match snd(eval env h) with
            | List(l)   -> l @ _append' t
            | _         -> failwith "invalid append arguments"

      (env, List(_append' combs))
                    
   let _car env combs =
      match combs with
      | e :: []   ->
         match snd(eval env e) with
         | List(l)       ->
            match l with
            | LValue(v) :: _    -> (env, Value(v))
            | LList(l) :: _     -> (env, List(l))
            | _                 -> failwith "invalid car arguments"
         | _   -> failwith "invalid car arguments"
      | _   -> failwith "invalid car arguments"

   let _cdr env combs =
      match combs with
      | e :: []   ->
         match snd(eval env e) with
         | List(l)       ->
            (env, List(l.Tail))
         | _   -> failwith "invalid cdr arguments"
      | _   -> failwith "invalid cdr arguments"

   let _not env combs =
      match combs with
      | expr :: []   ->
         match (eval env expr) with
         | _, Value(Boolean(v))  -> (env, Value(Boolean(not v)))
         | _                     -> failwith "invalid not agrument"
      | _   -> failwith "invalid not agrument"

   let _null env combs =
      match combs with
      | e :: []       ->
         match snd(eval env e) with
         | List([])    -> (env, Value(Boolean(true)))
         | _     -> (env, Value(Boolean(false)))
      | _   -> failwith "invalid null argument"
              
   let _display env combs =
      match combs with
      | expr :: []        ->
         match snd(eval env expr) with
         | Value(Number(v))  -> printfn "%4.2f" v
         | Value(Name(v))    -> printfn "%s" v
         | Value(Boolean(v)) -> printfn "%b" v
         | List(l)           -> printfn "%s" (listToStr l)
         | _   -> failwith "invalid display argument"
      | _   -> failwith "invalid display argument"

      (env, NullExpr)

   let _newline env combs =
      match combs with
      | []  -> printfn("\n")
      | _   -> failwith "invalid newline argument"
      (env, NullExpr)
            
   let globalEnv = 
      [ Map.ofList([  ("+",       Procedure(aritFun (+)));
                     ("-",       Procedure(aritFun (-)));
                     ("*",       Procedure(aritFun (*)));
                     ("/",       Procedure(aritFun (/)));
                     ("=",       Procedure(combFun (=)));
                     (">",       Procedure(combFun (>)));
                     ("<",       Procedure(combFun (<)));
                     (">=",      Procedure(combFun (>=)));
                     ("<=",      Procedure(combFun (<=)));
                     ("if",      Procedure(_if));
                     ("cond",    Procedure(_cond));
                     ("not",     Procedure(_not));
                     ("null?",   Procedure(_null));
                     ("define",  Procedure(_define));
                     ("lambda",  Procedure(_lambda));
                     ("let",     Procedure(_let));
                     ("begin",   Procedure(_begin));
                     ("cons",    Procedure(_cons));
                     ("list",    Procedure(_list));
                     ("append",  Procedure(_append));
                     ("car",     Procedure(_car));
                     ("cdr",     Procedure(_cdr));
                     ("display", Procedure(_display));
                     ("newline", Procedure(_newline));
                     ])]