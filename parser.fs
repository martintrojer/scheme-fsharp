// mtscheme
// Copyright (c) 2010 Martin Trojer <martin.trojer@gmail.com>
//
// inspired by
// http://blogs.msdn.com/b/ashleyf/archive/2010/09/24/fscheme-0-0-0.aspx

namespace mtscheme

module parser =
    
   open System
   open mtscheme.interpreter

   type Token = 
      | TOpen | TClose 
      | TNumber of string 
      | TString of string 
      | TSymbol of string

   let tokenize source = 
      let rec string acc = function 
         | '\\' :: '"' :: t -> string (acc + "\"") t     // escaped quote becomes quote 
         | '"' :: t -> acc, t                            // closing quote terminates 
         | c :: t -> string (acc + (c.ToString())) t     // otherwise accumulate chars 
         | _ -> failwith "malformed string" 
      let rec token acc = function 
         | (')' :: _) as t -> acc, t                     // closing paren terminates 
         | w :: t when Char.IsWhiteSpace(w) -> acc, t    // whitespace terminates 
         | [] -> acc, []                                 // end of list terminates 
         | c :: t -> token (acc + (c.ToString())) t      // otherwise accumulate chars 
      let rec tokenize' acc = function 
         | w :: t when Char.IsWhiteSpace(w) -> tokenize' acc t // skip whitespace 
         | '(' :: t -> tokenize' (TOpen :: acc) t 
         | ')' :: t -> tokenize' (TClose :: acc) t 
         | '"' :: t ->                                   // start of string 
            let s, t' = string "" t 
            tokenize' (TString(s) :: acc) t' 
         | '-' :: d :: t when Char.IsDigit(d) ->         // start of negative number 
            let n, t' = token ("-" + d.ToString()) t 
            tokenize' (TNumber(n) :: acc) t' 
         | '+' :: d :: t | d :: t when Char.IsDigit(d) -> // start of positive number 
            let n, t' = token (d.ToString()) t 
            tokenize' (TNumber(n) :: acc) t' 
         | s :: t ->                                     // otherwise start of symbol 
            let s, t' = token (s.ToString()) t 
            tokenize' (TSymbol(s) :: acc) t' 
         | [] -> List.rev acc                            // end of list terminates 
      tokenize' [] source

   let parse source = 
      let map = function 
         | TNumber(n) -> Value(Number(Double.Parse(n)))
         | TString(s) -> Value(Name(s))
         | TSymbol(s) -> Symbol(s) 
         | _ -> failwith "syntax error" 

      let rec parse' acc = function 
         | TOpen :: t ->
               let e, t' = parse' [] t 
               parse' (Combination(e) :: acc) t' 
         | TClose :: t -> (List.rev acc), t 
         | h :: t -> parse' ((map h) :: acc) t 
         | [] -> (List.rev acc), [] 

      let result, _ = parse' [] (tokenize (List.ofSeq source))
      result
