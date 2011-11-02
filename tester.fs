// mtscheme
// Copyright (c) 2010 Martin Trojer <martin.trojer@gmail.com>

namespace mtscheme

module tester =

   open mtscheme.helper
   open mtscheme.parser
   open mtscheme.interpreter
   open NUnit.Framework
//    open ZeroUnit.NUnit        // http://zerounit.codeplex.com/

   // --- Helpers ----------------------------
                           
   let getDoubleResult env expr =
      match eval env expr with
      | env, res -> 
         match res with
         | Value(Number(n))  -> n
         | _             -> failwith "expression failure"

   let getBoolResult env expr =
      match eval env expr with
      | env, res -> 
         match res with
         | Value(Boolean(b)) -> b
         | _                 -> failwith "expression failure"

   let testListResult correct (env, expr)  =
      let rec testLists' a b =
         let test' acc a b =
            match (a, b) with
            | (LValue(v1), LValue(v2))  ->
               acc && (v1 = v2)
            | (LList(l1), LList(l2))    ->
               acc && (testLists' l1 l2)
            | _   -> false
         try
            List.fold2 test' true a b
         with ex -> false                // different lengths == fail

      match snd(eval env expr) with
         | List(res) ->
            testLists' res correct
         | _   -> false

   let testDouble env exprStr correct =
      let res = exprStr |> parse |> List.head |> (getDoubleResult env)
      Assert.AreEqual(correct, res)

   let testBool env exprStr correct =
      let res = exprStr |> parse |> List.head |> (getBoolResult env)
      Assert.AreEqual(correct, res)

   let testList env exprStr correct =
      exprStr |> parse |> List.head |> (eval env) |> testListResult correct |> Assert.IsTrue

   // --- Tests ----------------------------

   [<TestFixture>]
   type TestClass() =

      let testEnv = expandEnv globalEnv |> setEnv "kalle" (Value(Number(1.0))) |> setEnv "olle" (Value(Number(2.0)))

      [<Test>]
      member tc.testAdd() =
         testDouble testEnv "(+ 1 2)" (1+2)
         testDouble testEnv "(+ 1 (+ 2 3))" (1+2+3)
         testDouble testEnv "(+ 1)" (1)
         testDouble testEnv "(+ 1 1 1)" (1+1+1)
        
      [<Test>]
      member tc.testSub() =
         testDouble testEnv "(- 1 2)" (1-2)
         testDouble testEnv "(- 1 (- 2 3))" (1-(2-3))
         testDouble testEnv "(- 1)" (-1)              // TODO; special case not handled correctly
         testDouble testEnv "(- 1 1 1)" (1-1-1)

      [<Test>]
      member tc.testMul() =
         testDouble testEnv "(* 2 3.14)" (2.0*3.14)
         testDouble testEnv "(+ 1 (* 2 3))" (1+2*3)
         testDouble testEnv "(* 1)" (1)
         testDouble testEnv "(* 2 1 2 2)" (2*1*2*2)

      [<Test>]
      member tc.testDiv() =
         testDouble testEnv "(/ 9 3)" (9/3)
         testDouble testEnv "(+ 1 (/ 2 3))" (1.0+2.0/3.0)
         testDouble testEnv "(/ 1)" (1)
         testDouble testEnv "(/ 2)" (1.0/2.0)            // TODO; special case not handled correctly
         testDouble testEnv "(/ 1 2 3)" (1.0/2.0/3.0)

      [<Test>]
      member tc.testVariable() =
         testDouble testEnv "(kalle)" (1)
         testDouble testEnv "(+ 1 (+ 1 olle))" (1+1+2)

      [<Test>]
      member tc.testEq() =
         testBool testEnv "(= 2 2)" (2=2)
         testBool testEnv "(= 2 (+ 1 1))" (2=(1+1))
         testBool testEnv "(= 1)" (true)
         testBool testEnv "(= 1 1 (+ 1 1) 1)" (false)

      [<Test>]
      member tc.testGt() =
         testBool testEnv "(> 2 2)" (2>2)
         testBool testEnv "(> 1 2)" (1>2)
         testBool testEnv "(> 2 1)" (2>1)
         testBool testEnv "(> (+ 1 1 1) 2)" ((1+1+1)>2)
         testBool testEnv "(> 1)" (true)                 // TODO; should raise failure
         testBool testEnv "(> 1 1 (+ 1 1) 1)" (false)

      [<Test>]
      member tc.testLt() =
         testBool testEnv "(< 2 2)" (2<2)
         testBool testEnv "(< 1 2)" (1<2)
         testBool testEnv "(< 2 1)" (2<1)
         testBool testEnv "(< (+ 1 1 1) 2)" ((1+1+1)<2)
         testBool testEnv "(< 1)" (true)                 // TODO; should raise failure
         testBool testEnv "(< 1 1 (+ 1 1) 1)" (false)

      [<Test>]
      member tc.testGe() =
         testBool testEnv "(>= 2 2)" (2>=2)
         testBool testEnv "(>= 1 2)" (1>=2)
         testBool testEnv "(>= 2 1)" (2>=1)
         testBool testEnv "(>= (+ 1 1 1) 2)" ((1+1+1)>=2)
         testBool testEnv "(>= 1)" (true)                 // TODO; should raise failure
         testBool testEnv "(>= 1 1 (+ 1 1) 1)" (false)    // TODO; returning true

      [<Test>]
      member tc.testLe() =
         testBool testEnv "(<= 2 2)" (2<=2)
         testBool testEnv "(<= 1 2)" (1<=2)
         testBool testEnv "(<= 2 1)" (2<=1)
         testBool testEnv "(<= (+ 1 1 1) 2)" ((1+1+1)<=2)
         testBool testEnv "(<= 1)" (true)                 // TODO; should raise failure
         testBool testEnv "(<= 1 1 (+ 1 1) 1)" (false)

      [<Test>]
      member tc.testNot() =
         testBool testEnv "(not (= 1 1))" (false)
         testBool testEnv "(not (not (= 1 1)))" (true)

      [<Test>]
      member tc.testDefine() =
         let env, res = "(define lisa 4)" |> parse |> List.head |> (eval testEnv)
         testDouble env "(lisa)" 4

         let env, res = "(define nisse (+ 1 1 1))" |> parse |> List.head |> (eval testEnv)
         testDouble env "(nisse)" 3

      [<Test>]
      member tc.testIf() =
         testDouble testEnv "(if (< 2 1) 10 11)" 11
         testDouble testEnv "(if (< (+ 1 1 1) 1) 11 (* 2 5))" 10

      [<Test>]
      member tc.testCond() =
         let comb = "(cond ((< x 0) (- 0 x)) ((= x 0) (100)) (else x))"

         let env, _ = "(define x -1)" |> parse |> List.head |> (eval testEnv)
         testDouble env comb 1

         let env, _ = "(define x 0)" |> parse |> List.head |> (eval testEnv)
         testDouble env comb 100

         let env, _ = "(define x 1)" |> parse |> List.head |> (eval testEnv)
         testDouble env comb 1

      [<Test>]
      member tc.testCons() =
         testList testEnv "(cons 1 2)" [LValue(Number(1.0)); LValue(Number(2.0))]
         testList testEnv "(cons 1 (cons 2 (cons 3 4)))" [LValue(Number(1.0)); LValue(Number(2.0)); 
                                                            LValue(Number(3.0)); LValue(Number(4.0))]
         testList testEnv "(cons (cons 1 2) (cons 3 4))" [LValue(Number(1.0)); LValue(Number(2.0)); 
                                                            LValue(Number(3.0)); LValue(Number(4.0))]
         testList testEnv "(cons (- 2 1) (cons 2 (+ 1 1 1)))" [LValue(Number(1.0)); LValue(Number(2.0)); LValue(Number(3.0))]
         testList testEnv "(cons \"kalle\" 2)" [LValue(Name("kalle")); LValue(Number(2.0))]

      [<Test>]
      member tc.testList() =
         testList testEnv "(list 1 2)" [LValue(Number(1.0)); LValue(Number(2.0))]
         testList testEnv "(list 5 (list 1 1) 2)" [LValue(Number(5.0)); LList([LValue(Number(1.0)); LValue(Number(1.0))]); LValue(Number(2.0))]
         testList testEnv "(list 1 \"kalle\")" [LValue(Number(1.0)); LValue(Name("kalle"))]

      [<Test>]
      member tc.testAppend() =
         testList testEnv "(append (list 1) (list 2)" [LValue(Number(1.0)); LValue(Number(2.0))]
         testList testEnv "(append (list 1 2) (list 3 4)" [LValue(Number(1.0)); LValue(Number(2.0)); 
                                                            LValue(Number(3.0)); LValue(Number(4.0))]
         testList testEnv "(append (list 1) (list 2 (list 3))" [LValue(Number(1.0)); LValue(Number(2.0)); 
                                                                  LList[LValue(Number(3.0))]]

      [<Test>]
      member tc.testCar() =
         testDouble testEnv "(car (list 1 2))" 1
         testList testEnv "(car (list (list 1) 2))" [LValue(Number(1.0))]

      [<Test>]
      member tc.testCdr() =
         testList testEnv "(cdr (list 1 2))" [LValue(Number(2.0))]
         testList testEnv "(cdr (list 1 2 3))" [LValue(Number(2.0)); LValue(Number(3.0))]
         testList testEnv "(cdr (list 1))" []

      [<Test>]
      member tc.testNull() =
         testBool testEnv "(null? (list 1))" false
         testBool testEnv "(null? (cdr (list 1)))" true

         let env, _ = "(define l (list 1 2))" |> parse |> List.head |> (eval testEnv)
         testBool env "(null? l)" false
         testBool env "(null? (cdr l))" false
         testBool env "(null? (cdr (cdr l)))" true

      [<Test>]
      member tc.testFunction() =
         let env, _ = "(define (hello) (display \"hello world\"))" |> parse |> List.head |> (eval testEnv)
         let _ = "(hello)" |> parse |> List.head |> (eval env)

         let env, _ = "(define (factorial x) (if (= x 0) 1 (* x (factorial (- x 1)))))" 
                        |> parse |> List.head |> (eval testEnv)
         testDouble env "(factorial (+ 5 5))" 3628800

         let env, _ = "(define (add x y) (define (worker x y) (+ x y)) (worker x y))"
                        |> parse |> List.head |> (eval testEnv)
         testDouble env "(add 1 3)" 4                                // TODO; there are atleast one stack frame too many here

      [<Test>]
      member tc.testLet() =
         let env, _ = "(let lisa 1)" |> parse |> List.head |> (eval testEnv)
         testDouble env "(lisa)" 1

         testDouble env "(let ((a (+ 1 1))(b 3)) (+ a b))" 5 

      [<Test>]
      member tc.testBegin() =
         let env, _ = "(define (foreach f l) (if (not (null? l)) (begin (f (car l)) (foreach f (cdr l)))))"
                        |> parse |> List.head |> (eval testEnv)
         let env, _ = "(define l (list 1 2 3))" |> parse |> List.head |> (eval env)
         let _ = "(foreach display l)" |> parse |> List.head |> (eval env)

         Assert.Pass()

      [<Test>]
      member tc.testLambda() =
         let env, _ = "(define (adder val) (lambda (x) (+ x val)))"
                        |> parse |> List.head |> (eval testEnv)
         let env, _ = "(define add4 (adder 4))" |> parse |> List.head |> (eval env)
         testDouble env "(add4 4)" 8

         let env, _ = "(define (map f l) (if (not (null? l)) (cons (f (car l)) (map f (cdr l)))))"
                        |> parse |> List.head |> (eval testEnv)
         let env, _ = "(define l (list 1 2 3))" |> parse |> List.head |> (eval env)
         testList env "(map (lambda (x) (* x x)) l)" [LValue(Number(1.0)); LValue(Number(4.0)); LValue(Number(9.0))]

      [<Test>]
      member tc.testDisplay() =
         let _ = "(display \"hello world\")" |> parse |> List.head |> (eval testEnv)
         let _ = "(display (+ 1 2))" |> parse |> List.head |> (eval testEnv)

         Assert.Pass()

      [<Test>]
      member tc.testNewline() =
         let _ = "(newline)" |> parse |> List.head |> (eval testEnv)

         Assert.Pass()
