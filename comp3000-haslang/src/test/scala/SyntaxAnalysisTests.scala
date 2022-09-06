/**
 * HasLang syntax analysis tests.
 *
 * Copyright 2021, Anthony Sloane, Matthew Roberts, Kym Haines, Macquarie University, All rights reserved.
 */

package haslang

import org.bitbucket.inkytonik.kiama.util.ParseTests
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Tests that check that the syntax analyser works correctly.  I.e., it accepts
 * correct input and produces the appropriate trees, and it rejects illegal input.
 */
@RunWith(classOf[JUnitRunner])
class SyntaxAnalysisTests extends ParseTests {

    import HasLangTree._

    val parsers = new SyntaxAnalysis (positions)
    import parsers._

    // Tests of parsing basic expressions

    test ("equal expression") {
        exp ("a == 1") should parseTo[HasLangNode] (EqualExp (IdnUse ("a"), IntExp (1)))
    }

    test ("less than expression") {
        exp ("a < 1") should parseTo[HasLangNode] (LessExp (IdnUse ("a"), IntExp (1)))
    }

    test ("addition expression") {
        exp ("a + 1") should parseTo[HasLangNode] (PlusExp (IdnUse ("a"), IntExp (1)))
    }

    test ("subtraction expression") {
        exp ("a - 1") should parseTo[HasLangNode] (MinusExp (IdnUse ("a"), IntExp (1)))
    }

    test ("multiplication expression") {
        exp ("a * 1") should parseTo[HasLangNode] (StarExp (IdnUse ("a"), IntExp (1)))
    }

    test ("division expression") {
        exp ("a / 1") should parseTo[HasLangNode] (SlashExp (IdnUse ("a"), IntExp (1)))
    }

    test ("integer expression") {
        exp ("823") should parseTo[HasLangNode] (IntExp (823))
    }

    test ("true expression") {
        exp ("true") should parseTo[HasLangNode] (BoolExp (true))
    }

    test ("false expression") {
        exp ("false") should parseTo[HasLangNode] (BoolExp (false))
    }

    test ("identifier expression") {
        exp ("v123") should parseTo[HasLangNode] (IdnUse ("v123"))
    }

    test ("parenthesized expression") {
        exp ("(a + 5)") should parseTo[HasLangNode] (PlusExp (IdnUse ("a"), IntExp (5)))
    }

    test ("application expression 1") {
        exp ("a b") should parseTo[HasLangNode] (AppExp (IdnUse ("a"), IdnUse ("b")))
    }

    test ("expression containing an application expression") {
        exp ("1 + foo 2") should parseTo[HasLangNode] (PlusExp(IntExp(1), AppExp (IdnUse ("foo"), IntExp (2))))
    }

    test ("if expression") {
        exp ("if (true) then 3 else 4") should parseTo[HasLangNode] (IfExp (BoolExp (true), IntExp (3), IntExp (4)))
    }

    test ("lambda expression") {
        exp ("\\a :: Int -> a + 1") should parseTo[Exp] (LamExp(
                            IdnDef("a", IntType()),
                            PlusExp(IdnUse("a"), IntExp(1))))
    }

    test ("basic type") {
        tipe ("Bool") should parseTo[Type] (BoolType())
    }

    test ("parsing unit type") {
        tipe ("()") should parseTo[Type] (UnitType())
    }

    test ("parsing list type") {
        tipe ("[Int]") should parseTo[Type] (ListType(IntType()))
    }

    test ("parsing tuple type") {
        tipe ("(Int,Bool,[Bool])") should parseTo[Type] (TupleType(Vector(IntType(), BoolType(), ListType(BoolType()))))
    }

    test ("parsing function type") {
      tipe ("Int->Bool->[Int]") should parseTo[Type] (FunType(IntType(), FunType(BoolType(), ListType(IntType()))))
    }

    test ("parsing bracketted function type") {
      tipe ("(Int->Bool)->[Int]") should parseTo[Type] (FunType(FunType(IntType(), BoolType()), ListType(IntType())))
    }

    test ("empty list") {
        exp ("[]") should parseTo[HasLangNode] (ListExp (Vector()))
    }

    test ("cons expression") {
        exp ("3 : []") should parseTo[HasLangNode] (ConsExp (IntExp (3), ListExp (Vector())))
    }

    test ("list expression") {
        exp ("[3, 4, 5]") should parseTo[HasLangNode] (ListExp (Vector(IntExp(3), IntExp(4), IntExp(5))))
    }

    test ("tuple expression") {
        exp ("(3, 4, 5)") should parseTo[HasLangNode] (TupleExp (Vector(IntExp(3), IntExp(4), IntExp(5))))
    }

    test ("underscore pattern") {
        pat ("_") should parseTo[Pat] (AnyPat())
    }

    test ("literal pattern") {
        pat ("3") should parseTo[Pat] (LiteralPat(IntExp(3)))
    }

    test ("list pattern") {
        pat ("[3, _, 5]") should parseTo[Pat] (ListPat(Vector(LiteralPat(IntExp(3)), AnyPat(), LiteralPat(IntExp(5)))))
    }

    test ("cons pattern") {
        pat ("3 : []") should parseTo[Pat] (ConsPat(LiteralPat(IntExp(3)), ListPat(Vector())))
    }

    test ("tuple pattern") {
        pat ("(3, _, 5)") should parseTo[Pat] (TuplePat(Vector(LiteralPat(IntExp(3)), AnyPat(), LiteralPat(IntExp(5)))))
    }

    test ("simple function line") {
        funline ("fac 0 = 1") should parseTo[FunLine] (FunLine("fac", Vector(LiteralPat(IntExp(0))), IntExp(1)))
    }

    test ("more complicated function line") {
        funline ("length h:t = 1 + length t") should parseTo[FunLine] (FunLine("length", Vector(ConsPat(IdentPat("h"), IdentPat("t"))), PlusExp(IntExp(1), AppExp(IdnUse("length"), IdnUse("t")))))
    }

    test ("simple variable") {
        defn ("x :: Int = 100") should parseTo[Defn] (Defn(IdnDef("x", IntType()), Vector(FunLine("", Vector(), IntExp(100)))))
    }

    test ("function with two lines") {
      defn ("""inc :: Int -> Int
               inc n = n + 1
            """) should parseTo[Defn] (Defn(
                  IdnDef("inc", FunType(IntType(), IntType())),
                  Vector(FunLine("inc", Vector(IdentPat("n")),
                                 PlusExp(IdnUse("n"), IntExp(1))))))
    }

    test ("function with three lines") {
      defn ("""fac :: Int -> Int
               fac 0 = 1.
               fac n = n * fac (n - 1)
            """) should parseTo[Defn] (Defn(
                  IdnDef("fac", FunType(IntType(), IntType())),
                  Vector(FunLine("fac", Vector(LiteralPat(IntExp(0))),
                                 IntExp(1)),
                         FunLine("fac", Vector(IdentPat("n")),
                                 StarExp(IdnUse("n"),
                                         AppExp(IdnUse("fac"),
                                                MinusExp(IdnUse("n"),
                                                         IntExp(1))))))))
    }

    test ("one definition") {
      definitions ("""x   :: Int        = 100
                """) should parseTo[Vector[Defn]] (Vector(Defn(
                            IdnDef("x", IntType()),
                            Vector(FunLine("", Vector(), IntExp(100))))))
    }

    test ("one definition with lambda") {
      definitions ("""inc :: Int -> Int = \a :: Int -> a + 1
                """) should parseTo[Vector[Defn]] (Vector(Defn(
                            IdnDef("inc", FunType(IntType(), IntType())),
                            Vector(FunLine("", Vector(), LamExp(
                                              IdnDef("a", IntType()),
                                              PlusExp(IdnUse("a"), IntExp(1)))))
                                        )))
    }

    test ("two definitions") {
      definitions ("""x   :: Int        = 100;
                      y   :: Bool       = false
                """) should parseTo[Vector[Defn]] (Vector(
                       Defn(IdnDef("x", IntType()),
                            Vector(FunLine("", Vector(), IntExp(100)))),
                       Defn(IdnDef("y", BoolType()),
                            Vector(FunLine("", Vector(), BoolExp(false))))))
    }

    test ("let with one definition") {
      program ("""let
                    x   :: Int        = 100
                  in
                    inc x
                """) should parseTo[Program] (Program(LetExp(
                    Vector(Defn(
                            IdnDef("x", IntType()),
                            Vector(FunLine("", Vector(), IntExp(100))))),
                    AppExp (IdnUse ("inc"), IdnUse ("x")))))
    }

    test ("let with two definitions") {
      program ("""let
                    x   :: Int        = 100;
                    y   :: Bool       = false
                  in
                    inc x
                """) should parseTo[Program] (Program(LetExp(
                    Vector(Defn(
                             IdnDef("x", IntType()),
                             Vector(FunLine("", Vector(), IntExp(100)))),
                           Defn(
                             IdnDef("y", BoolType()),
                             Vector(FunLine("", Vector(), BoolExp(false))))),
                    AppExp (IdnUse ("inc"), IdnUse ("x")))))
    }

    test ("program with two definitions including lambda") {
      program ("""let
                    x   :: Int        = 100;
                    inc :: Int -> Int = \a :: Int -> a + 1
                  in
                    inc x
                """) should parseTo[Program] (Program(LetExp(
                    Vector(Defn(
                            IdnDef("x", IntType()),
                            Vector(FunLine("", Vector(), IntExp(100)))),
                           Defn(
                            IdnDef("inc", FunType(IntType(), IntType())),
                            Vector(FunLine("", Vector(), LamExp(
                                              IdnDef("a", IntType()),
                                              PlusExp(IdnUse("a"), IntExp(1)))))
                                        )),
                    AppExp (IdnUse ("inc"), IdnUse ("x")))))
    }

    test ("program with definitions including lambda and multiline fun") {
      program ("""let
                    x   :: Int        = 100;
                    inc :: Int -> Int = \a :: Int -> a + 1;
                    length :: [Int] -> Int
                    length [] = 0.
                    length h:t = 1 + length t
                  in
                    inc x
                """) should parseTo[Program] (Program(LetExp(
                    Vector(Defn(
                            IdnDef("x", IntType()),
                            Vector(FunLine("", Vector(), IntExp(100)))),
                           Defn(
                            IdnDef("inc", FunType(IntType(), IntType())),
                            Vector(FunLine("", Vector(), LamExp(
                                            IdnDef("a", IntType()),
                                            PlusExp(IdnUse("a"), IntExp(1)))))),
                           Defn(
                            IdnDef("length", FunType(ListType(IntType()),
                                                     IntType())),
                            Vector(FunLine("length", Vector(ListPat(Vector())),
                                           IntExp(0)),
                                   FunLine("length",
                                           Vector(ConsPat(IdentPat("h"),
                                                          IdentPat("t"))),
                                           PlusExp(IntExp(1),
                                                   AppExp(IdnUse("length"),
                                                          IdnUse("t")))))
                                        )),
                     AppExp (IdnUse ("inc"), IdnUse ("x")))))
    }

    // FIXME: more tests here...

    //Precedence, Associativity and Exp Tests
    test ("Precedence and Associativity Test #1"){
        exp("2 * 3 + 4") should parseTo[HasLangNode] (PlusExp(StarExp(IntExp(2), IntExp(3)), IntExp(4)))
    }

    test ("Precedence and Associativity Test #2"){
        exp("1 + 2 * 3 + 4") should parseTo[HasLangNode] (PlusExp(PlusExp(IntExp (1),StarExp(IntExp (2),IntExp(3))),IntExp(4)))
    }

    test ("Precedence and Associativity Test #3 - should ignore 3"){
        exp("1 == 2 == 3") should parseTo[HasLangNode] (EqualExp(IntExp(1), IntExp(2)))
    }

    test ("Precedence and Associativity Test #4"){
        exp("1 + 1 == 2 == 3") should parseTo[HasLangNode] (EqualExp(PlusExp(IntExp(1), IntExp(1)), IntExp(2)))
    }

    test ("Precedence and Associativity Test #5"){
        exp("2 + 3 * 4") should parseTo[HasLangNode] (PlusExp(IntExp(2), StarExp(IntExp(3), IntExp(4))))
    }

    test ("Precedence and Associtivity Test #6 - / right Associativity"){
        exp("2 / 4 / 8 / 16 / 32") should parseTo[HasLangNode] (SlashExp(IntExp (2),SlashExp(IntExp(4),SlashExp(IntExp(8),SlashExp(IntExp(16),IntExp(32))))))
    }

    test ("Precedence and Associtivity Test #7 - - right Associativity"){
        exp("2 - 4 - 8 - 16 - 32") should parseTo[HasLangNode] (MinusExp(IntExp(2),MinusExp(IntExp (4),MinusExp (IntExp (8), MinusExp (IntExp (16), IntExp (32))))))
    }   

    test ("Precedence and Associtivity Test #8 - * Left Associativity"){
        exp("2 * 4 * 8 * 16 * 32") should parseTo[HasLangNode] (StarExp(StarExp(StarExp(StarExp(IntExp(2),IntExp(4)),IntExp(8)),IntExp(16)),IntExp(32)))
    }

    test ("Precedence and Associtivity Test #9 - + Left Associativity"){
        exp("2 + 4 + 8 + 16 + 32") should parseTo[HasLangNode] (PlusExp(PlusExp(PlusExp(PlusExp(IntExp(2),IntExp(4)),IntExp(8)),IntExp(16)),IntExp(32)))
    }

    test ("Precedence and Associtivity Test #10 - : Right Associativity"){
        exp("3 < 16 : [2, 4, 8]") should parseTo[HasLangNode] (LessExp(IntExp(3),ConsExp(IntExp(16),ListExp(Vector(IntExp(2),IntExp(4),IntExp(8))))))
    }

    test ("Precedence and Associtivity Test #11 - AppExp Left Associativity"){
        exp("(2 + 4) (8 + 16)") should parseTo[HasLangNode] (AppExp(PlusExp(IntExp (2),IntExp(4)),PlusExp(IntExp(8),IntExp(16))))
    }

    test ("Precedence and Associtivity Test #12 - If and LambdaExp Left Associativity"){
        exp("if (\\x :: Int -> [Bool] -> x + 2 + 4) then [2 * 4, 8 + 16] else (16 / 8, 4 - 2)") should parseTo[HasLangNode] (IfExp(
        LamExp(
            IdnDef("x",FunType(IntType(),ListType(BoolType()))),
            PlusExp(PlusExp(IdnUse("x"),IntExp(2)),IntExp(4))),
        ListExp(
            Vector(
                StarExp(IntExp(2),IntExp(4)),
                PlusExp(IntExp(8),IntExp(16)))),
        TupleExp(
            Vector(
                SlashExp(IntExp(16),IntExp(8)),
                MinusExp(IntExp(4),IntExp(2))))))
    }

    test ("Precedence and Associtivity Test #13 - ListExp Left Associativity"){
        exp("[[2, 4, 8], 2 * 4, 8 + 16]") should parseTo[HasLangNode] (ListExp(
        Vector(
            ListExp(Vector(IntExp(2),IntExp(4),IntExp(8))),
            StarExp(IntExp(2),IntExp(4)),
            PlusExp(IntExp(8),IntExp(16)))))
    }

    test ("Precedence and Associtivity Test #14 - TupleExp Left Associativity"){
        exp("((2, 4, 8), 4 / 2, 16 - 8)") should parseTo[HasLangNode] (TupleExp(
        Vector(
            TupleExp(Vector(IntExp(2),IntExp(4),IntExp(8))),
            SlashExp(IntExp(4),IntExp(2)),
            MinusExp(IntExp(16),IntExp(8)))))
    }

    test ("Precedence and Associtivity Test #15 - LetExp Left Associativity"){
        program("""let
                    x   :: Int        = 62;
                    inc :: Int -> Int = \a :: Int -> a + a;
                    sum :: [Int] -> Int
                    sum [2, 4, 8, 16, 32] = 5.
                    sum [] = 0. 
                    sum h:t = h + sum t
                  in
                    if (2 + 4 + 8 + 16 + 32 == x) then 32 - 16 - 8 - 4 - 2 == 2 else sum invalid)""") should parseTo[Program] (Program(
    LetExp(
        Vector(
            Defn(
                IdnDef("x",IntType()),
                Vector(FunLine ("", Vector(),IntExp(62)))),
            Defn (
                IdnDef("inc",FunType(IntType(),IntType ())),
                Vector(
                    FunLine("",Vector(),
                        LamExp(
                            IdnDef("a",IntType()),
                            PlusExp(IdnUse("a"),IdnUse("a")))))),
            Defn (
                IdnDef("sum",FunType(ListType(IntType()),IntType())),
                Vector(FunLine("sum",Vector(
                            ListPat(
                                Vector(
                                    LiteralPat(IntExp(2)),
                                    LiteralPat(IntExp(4)),
                                    LiteralPat(IntExp(8)),
                                    LiteralPat(IntExp(16)),
                                    LiteralPat(IntExp(32))))),
                        IntExp(5)),
                    FunLine("sum",Vector(ListPat(Vector())),
                        IntExp(0)),
                    FunLine("sum",Vector(ConsPat(IdentPat("h"),IdentPat("t"))),
                        PlusExp(
                            IdnUse("h"),
                            AppExp(IdnUse("sum"),IdnUse("t"))))))),
        IfExp(
            EqualExp(
                PlusExp(
                    PlusExp(
                        PlusExp(
                            PlusExp(IntExp(2),IntExp(4)),
                            IntExp(8)),
                        IntExp(16)),
                    IntExp(32)),
                IdnUse("x")),
            EqualExp(
                MinusExp(
                    IntExp(32),
                    MinusExp(
                        IntExp(16),
                        MinusExp(
                            IntExp(8),
                            MinusExp(IntExp(4),IntExp(2))))),
                IntExp(2)),
            AppExp(IdnUse("sum"),IdnUse("invalid"))))))
   }

   // Type Testing

   test ("Testing ListType With Set of Empty () #1") {
        tipe ("[()]") should parseTo[Type] (ListType(UnitType()))
    }

   test ("Strenuous Testing of TupleType #2") {
        tipe ("((Int, Int, Bool, (), Int -> Bool), Int -> Bool, [Bool], Int)") should parseTo[Type] (TupleType(Vector
        (TupleType
        (Vector(IntType(),IntType(),BoolType(),UnitType(),FunType(IntType(),BoolType()))),FunType(IntType(), BoolType()), ListType(BoolType()),IntType())))
    }

    test ("Testing IntType #3"){
        tipe ("Int") should parseTo[Type] (IntType())
    }

    test ("Multi Bracketted FunType #4"){
        tipe ("(Int -> Bool) -> ([Bool] -> Int) ") should parseTo[Type] (FunType(FunType(IntType(),BoolType()),FunType(ListType(BoolType()),IntType())))
    }

    test ("Multi FunType #5"){
        tipe ("Int -> Bool -> () -> Int -> Bool -> [Int]") should parseTo[Type] (FunType(IntType(),FunType(BoolType(),FunType(UnitType(),FunType(IntType(),FunType(BoolType(),ListType(IntType())))))))
    }

    // Pattern Testing

     test ("Empty List Pat [] #1") {
        pat ("[]") should parseTo[Pat] (ListPat(Vector()))
    }

    test ("True pat #2") {
        pat ("true") should parseTo[Pat] (LiteralPat(BoolExp (true)))
    }

    test ("False pat #3") {
        pat ("false") should parseTo[Pat] (LiteralPat(BoolExp (false)))
    }

    test ("Testing ListPat #4"){
        pat ("[[2, 4, 8, 16, _], 32, 64, _]") should parseTo[Pat] (ListPat(
        Vector(
            ListPat(Vector(LiteralPat(IntExp(2)),LiteralPat(IntExp(4)),LiteralPat(IntExp(8)),LiteralPat(IntExp(16)), AnyPat())),
            LiteralPat(IntExp(32)), LiteralPat(IntExp(64)), AnyPat())))
    }

    test ("Testing TuplePat #5"){
        pat ("((2, 4, 8, 16, _), 32, 64, _)") should parseTo[Pat] (TuplePat(
        Vector(
            TuplePat(Vector(LiteralPat(IntExp(2)),LiteralPat(IntExp(4)),LiteralPat(IntExp(8)),LiteralPat(IntExp(16)), AnyPat())),
            LiteralPat(IntExp(32)), LiteralPat(IntExp(64)), AnyPat())))
    }

    test ("Testing ConsPat #6"){
        pat ("_ : [2, 4, 8]") should parseTo[Pat] (ConsPat(AnyPat(), ListPat(Vector(LiteralPat(IntExp(2)),LiteralPat(IntExp(4)),LiteralPat(IntExp(8))))))
    }

    test ("Testing ConsPat #7"){
        pat ("_ : [_, _, _]") should parseTo[Pat] (ConsPat(AnyPat(), ListPat(Vector(AnyPat(),AnyPat(),AnyPat()))))
    }

    // Funline, Definitions, Defn Testing

    test ("sum FunLine Testing #1") {
        funline ("sum h:t = h + sum t") should parseTo[FunLine] (FunLine("sum", Vector(ConsPat(IdentPat("h"), IdentPat("t"))), PlusExp(IdnUse("h"), AppExp(IdnUse("sum"), IdnUse("t")))))
    }

    test ("Multi FunLine Testing #2") {
      defn ("""sum :: Bool -> [Int]
               sum 0 = 1.
               sum n = n + n.
               sum 1 = 2.
               sum 2 = 3.
               sum 3 = 4.
               sum 4 = 5
            """) should parseTo[Defn] (Defn(
                  IdnDef("sum", FunType(BoolType(), ListType(IntType()))),
                  Vector(FunLine("sum", Vector(LiteralPat(IntExp(0))),
                                 IntExp(1)),
                         FunLine("sum", Vector(IdentPat("n")),
                                 PlusExp(IdnUse("n"),IdnUse("n"))),
                            FunLine("sum", Vector(LiteralPat(IntExp(1))),
                                 IntExp(2)),
                                    FunLine("sum", Vector(LiteralPat(IntExp(2))),
                                        IntExp(3)),
                                             FunLine("sum", Vector(LiteralPat(IntExp(3))),
                                                IntExp(4)),
                                                     FunLine("sum", Vector(LiteralPat(IntExp(4))),
                                                        IntExp(5)))))
    }

     test ("Multi Definitions Testing #3") {
      definitions ("""a   :: [Int]        = [2, 4, 8];
                      b   :: Bool         = true;
                      c   :: ()           = (2, 4, 8);
                      d   :: Int          = 69;
                      e   :: Bool         = false
                      
                """) should parseTo[Vector[Defn]] (Vector(
                       Defn(IdnDef("a", ListType(IntType())),
                            Vector(FunLine("", Vector(), ListExp(Vector(IntExp(2), IntExp(4), IntExp(8)))))),
                       Defn(IdnDef("b", BoolType()),
                            Vector(FunLine("", Vector(), BoolExp(true)))),
                        Defn(IdnDef("c", UnitType()),
                            Vector(FunLine("", Vector(), TupleExp(Vector(IntExp(2), IntExp(4), IntExp(8)))))),
                        Defn(IdnDef("d", IntType()),
                            Vector(FunLine("", Vector(), IntExp(69)))),
                        Defn(IdnDef("e", BoolType()),
                            Vector(FunLine("", Vector(), BoolExp(false))))))
    }

}
