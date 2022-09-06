/**
 * HasLang syntax analyser.
 *
 * Copyright 2021, Anthony Sloane, Matthew Roberts, Kym Haines, Macquarie University, All rights reserved.
 */

package haslang

import org.bitbucket.inkytonik.kiama.parsing.Parsers
import org.bitbucket.inkytonik.kiama.util.Positions
import scala.reflect.internal.Definitions

/**
 * Module containing parsers for HasLang.
 */
class SyntaxAnalysis (positions : Positions) extends Parsers (positions) {

    import HasLangTree._
    import scala.language.postfixOps

    lazy val parser : PackratParser[Program] =
        phrase (program)

    lazy val program : PackratParser[Program] =
        exp ^^ Program

    lazy val literal : PackratParser[Exp] =
        "false" ^^^ BoolExp (false) |
        "true" ^^^ BoolExp (true) |
        integer ^^ (s => IntExp (s.toInt))

    lazy val letexp : PackratParser[Exp] = 
        ("let" ~> definitions) ~ ("in" ~> exp) ^^ {case x ~ y => LetExp(x, y)} 

    //precedence 0
    lazy val exp : PackratParser[Exp] = 
        // FIXME
        exp1
    
    //precedence 1 if Left Associativity 
    lazy val exp1 : PackratParser[Exp] =
        ("if" ~> "(" ~> exp1 <~ ")") ~ ("then" ~> exp2) ~ ("else" ~> exp2) ^^ {case x ~ y ~ z => IfExp(x, y, z)} |
        exp2

    //precedence 2 == and < No Associativity 
    lazy val exp2 : PackratParser[Exp] =
        exp3 ~ ("==" ~> exp3) ^^ {case x ~ y => EqualExp(x, y)} |
        exp3 ~ ("<" ~> exp3) ^^ {case x ~ y => LessExp(x, y)} |
        exp3

    //precedence 3 : Right Associativity
    lazy val exp3 : PackratParser[Exp] =
        exp4 ~ (":" ~> exp3) ^^ {case x ~ y => ConsExp(x, y)} |
        exp4
    
    //precedence 4 + Left Associativity and - Right Associativity
    lazy val exp4 : PackratParser[Exp] =
        exp4 ~ ("+" ~> exp5) ^^ {case x ~ y => PlusExp(x, y)} |
        exp5 ~ ("-" ~> exp4) ^^ {case x ~ y => MinusExp(x, y)} |
        exp5

    //precedence 5 * Left Associativity and / Right Associativity
    lazy val exp5 : PackratParser[Exp] =
        exp5 ~ ("*" ~> factor) ^^ {case x ~ y => StarExp(x, y)} |
        factor ~ ("/" ~> exp5) ^^ {case x ~ y => SlashExp(x, y)} |
        factor
    
    //precedence 6 All Other Expressions
    lazy val factor : PackratParser[Exp] =
        // FIXME
        factor ~ factor ^^ { case x ~ y => AppExp(x, y)} |
        ("\\" ~> identifier <~ "::") ~ (tipe) ~ ("->" ~> exp) ^^ {case x ~ y ~ z => LamExp(IdnDef(x,y),z)} |
        "[]" ^^ { case x => ListExp(Vector())} |
        literal |
        identifier ^^ IdnUse |
        "(" ~> exp <~ ")" |
        "[" ~> listexp <~ "]" |
        "(" ~> tupexp <~ ")" |
        exp |
        letexp |
        failure ("exp expected")

    // FIXME   add parsers between factor and exp

    lazy val listexp : PackratParser[Exp] = 
        //repsep exp implemented according to specs where ListExp+ seperated by ','
         repsep(exp, ",") ^^ {case x => ListExp(x)} |
         factor

    lazy val tupexp : PackratParser[Exp] =
        //rep1sep exp implemented according to specs where TupleExp* seperated by ','
        rep1sep(exp, ",") ^^ {case x => TupleExp(x)} |
        factor
    
    lazy val definitions : PackratParser[Vector[Defn]] =
        // FIXME
        //repsep defn implemented according to specs where defn+ seperated by ';'
        repsep(defn, ";") ^^ Vector[Defn]

    lazy val defn : PackratParser[Defn] =
        // FIXME
        //repsep funline implemented according to specs where funline+ seperated by '.'
        identifier ~ ("::" ~> tipe) ~ (repsep(funline, ".")) ^^ {case x ~ y ~ z => Defn(IdnDef(x, y), z)} 

    lazy val funline : PackratParser[FunLine] =
        // FIXME
        //repsep pat implemented according to specs where pat+ seperated by ','
        ("=" ~> exp) ^^ {case x => FunLine("", Vector(), x)} |
        identifier ~ repsep(pat, ",") ~ ("=" ~> exp) ^^ {case x ~ y ~ z => FunLine(x, y, z)} |
        funline

    lazy val pat : PackratParser[Pat] =
        // FIXME
        //repsep pat implemented according to specs where ListPat+ seperated by ','
        //rep1sep pat implemented according to specs where TuplePat* seperated by ','
        pat ~ (":" ~> pat) ^^ {case x ~ y => ConsPat(x, y)} |
        "(" ~> pat <~ ")" |
        ("[" ~> repsep(pat, ",") <~ "]") ^^ {case x => ListPat(x)} |
        ("(" ~> rep1sep(pat, ",") <~ ")") ^^ {case x => TuplePat(x)} |
        basicpat

    lazy val basicpat : PackratParser[Pat] =
        // FIXME
        "_" ^^^ AnyPat() |
        literal ^^ LiteralPat |
        identifier ^^ IdentPat |
        "[]" ^^^ ListPat(Vector()) |
        pat 

    lazy val tipe : PackratParser[Type] =
        // FIXME
        //rep1sep tipe implemented according to specs where TupleType* seperated by ','
        tipe ~ ("->" ~> tipe) ^^ {case x ~ y => FunType(x, y)} |
        ("(" ~> tipe <~ ")") |
        ("[" ~> tipe <~ "]") ^^ {case x  => ListType(x)} |
        ("(" ~> rep1sep(tipe, ",") <~ ")") ^^ {case x => TupleType(x)} |
        basictipe

    lazy val basictipe : PackratParser[Type] =
        // FIXME
        "Bool" ^^^ BoolType() |
        "Int" ^^^ IntType() |
        "()" ^^^ UnitType() |
        tipe 
        

    // NOTE: You should not change anything below here...

    lazy val integer =
        regex ("[0-9]+".r)

    lazy val idndef =
        (identifier <~ "::") ~ tipe ^^ IdnDef

    val keywordStrings =
        List ("let", "else", "false", "if", "then", "true", "in")

    lazy val keyword =
        keywords ("[^a-zA-Z0-9_]".r, keywordStrings)

    lazy val identifier =
        not (keyword) ~> identifierBase

    lazy val identifierBase =
        regex ("[a-zA-Z][a-zA-Z0-9_]*".r) |
        failure ("identifier expected")

    lazy val whitespaceParser : PackratParser[Any] =
        rep ( """\s""".r | comment)

    lazy val comment : PackratParser[Any] =
        "{-" ~ rep (not ("-}") ~ (comment | any)) ~ "-}" |
        "--.*(\n|\\z)".r

}
