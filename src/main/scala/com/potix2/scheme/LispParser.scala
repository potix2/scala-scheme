package com.potix2.scheme

import scala.util.parsing.combinator._

import scalaz.Scalaz._

trait LispParser extends RegexParsers {
  override val whiteSpace = """(?s)(#\|.*\|#|#?;[^\r\n]*[\r\n]*)+""".r
  val spaces = "[ \t\r\n]+".r

  def readOrThrow[A](parser: Parser[A], input: String): ThrowsError[A] =
    parseAll(parser, input) match {
      case Failure(msg, in) => ParseFailure(msg).left[A]
      case Success(result, next) => result.point[ThrowsError]
    }

  def readExpr(input: String): ThrowsError[LispVal] = readOrThrow(command, input)
  def readExprList(input: String): ThrowsError[List[LispVal]] = readOrThrow(program, input)

  def command: Parser[LispVal] = rep(spaces) ~> expression <~ rep(spaces)
  def program: Parser[List[LispVal]] = rep(spaces) ~> repsep(expression, spaces) <~ rep(spaces)

  def expression: Parser[LispVal] =
    vector |
    list |
    quotation |
    lispLiteral |
    identifier

  //7.1.1 Lexical structure
  def identifier: Parser[LispAtom] =
    initial ~ rep(subsequent) ^^ { case x~xs => LispAtom(xs.foldLeft(x)(_ + _)) } |
    "|" ~ rep(symbolElement) ~ "|" ^^ { case vb1~xs~vb2 => LispAtom(xs.foldLeft("|")(_ + _) + "|") } |
    peculiarIdentifier ^^ LispAtom
  def initial: Parser[String] = letter | specialInitial
  def letter: Parser[String] = """[a-zA-Z]""".r
  def specialInitial: Parser[String] = """[!$%&*/:<=>?^_~]""".r
  def subsequent: Parser[String] = initial | digit | specialSubsequent
  def digit: Parser[String] = "[0-9]".r
  def explicitSign: Parser[String] = "+" | "-"
  def specialSubsequent: Parser[String] = explicitSign | "." | "@"
  def symbolElement: Parser[String] = "[^|\\\\]".r
  def peculiarIdentifier: Parser[String] =
    explicitSign ~ "." ~ dotSubsequent ~ rep(subsequent) ^^ { case es~"."~ds~subs => subs.foldLeft(es+"."+ds)(_ + _)} |
    explicitSign ~ signSubsequent ~ rep(subsequent) ^^ { case es~ss~subs => subs.foldLeft(es+ss)(_ + _) } |
    explicitSign |
    "." ~ nonDigit ~ rep(subsequent) ^^ { case "."~nd~subs => subs.foldLeft("."+nd)(_ + _)}
  def nonDigit: Parser[String] = dotSubsequent | explicitSign
  def dotSubsequent: Parser[String] = signSubsequent | "."
  def signSubsequent: Parser[String] = initial | explicitSign | "@"

  // literals
  def string: Parser[LispVal] = "\"" ~> "([^\"\\\\]|\\\\a|\\\\b|\\\\t|\\\\n|\\\\r)*".r <~ "\"" ^^ LispString

  def boolean: Parser[LispBool] = boolTrue ^^ (x => LispBool(true)) | boolFalse ^^ (x => LispBool(false))
  def boolTrue: Parser[String] = "#true" | "#t"
  def boolFalse: Parser[String] = "#false" | "#f"

  def character: Parser[LispChar] =
    "#\\" ~> characterName |
//    "#\\" ~> hex_scalar_value |
    "#\\" ~> ".".r ^^ (LispChar(_))
  def characterName: Parser[LispChar] =
    "alarm"     ^^ (x => LispChar("\u0007")) |
    "backspace" ^^ (x => LispChar("\u0008")) |
    "delete"    ^^ (x => LispChar("\u007F")) |
    "escape"    ^^ (x => LispChar("\u001B")) |
    "newline"   ^^ (x => LispChar("\u000A")) |
    "null"      ^^ (x => LispChar("\u0000")) |
    "return"    ^^ (x => LispChar("\u000D")) |
    "space"     ^^ (x => LispChar(" ")) |
    "tab"       ^^ (x => LispChar("\u0009"))

  //TODO: implement number
  def number: Parser[LispNumber] =
    real10    ^^ LispDouble |
    int10     ^^ LispLong
  def int10: Parser[Long] = rep1(digit10) ^^ (xs => xs.foldLeft("")(_ + _).toLong) |
    explicitSign ~ rep1(digit10) ^^ { case s ~ xs => xs.foldLeft(s)(_ + _).toLong }
  def real10: Parser[Double] =
    ureal10 ^^ { _.toDouble } |
    explicitSign ~ ureal10 ^^ { case sign~r => (sign + r).toDouble }
  def ureal10: Parser[String] = decimal10
  def decimal10: Parser[String] =
    "." ~ rep1(digit) ^^ { case x~xs => xs.foldLeft(x)(_ + _) } |
    rep1(digit) ~ "." ~ rep(digit) ^^ { case i~dot~d => (i.foldLeft("")(_ + _) + dot + d.foldLeft("")(_+_)) }
  def digit10: Parser[String] = digit

  //7.1.2 External representations
  def list: Parser[LispVal] =
    "(" ~> rep(spaces) ~ rep1sep(expression, spaces) ~ rep1(spaces) ~ "." ~ rep1(spaces) ~ expression ~ rep(spaces) <~ ")" ^^ {case s1~xs~s2~"."~s3~y~s4 => LispDottedList(xs, y)} |
    "(" ~> rep(spaces) ~ repsep(expression, spaces) ~ rep(spaces) <~ ")" ^^ { case s1~xs~s2 => LispList(xs) }
  def vector: Parser[LispVector] =  "#(" ~> repsep(expression, spaces) <~ ")" ^^ (xs => LispVector(xs.toVector))

  def lispLiteral: Parser[LispVal] =
    boolean |
    character |
    string |
    number

  def quotation: Parser[LispVal] =
    "'" ~> expression ^^ (x => LispList(List(LispAtom("quote"), x)))
}
