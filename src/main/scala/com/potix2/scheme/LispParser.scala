package com.potix2.scheme

import scala.util.parsing.combinator._
import com.potix2.scheme.LispError._
import scalaz._
import scalaz.Scalaz._

trait LispParser extends RegexParsers {
  override val whiteSpace = "".r
  def readExpr(input: String): ThrowsError[LispVal] = {
    parseAll(program, input) match {
      case Failure(msg, in) => ParseFailure(msg).left[LispVal]
      case Success(result, next) => result.point[ThrowsError]
    }
  }

  //7.1.1 Lexical structure
  //TODO: inline_hex_digitsに対応する
  def token: Parser[LispVal] =
    identifier |
    boolean |
    string |
    number

  def identifier: Parser[LispAtom] =
    initial ~ rep(subsequent) ^^ { case x~xs => LispAtom(xs.foldLeft(x)(_ + _)) } |
    "|" ~> rep(symbol_element) <~ "|" ^^ { case xs => LispAtom(xs.foldLeft("")(_ + _)) } |
    peculiar_identifier ^^ LispAtom
  def initial: Parser[String] = letter | special_initial
  def letter: Parser[String] = """[a-zA-Z]""".r
  def special_initial: Parser[String] = """[!$%&*/:<=>?^_~]""".r
  def subsequent: Parser[String] = initial | digit | special_subsequent
  def digit: Parser[String] = "[0-9]".r
  def explicit_sign: Parser[String] = "+" | "-"
  def special_subsequent: Parser[String] = explicit_sign | "." | "@"
  def symbol_element: Parser[String] = "[^|\\\\]".r
  def peculiar_identifier: Parser[String] =
    explicit_sign ~ "." ~ dot_subsequent ~ rep(subsequent) ^^ { case es~"."~ds~subs => subs.foldLeft(es+"."+ds)(_ + _)} |
    explicit_sign ~ sign_subsequent ~ rep(subsequent) ^^ { case es~ss~subs => subs.foldLeft(es+ss)(_ + _) } |
    explicit_sign |
    "." ~ non_digit ~ rep(subsequent) ^^ { case "."~nd~subs => subs.foldLeft("."+nd)(_ + _)}
  def non_digit: Parser[String] = dot_subsequent | explicit_sign
  def dot_subsequent: Parser[String] = sign_subsequent | "."
  def sign_subsequent: Parser[String] = initial | explicit_sign | "@"

  def string: Parser[LispVal] = "\"" ~> "([^\"\\\\]|\\\\a|\\\\b|\\\\t|\\\\n|\\\\r)*".r <~ "\"" ^^ LispString

  def boolean: Parser[LispBool] = boolTrue ^^ (x => LispBool(true)) | boolFalse ^^ (x => LispBool(false))
  def boolTrue: Parser[String] = "#true" | "#t"
  def boolFalse: Parser[String] = "#false" | "#f"

  def character: Parser[LispChar] =
    "#\\" ~> character_name |
//    "#\\" ~> hex_scalar_value |
    "#\\" ~> ".".r ^^ (LispChar(_))
  def character_name: Parser[LispChar] =
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
  def number: Parser[LispNumber] = int10 ^^ LispInteger
  def int10: Parser[Int] = rep1(digit10) ^^ (xs => xs.foldLeft("")(_ + _).toInt)
  def digit10: Parser[String] = digit

  //7.1.2 External representations
  def datum: Parser[LispVal] =
    compound_datum |
    simple_datum
  // TODO: labelを実装する(ref. 2.4)
//    label ~ "=" ~ datum |
//    label <~ "#"

  def simple_datum: Parser[LispVal] =
    symbol |
    boolean |
    character |
    string |
    number
//    bytevector

  val spaces = "[ \t\r\n]+".r
  def symbol: Parser[LispAtom] = identifier
  def compound_datum: Parser[LispVal] = list | vector
  def list: Parser[LispVal] =
    "(" ~> rep1sep(datum, spaces) ~ rep1(spaces) ~ "." ~ rep1(spaces) ~ datum ~ rep(spaces) <~ ")" ^^ {case xs~s1~"."~s2~y~s3 => LispDottedList(xs, y)} |
    "(" ~> repsep(datum, spaces) <~ ")" ^^ LispList |
    abbreviation
  def abbreviation: Parser[LispVal] =
    abbrev_prefix ~ datum ^^  { case prefix~v => v }
  def abbrev_prefix: Parser[String] = "'" | "`" | "," | ",@"
  def vector: Parser[LispVector] =  "#(" ~> repsep(datum, spaces) <~ ")" ^^ (xs => LispVector(xs.toVector))
  //def label: Parser[Int] = "#" ~> digit10 ~ rep(digit10) ^^ {case x~xs => xs.foldLeft(x)(_ + _).toInt }

  def expression: Parser[LispVal] =
    identifier |
    procedure_call |
    exprLiteral

  def exprLiteral: Parser[LispVal] =
    quotation |
    self_evaluating

  def self_evaluating: Parser[LispVal] =
    boolean |
    character |
    string |
    number

  def quotation: Parser[LispVal] =
    "'" ~> datum ^^ (x => LispList(List(LispAtom("quote"), x)))

  def procedure_call: Parser[LispVal] =
    "(" ~> operator ~ spaces ~ repsep(operand, spaces) <~ ")" ^^
      { case op~s~ops => LispList(List(op) ++ ops)}

  def operator: Parser[LispVal] = expression
  def operand: Parser[LispVal] = expression

  def command: Parser[LispVal] = expression
  def program: Parser[LispVal] = command
}
