package com.potix2.scheme

import scala.util.parsing.combinator._

sealed trait LispVal
case class LispAtom(value: String) extends LispVal
case class LispList(value: List[LispVal]) extends LispVal
case class LispDottedList(list: List[LispVal], value: LispVal) extends LispVal
abstract trait LispNumber extends LispVal
case class LispComplex(r: Double, i: Double) extends LispNumber
case class LispReal(value: Double) extends LispNumber
case class LispUReal(value: Int) extends LispNumber
case class LispDecimal(value: Int) extends LispNumber
case class LispInteger(value: Int) extends LispNumber
case class LispString(value: String) extends LispVal
case class LispBool(value: Boolean) extends LispVal

sealed trait Radix
case class Binary extends Radix
case class Oct extends Radix
case class Digit extends Radix
case class Hex extends Radix

trait LispParser extends RegexParsers {
  def readExpr(input: String): String = {
    parseAll(token, input) match {
      case Failure(msg, in) => msg
      case Success(result, next) => "Found Value"
    }
  }

  //TODO: inline_hex_digitsに対応する
  def symbol: Parser[String] = """[!#$%&|\\*+-/:<=>?@^_~]""".r
  def letter: Parser[String] = """[a-zA-Z]""".r
  def token: Parser[LispVal] =
    identifier ^^ (LispAtom(_)) |
    boolean ^^ (LispBool(_)) |
    string ^^ (LispString(_)) |
    number

  def identifier: Parser[String] = initial ~ rep(subsequent) ^^ { case x~xs => xs.foldLeft(x)(_ + _) } |
    "|" ~> rep(symbol_element) <~ "|" ^^ { case xs => xs.foldLeft("")(_ + _) } |
    perculiar_identifier
  def initial: Parser[String] = letter | special_initial
  def special_initial: Parser[String] = """[!$%&*/:<=>?^_~]""".r
  def subsequent: Parser[String] = initial | digit | special_subsequent
  def digit: Parser[String] = "[0-9]".r
  def explicit_sign: Parser[String] = "+" | "-"
  def special_subsequent: Parser[String] = explicit_sign | "." | "@"
  def symbol_element: Parser[String] = "[^|\\\\]".r
  def perculiar_identifier: Parser[String] =
    explicit_sign ~ "." ~ dot_subsequent ~ rep(subsequent) ^^ { case es~"."~ds~subs => subs.foldLeft(es+"."+ds)(_ + _)} |
    explicit_sign ~ sign_subsequent ~ rep(subsequent) ^^ { case es~ss~subs => subs.foldLeft(es+ss)(_ + _) } |
    explicit_sign |
    "." ~ non_digit ~ rep(subsequent) ^^ { case "."~nd~subs => subs.foldLeft("."+nd)(_ + _)}
  def non_digit: Parser[String] = dot_subsequent | explicit_sign
  def dot_subsequent: Parser[String] = sign_subsequent | "."
  def sign_subsequent: Parser[String] = initial | explicit_sign | "@"

  def string: Parser[String] = "\"" ~> "([^\"\\\\]|\\\\a|\\\\b|\\\\t|\\\\n|\\\\r)*".r <~ "\""

  def boolean: Parser[Boolean] = boolTrue ^^ (x => true) | boolFalse ^^ (x => false)
  def boolTrue: Parser[String] = "#true" | "#t"
  def boolFalse: Parser[String] = "#false" | "#f"

  def number: Parser[LispNumber] = int10 ^^ (LispInteger(_))
  def int10: Parser[Int] = rep(digit) ^^ (xs => (xs.foldLeft("")(_ + _)).toInt)
}


