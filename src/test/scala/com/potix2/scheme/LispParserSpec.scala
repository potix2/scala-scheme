package com.potix2.scheme

import org.specs2.mutable._
import com.potix2.scheme.LispError.ThrowsError
import scalaz.Scalaz._

class LispParserSpec extends SpecificationWithJUnit {
  val parser = new LispParser {}

  "readExpr" should {
    "return quoted identifier" in {
      parser.readExpr("'a") must_== LispList(List(LispAtom("quote"), LispAtom("a"))).point[ThrowsError]
    }
    "return expression" in {
      parser.readExpr("(+ 1 2)") must_== LispList(List(LispAtom("+"), LispInteger(1), LispInteger(2))).point[ThrowsError]
    }
//    "return an input string when the input string is invalid" in {
    //      parser.readExpr("(a b)") must_== LispList(List(LispAtom("a"), LispAtom("b")))
//    }
  }

  "identifier" should {
    "return LispAtom" in {
      parser.parseAll(parser.identifier, "quote").get must_== LispAtom("quote")
      parser.parseAll(parser.identifier, "a123!").get must_== LispAtom("a123!")
    }
    "return LispAtom with vertical lines" in {
      parser.parseAll(parser.identifier, "|a b|").get must_== LispAtom("a b")
    }
    "return LispAtom with peculiar identifier" in {
      parser.parseAll(parser.identifier, "->").get must_== LispAtom("->")
      parser.parseAll(parser.identifier, "...").get must_== LispAtom("...")
    }
  }

  "parseBool" should {
    "return True when the input is #t" in {
      parser.parseAll(parser.boolean, "#t").get must_== LispBool(true)
    }
    "return True when the input is #true" in {
      parser.parseAll(parser.boolean, "#true").get must_== LispBool(true)
    }
    "return True when the input is #f" in {
      parser.parseAll(parser.boolean, "#f").get must_== LispBool(false)
    }
    "return True when the input is #false" in {
      parser.parseAll(parser.boolean, "#false").get must_== LispBool(false)
    }
  }

  "string" should {
    "return LispString with quoted string" in {
      parser.parseAll(parser.string, "\"abc\"").get must_== LispString("abc")
    }
    "return LispString with escaped quote" in {
      parser.parseAll(parser.string, "\"\\t\\a\\b\\n\\r\"").get must_== LispString("\\t\\a\\b\\n\\r")
    }
  }

  "number" should {
    "return integer when the input is integer" in {
      parser.parseAll(parser.number, "123").get must_== LispInteger(123)
    }
  }

  "datum" should {
    "return list when the token startsWith '('" in {
      parser.parseAll(parser.datum, "(a b)").get must_== LispList(List(LispAtom("a"), LispAtom("b")))
    }
    "return vector when the token startsWith '#('" in {
      parser.parseAll(parser.datum, "#(a b)").get must_== LispVector(Vector(LispAtom("a"), LispAtom("b")))
    }
    "return LispAtom with a token is identifier" in {
      parser.parseAll(parser.datum, "a").get must_== LispAtom("a")
    }
    "(a b . c)" in {
      parser.parseAll(parser.datum, "(a b . c)").get must_== LispDottedList(List(LispAtom("a"), LispAtom("b")), LispAtom("c"))
    }
  }
}
