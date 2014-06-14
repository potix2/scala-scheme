package com.potix2.scheme

import org.specs2.mutable._
import com.potix2.scheme.Lisp._
import org.specs2.matcher.Matcher

class LispParserSpec extends SpecificationWithJUnit {
  val parser = new LispParser {}

  "readExpr" should {
    "return quoted identifier" in {
      parser.readExpr("'a") must beLispVal(LispList(List(LispAtom("quote"), LispAtom("a"))))
    }
    "return expression" in {
      parser.readExpr("(+ 1 2)") must beLispVal(LispList(List(LispAtom("+"), LispInteger(1), LispInteger(2))))
    }
  }

  "identifier" should {
    "return LispAtom" in {
      parser.readExpr("quote") must beLispVal(LispAtom("quote"))
      parser.readExpr("a123!") must beLispVal(LispAtom("a123!"))
    }
    "return LispAtom with vertical lines" in {
      parser.readExpr("|a b|") must beLispVal(LispAtom("|a b|"))
    }
    "return LispAtom with peculiar identifier" in {
      parser.readExpr("->") must beLispVal(LispAtom("->"))
      parser.readExpr("...") must beLispVal(LispAtom("..."))
      parser.readExpr("+-a!$%&*/:<=>?^_~1@") must beLispVal(LispAtom("+-a!$%&*/:<=>?^_~1@"))
    }
  }

  "parseBool" should {
    "return True when the input is #t" in {
      parser.readExpr("#t") must beLispVal(LispBool(true))
    }
    "return True when the input is #true" in {
      parser.readExpr("#true") must beLispVal(LispBool(true))
    }
    "return True when the input is #f" in {
      parser.readExpr("#f") must beLispVal(LispBool(false))
    }
    "return True when the input is #false" in {
      parser.readExpr("#false") must beLispVal(LispBool(false))
    }
  }

  "string" should {
    "return LispString with quoted string" in {
      parser.readExpr("\"abc\"") must beLispVal(LispString("abc"))
    }
    "return LispString with escaped quote" in {
      parser.readExpr("\"\\t\\a\\b\\n\\r\"") must beLispVal(LispString("\\t\\a\\b\\n\\r"))
    }
  }

  "number" should {
    "return integer when the input is integer" in {
      parser.readExpr("123") must beLispVal(LispInteger(123))
    }
  }

  "datum" should {
    "return list when the token startsWith '('" in {
      parser.readExpr("(a b)") must beLispVal(LispList(List(LispAtom("a"), LispAtom("b"))))
    }
    "return vector when the token startsWith '#('" in {
      parser.readExpr("'#(a b)") must beLispVal(LispList(List(LispAtom("quote"), LispVector(Vector(LispAtom("a"), LispAtom("b"))))))
      parser.readExpr("#(a b)") must beLispVal(LispVector(Vector(LispAtom("a"), LispAtom("b"))))
    }
    "return LispAtom with a token is identifier" in {
      parser.readExpr("a") must beLispVal(LispAtom("a"))
    }
    "(a b . c)" in {
      parser.parseAll(parser.datum, "(a b . c)").get must_== LispDottedList(List(LispAtom("a"), LispAtom("b")), LispAtom("c"))
    }
    "(define (list . objs) objs)" in {
      parser.parseAll(parser.datum, "(define (list . objs) objs)").get must_== LispList(List(LispAtom("define"), LispDottedList(List(LispAtom("list")), LispAtom("objs")), LispAtom("objs")))
    }
    "()" in {
      parser.parseAll(parser.datum, "()").get must_== LispList(List())
    }
  }

  def beLispVal(expected:LispVal): Matcher[ThrowsError[LispVal]] = { value: ThrowsError[LispVal] =>
    value.toEither must beRight(expected)
  }
}
