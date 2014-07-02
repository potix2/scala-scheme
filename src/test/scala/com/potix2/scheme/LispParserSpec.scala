package com.potix2.scheme

import org.specs2.matcher.Matcher
import org.specs2.mutable._

class LispParserSpec extends SpecificationWithJUnit {
  val parser = new LispParser {}

  "readExpr" should {
    "return quoted identifier" in {
      parser.readExpr("'a") must beLispVal(LispList(List(LispAtom("quote"), LispAtom("a"))))
    }
    "return expression" in {
      parser.readExpr("(+ 1 2)") must beLispVal(LispList(List(LispAtom("+"), LispLong(1), LispLong(2))))
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
    "123" in {
      parser.readExpr("123") must beLispVal(LispLong(123))
    }
    "-1" in {
      parser.readExpr("-1") must beLispVal(LispLong(-1))
    }
    "1.0" in {
      parser.readExpr("1.0") must beLispVal(LispDouble(1.0d))
    }
    ".1234" in {
      parser.readExpr(".1234") must beLispVal(LispDouble(0.1234d))
    }
    "-1.234" in {
      parser.readExpr("-1.234") must beLispVal(LispDouble(-1.234d))
    }
    "-.1234" in {
      parser.readExpr("-.1234") must beLispVal(LispDouble(-0.1234d))
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
      parser.readExpr("(a b . c)") must beLispVal(LispDottedList(List(LispAtom("a"), LispAtom("b")), LispAtom("c")))
    }
    "(define (list . objs) objs)" in {
      parser.readExpr("(define (list . objs) objs)") must beLispVal(LispList(List(LispAtom("define"), LispDottedList(List(LispAtom("list")), LispAtom("objs")), LispAtom("objs"))))
    }
    "()" in {
      parser.readExpr("()") must beLispVal(LispList(List()))
    }
  }

  "comments" should {
    "skip text followed by a semicolon" in {
      parser.readExpr("""
                        |; comment line
                        |a
                      """.stripMargin) must beLispVal(LispAtom("a"))
    }
    "skip text followed by a semicolon" in {
      parser.readExpr("a ; comment line") must beLispVal(LispAtom("a"))
    }
    "skip text followed by a semicolon" in {
      parser.readExpr("""
                        |; comment line
                        |a
                      """.stripMargin) must beLispVal(LispAtom("a"))
    }
    "skip text quoted by #| and |#" in {
      val e = """|#|
                | comment line
                ||#
                |a""".stripMargin
      parser.readExpr(e) must beLispVal(LispAtom("a"))
    }
    "be skipped when it is in an expression" in {
      val s: String = """
                        |(a
                        |   #; comment
                        |b c)""".stripMargin
      parser.readExpr(s) must beLispVal(LispList(List(LispAtom("a"), LispAtom("b"), LispAtom("c"))))
    }
  }
  def beLispVal(expected:LispVal): Matcher[ThrowsError[LispVal]] = { value: ThrowsError[LispVal] =>
    value.toEither must beRight(expected)
  }
}
