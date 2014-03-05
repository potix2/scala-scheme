package com.potix2.scheme

import org.specs2.mutable._

class LispParserSpec extends SpecificationWithJUnit {
  val parser = new LispParser {}

  "readExpr" should {
    "return 'Found value' when input string is valid" in {
      parser.readExpr("%") must_== "Found Value"
    }
    "return an input string when the input string is invalid" in {
      parser.readExpr("abc") must_== "Found Value"
    }
    "ignore white space" in {
      parser.readExpr("  %") must_== "Found Value"
    }
  }
  "parseString" should {
    "return LispString with quoted string" in {
      parser.parseAll(parser.string, "\"abc\"").get must_== "abc"
    }
    "return LispString with escaped quote" in {
      parser.parseAll(parser.string, "\"\\t\\a\\b\\n\\r\"").get must_== "\\t\\a\\b\\n\\r"
    }
  }

  "parseBool" should {
    "return True when the input is #t" in {
      parser.parseAll(parser.boolean, "#t").get must_== true
    }
    "return True when the input is #true" in {
      parser.parseAll(parser.boolean, "#true").get must_== true
    }
    "return True when the input is #f" in {
      parser.parseAll(parser.boolean, "#f").get must_== false
    }
    "return True when the input is #false" in {
      parser.parseAll(parser.boolean, "#false").get must_== false
    }
  }

  "number" should {
    "return integer when the input is integer" in {
      parser.parseAll(parser.number, "123").get must_== LispInteger(123)
    }
  }
}
