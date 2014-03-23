package com.potix2.scheme

import org.specs2.mutable.SpecificationWithJUnit

class EvaluatorSpec extends SpecificationWithJUnit {
  val evaluator = new Evaluator {}

  "eval an atom" should {
    "return the atom" in {
      evaluator.eval(LispAtom("abcd")) must_== LispAtom("abcd")
    }
  }
  "eval a quoted value" should {
    "return the unquoted value" in {
      evaluator.eval(LispList(List(LispAtom("quote"), LispAtom("abcd")))) must_== LispAtom("abcd")
    }
  }
  "eval" should {
    "reduce (+ 1 2 3) to 6" in {
      evaluator.eval(LispList(List(LispAtom("+"), LispInteger(1), LispInteger(2), LispInteger(3)))) must_== LispInteger(6)
    }
    "reduce (- 4 1 2) to 1" in {
      evaluator.eval(LispList(List(LispAtom("-"), LispInteger(4), LispInteger(1), LispInteger(2)))) must_== LispInteger(1)
    }
    "reduce (+ 2 (- 4 1)) to 5" in {
      evaluator.eval(LispList(List(LispAtom("+"), LispInteger(2), LispList(List(LispAtom("-"), LispInteger(4), LispInteger(1)))))) must_== LispInteger(5)
    }
    "reduce (* 2 3) to 6" in {
      evaluator.eval(LispList(List(LispAtom("*"), LispInteger(2), LispInteger(3)))) must_== LispInteger(6)
    }
  }
  "type-test boolean?" should {
    "(boolean? #t)" in { evaluator.eval(makeExpr("boolean?", LispBool(true))) must_== LispBool(true) }
    "(boolean? #f)" in { evaluator.eval(makeExpr("boolean?", LispBool(false))) must_== LispBool(true) }
    "(boolean? \"abc\")" in { evaluator.eval(makeExpr("boolean?", LispString("abc"))) must_== LispBool(false) }
    "(boolean? 1)" in { evaluator.eval(makeExpr("boolean?", LispInteger(1))) must_== LispBool(false) }
    "(boolean? a)" in { evaluator.eval(makeExpr("boolean?", LispAtom("a"))) must_== LispBool(false) }
  }

  "type-test string?" should {
    "(string? #t)" in { evaluator.eval(makeExpr("string?", LispBool(true))) must_== LispBool(false) }
    "(string? \"abc\")" in { evaluator.eval(makeExpr("string?", LispString("abc"))) must_== LispBool(true) }
    "(string? 1)" in { evaluator.eval(makeExpr("string?", LispInteger(1))) must_== LispBool(false) }
    "(string? a)" in { evaluator.eval(makeExpr("string?", LispAtom("a"))) must_== LispBool(false) }
  }

  "type-test number?" should {
    "(number? #t)" in { evaluator.eval(makeExpr("number?", LispBool(true))) must_== LispBool(false) }
    "(number? \"abc\")" in { evaluator.eval(makeExpr("number?", LispString("abc"))) must_== LispBool(false) }
    "(number? 1)" in { evaluator.eval(makeExpr("number?", LispInteger(1))) must_== LispBool(true) }
    "(number? a)" in { evaluator.eval(makeExpr("number?", LispAtom("a"))) must_== LispBool(false) }
  }

  "type-test symbol?" should {
    "(symbol? #t)" in { evaluator.eval(makeExpr("symbol?", LispBool(true))) must_== LispBool(false) }
    "(symbol? \"abc\")" in { evaluator.eval(makeExpr("symbol?", LispString("abc"))) must_== LispBool(false) }
    "(symbol? 1)" in { evaluator.eval(makeExpr("symbol?", LispInteger(1))) must_== LispBool(false) }
    "(symbol? a)" in { evaluator.eval(makeExpr("symbol?", LispAtom("a"))) must_== LispBool(true) }
  }

  def makeExpr(sym:String, value: LispVal): LispList =
    LispList(List(LispAtom(sym), value))
}
