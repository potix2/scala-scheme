package com.potix2.scheme

import com.potix2.scheme.LispError.ThrowsError
import org.specs2.mutable.SpecificationWithJUnit
import scalaz.Scalaz._

class EvaluatorSpec extends SpecificationWithJUnit {
  val evaluator = new Evaluator {}

  "eval an atom" should {
    "return the atom" in {
      evaluator.eval(LispAtom("abcd")) must_== LispAtom("abcd").point[ThrowsError]
    }
  }
  "eval a quoted value" should {
    "'abcd to abcd" in {
      evaluator.eval(makeExpr("quote", LispAtom("abcd"))).toEither must beRight(LispAtom("abcd"))
    }
    "''a to 'a" in {
      evaluator.eval(makeExpr("quote", makeExpr("quote", LispAtom("a")))).toEither must beRight(LispList(List(LispAtom("quote"), LispAtom("a"))))
    }
  }
  "eval" should {
    "reduce (+ 1 2 3) to 6" in {
      evaluator.eval(LispList(List(LispAtom("+"), LispInteger(1), LispInteger(2), LispInteger(3)))) must_== LispInteger(6).point[ThrowsError]
    }
    "reduce (- 4 1 2) to 1" in {
      evaluator.eval(LispList(List(LispAtom("-"), LispInteger(4), LispInteger(1), LispInteger(2)))) must_== LispInteger(1).point[ThrowsError]
    }
    "reduce (+ 2 (- 4 1)) to 5" in {
      evaluator.eval(LispList(List(LispAtom("+"), LispInteger(2), LispList(List(LispAtom("-"), LispInteger(4), LispInteger(1)))))) must_== LispInteger(5).point[ThrowsError]
    }
    "reduce (* 2 3) to 6" in {
      evaluator.eval(LispList(List(LispAtom("*"), LispInteger(2), LispInteger(3)))) must_== LispInteger(6).point[ThrowsError]
    }
  }
  "type-test boolean?" should {
    "(boolean? #t)" in { evaluator.eval(makeExpr("boolean?", LispBool(true))) must_== LispBool(true).point[ThrowsError] }
    "(boolean? #f)" in { evaluator.eval(makeExpr("boolean?", LispBool(false))) must_== LispBool(true).point[ThrowsError] }
    "(boolean? \"abc\")" in { evaluator.eval(makeExpr("boolean?", LispString("abc"))) must_== LispBool(false).point[ThrowsError] }
    "(boolean? 1)" in { evaluator.eval(makeExpr("boolean?", LispInteger(1))) must_== LispBool(false).point[ThrowsError] }
    "(boolean? a)" in { evaluator.eval(makeExpr("boolean?", LispAtom("a"))) must_== LispBool(false).point[ThrowsError] }
  }

  "type-test string?" should {
    "(string? #t)" in { evaluator.eval(makeExpr("string?", LispBool(true))) must_== LispBool(false).point[ThrowsError] }
    "(string? \"abc\")" in { evaluator.eval(makeExpr("string?", LispString("abc"))) must_== LispBool(true).point[ThrowsError] }
    "(string? 1)" in { evaluator.eval(makeExpr("string?", LispInteger(1))) must_== LispBool(false).point[ThrowsError] }
    "(string? a)" in { evaluator.eval(makeExpr("string?", LispAtom("a"))) must_== LispBool(false).point[ThrowsError] }
  }

  "type-test number?" should {
    "(number? #t)" in { evaluator.eval(makeExpr("number?", LispBool(true))) must_== LispBool(false).point[ThrowsError] }
    "(number? \"abc\")" in { evaluator.eval(makeExpr("number?", LispString("abc"))) must_== LispBool(false).point[ThrowsError] }
    "(number? 1)" in { evaluator.eval(makeExpr("number?", LispInteger(1))) must_== LispBool(true).point[ThrowsError] }
    "(number? a)" in { evaluator.eval(makeExpr("number?", LispAtom("a"))) must_== LispBool(false).point[ThrowsError] }
  }

  "type-test symbol?" should {
    "(symbol? #t)" in { evaluator.eval(makeExpr("symbol?", LispBool(true))) must_== LispBool(false).point[ThrowsError] }
    "(symbol? \"abc\")" in { evaluator.eval(makeExpr("symbol?", LispString("abc"))) must_== LispBool(false).point[ThrowsError] }
    "(symbol? 1)" in { evaluator.eval(makeExpr("symbol?", LispInteger(1))) must_== LispBool(false).point[ThrowsError] }
    "(symbol? a)" in { evaluator.eval(makeExpr("symbol?", LispAtom("a"))) must_== LispBool(true).point[ThrowsError] }
  }

  "if" should {
    "(if #f 1 2)" in { evaluator.eval(makeExpr("if", LispBool(false), LispInteger(1), LispInteger(2))).toEither must beRight(LispInteger(2)) }
    "(if #f 1 2)" in { evaluator.eval(makeExpr("if", LispBool(true), LispInteger(1), LispInteger(2))).toEither must beRight(LispInteger(1)) }
  }

  "car" should {
    "(car 'a)" in { evaluator.eval(makeExpr("car", LispList(List(LispAtom("quote"), LispAtom("a"))))).toEither must beLeft(TypeMismatch("pair", LispAtom("a"))) }
    "(car 'a 'b)" in { evaluator.eval(makeExpr("car", makeExpr("quote", LispAtom("a")), makeExpr("quote", LispAtom("b")))).toEither must beLeft(beAnInstanceOf[NumArgs]) }
    "(car '(a b))" in { evaluator.eval(makeExpr("car", makeExpr("quote", LispList(List(LispAtom("a"), LispAtom("b")))))).toEither must beRight(LispAtom("a")) }
    "(car '(a b . c))" in { evaluator.eval(makeExpr("car", makeExpr("quote", LispDottedList(List(LispAtom("a"), LispAtom("b")),LispAtom("c"))))).toEither must beRight(LispAtom("a")) }
  }

  "cdr" should {
    "(cdr 'a)" in { evaluator.eval(makeExpr("cdr", LispList(List(LispAtom("quote"), LispAtom("a"))))).toEither must beLeft(TypeMismatch("pair", LispAtom("a"))) }
    "(cdr 'a 'b)" in { evaluator.eval(makeExpr("cdr", makeExpr("quote", LispAtom("a")), makeExpr("quote", LispAtom("b")))).toEither must beLeft(beAnInstanceOf[NumArgs]) }
    "(cdr '(a b))" in { evaluator.eval(makeExpr("cdr", makeExpr("quote", LispList(List(LispAtom("a"), LispAtom("b")))))).toEither must beRight(LispList(List(LispAtom("b")))) }
    "(car '(a . b))" in { evaluator.eval(makeExpr("cdr", makeExpr("quote", LispDottedList(List(LispAtom("a")),LispAtom("b"))))).toEither must beRight(LispAtom("b")) }
    "(car '(a b . c))" in { evaluator.eval(makeExpr("cdr", makeExpr("quote", LispDottedList(List(LispAtom("a"), LispAtom("b")),LispAtom("c"))))).toEither must beRight(LispDottedList(List(LispAtom("b")), LispAtom("c"))) }
    "(cdr '(a))" in { evaluator.eval(makeExpr("cdr", makeExpr("quote", LispList(List(LispAtom("a")))))).toEither must beRight(LispList(List())) }
  }

  def makeExpr(sym:String, values: LispVal*): LispList =
    LispList((LispAtom(sym) :: values.toList))
}
