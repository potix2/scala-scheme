package com.potix2.scheme

import scala.reflect.ClassTag

import com.potix2.scheme.Lisp._
import org.specs2.matcher.Matcher
import org.specs2.mutable.SpecificationWithJUnit

class EvaluatorSpec extends SpecificationWithJUnit {
  val evaluator = new Evaluator with LispEnv with LispParser with IOPrimitives with ListPrimitives

  "eval an atom" should {
    "return the atom" in {
      eval(LispAtom("abcd")) must haveError[UnboundVar]
    }
  }
  "eval a quoted value" should {
    "'abcd to abcd" in {
      eval(makeExpr("quote", LispAtom("abcd"))) must beLispVal(LispAtom("abcd"))
    }
    "''a to 'a" in {
      eval(makeExpr("quote", makeExpr("quote", LispAtom("a")))) must beLispVal(LispList(List(LispAtom("quote"), LispAtom("a"))))
    }
  }
  "eval" should {
    "reduce (+ 1 2 3) to 6" in {
      eval(LispList(List(LispAtom("+"), LispLong(1), LispLong(2), LispLong(3)))) must beLispVal(LispLong(6))
    }
    "reduce (- 4 1 2) to 1" in {
      eval(LispList(List(LispAtom("-"), LispLong(4), LispLong(1), LispLong(2)))) must beLispVal(LispLong(1))
    }
    "reduce (+ 2 (- 4 1)) to 5" in {
      eval(LispList(List(LispAtom("+"), LispLong(2), LispList(List(LispAtom("-"), LispLong(4), LispLong(1)))))) must beLispVal(LispLong(5))
    }
    "reduce (* 2 3) to 6" in {
      eval(LispList(List(LispAtom("*"), LispLong(2), LispLong(3)))) must beLispVal(LispLong(6))
    }
  }
  "type-test boolean?" should {
    "(boolean? #t)" in { eval(makeExpr("boolean?", LispBool(true))) must beLispVal(LispBool(true)) }
    "(boolean? #f)" in { eval(makeExpr("boolean?", LispBool(false))) must beLispVal(LispBool(true)) }
    "(boolean? \"abc\")" in { eval(makeExpr("boolean?", LispString("abc"))) must beLispVal(LispBool(false)) }
    "(boolean? 1)" in { eval(makeExpr("boolean?", LispLong(1))) must beLispVal(LispBool(false)) }
    "(boolean? 'a)" in { eval(makeExpr("boolean?", makeExpr("quote", LispAtom("a")))) must beLispVal(LispBool(false)) }
  }

  "type-test string?" should {
    "(string? #t)" in { eval(makeExpr("string?", LispBool(true))) must beLispVal(LispBool(false)) }
    "(string? \"abc\")" in { eval(makeExpr("string?", LispString("abc"))) must beLispVal(LispBool(true)) }
    "(string? 1)" in { eval(makeExpr("string?", LispLong(1))) must beLispVal(LispBool(false)) }
    "(string? 'a)" in { eval(makeExpr("string?", makeExpr("quote", LispAtom("a")))) must beLispVal(LispBool(false)) }
  }

  "type-test number?" should {
    "(number? #t)" in { eval(makeExpr("number?", LispBool(true))) must beLispVal(LispBool(false)) }
    "(number? \"abc\")" in { eval(makeExpr("number?", LispString("abc"))) must beLispVal(LispBool(false)) }
    "(number? 1)" in { eval(makeExpr("number?", LispLong(1))) must beLispVal(LispBool(true)) }
    "(number? 1.0)" in { eval(makeExpr("number?", LispDouble(1.0))) must beLispVal(LispBool(true)) }
    "(number? 'a)" in { eval(makeExpr("number?", makeExpr("quote", LispAtom("a")))) must beLispVal(LispBool(false)) }
  }

  "type-test symbol?" should {
    "(symbol? #t)" in { eval(makeExpr("symbol?", LispBool(true))) must beLispVal(LispBool(false)) }
    "(symbol? \"abc\")" in { eval(makeExpr("symbol?", LispString("abc"))) must beLispVal(LispBool(false)) }
    "(symbol? 1)" in { eval(makeExpr("symbol?", LispLong(1))) must beLispVal(LispBool(false)) }
    "(symbol? 'foo)" in { eval(makeExpr("symbol?", makeExpr("quote", LispAtom("foo")))) must beLispVal(LispBool(true)) }
  }

  "type-test pair?" should {
    "(pair? #t)"       in { eval(makeExpr("pair?", LispBool(true))) must beLispVal(LispBool(false)) }
    "(pair? \"abc\")"  in { eval(makeExpr("pair?", LispString("abc"))) must beLispVal(LispBool(false)) }
    "(pair? 1)"        in { eval(makeExpr("pair?", LispLong(1))) must beLispVal(LispBool(false)) }
    "(pair? 'foo)"     in { eval(makeExpr("pair?", makeExpr("quote", LispAtom("foo")))) must beLispVal(LispBool(false)) }
    "(pair? '(a . b))" in { eval(makeExpr("pair?", makeExpr("quote", LispDottedList(List(LispAtom("a")), LispAtom("b"))))) must beLispVal(LispBool(true)) }
    "(pair? '(a b c))" in { eval(makeExpr("pair?", makeExpr("quote", LispList(List(LispAtom("a"), LispAtom("b"), LispAtom("c")))))) must beLispVal(LispBool(true)) }
    "(pair? '())"      in { eval(makeExpr("pair?", makeExpr("quote", LispList(List())))) must beLispVal(LispBool(false)) }
    "(pair? '#(a b))"  in { eval(makeExpr("pair?", makeExpr("quote", LispVector(Vector(LispAtom("a"), LispAtom("b")))))) must beLispVal(LispBool(false)) }
  }

  "if" should {
    "(if #f 1 2)" in { eval(makeExpr("if", LispBool(false), LispLong(1), LispLong(2))) must beLispVal(LispLong(2)) }
    "(if #t 1 2)" in { eval(makeExpr("if", LispBool(true), LispLong(1), LispLong(2))) must beLispVal(LispLong(1)) }
  }

  "car" should {
    "(car 'a)" in { eval(makeExpr("car", makeExpr("quote", LispAtom("a")))) must haveError[TypeMismatch] }
    "(car 'a 'b)" in { eval(makeExpr("car", makeExpr("quote", LispAtom("a")), makeExpr("quote", LispAtom("b")))) must haveError[NumArgs] }
    "(car '(a b))" in { eval(makeExpr("car", makeExpr("quote", LispList(List(LispAtom("a"), LispAtom("b")))))) must beLispVal(LispAtom("a")) }
    "(car '(a b . c))" in { eval(makeExpr("car", makeDottedList(List(LispAtom("a"), LispAtom("b")),LispAtom("c")))) must beLispVal(LispAtom("a")) }
  }

  "cdr" should {
    "(cdr 'a)" in { eval(makeExpr("cdr", makeExpr("quote", LispAtom("a")))) must haveError[TypeMismatch] }
    "(cdr 'a 'b)" in { eval(makeExpr("cdr", makeExpr("quote", LispAtom("a")), makeExpr("quote", LispAtom("b")))) must haveError[NumArgs] }
    "(cdr '(a b))" in { eval(makeExpr("cdr", makeExpr("quote", LispList(List(LispAtom("a"), LispAtom("b")))))) must beLispVal(LispList(List(LispAtom("b")))) }
    "(cdr '(a . b))" in { eval(makeExpr("cdr", makeDottedList(List(LispAtom("a")), LispAtom("b")))) must beLispVal(LispAtom("b")) }
    "(cdr '(a b . c))" in { eval(makeExpr("cdr", makeExpr("quote", LispDottedList(List(LispAtom("a"), LispAtom("b")),LispAtom("c"))))) must beLispVal(LispDottedList(List(LispAtom("b")), LispAtom("c"))) }
    "(cdr '(a))" in { eval(makeExpr("cdr", makeExpr("quote", LispList(List(LispAtom("a")))))) must beLispVal(LispList(List())) }
  }

  "cons" should {
    "(cons 'a 'b)" in { eval(makeExpr("cons", makeExpr("quote", LispAtom("a")), makeExpr("quote", LispAtom("b")))) must beLispVal(LispDottedList(List(LispAtom("a")), LispAtom("b"))) }
    "(cons 'a '(b . c))" in { eval(makeExpr("cons", makeExpr("quote", LispAtom("a")), makeDottedList(List(LispAtom("b")), LispAtom("c")))) must beLispVal(LispDottedList(List(LispAtom("a"), LispAtom("b")), LispAtom("c"))) }
    "(cons 'a '(b c))" in { eval(makeExpr("cons", makeExpr("quote", LispAtom("a")), makeExpr("quote", LispList(List(LispAtom("b"), LispAtom("c")))))) must beLispVal(LispList(List(LispAtom("a"), LispAtom("b"), LispAtom("c")))) }
    "(cons 1 2 3))" in { eval(makeExpr("cons", LispLong(1), LispLong(2), LispLong(3))) must haveError[NumArgs] }
  }

  "eqv?" should {
    "(eqv? #f #f)" in { eval(makeExpr("eqv?", LispBool(false), LispBool(false))) must beLispVal(LispBool(true)) }
    "(eqv? #f #t)" in { eval(makeExpr("eqv?", LispBool(false), LispBool(true))) must beLispVal(LispBool(false)) }
    "(eqv? 1 1)" in { eval(makeExpr("eqv?", LispLong(1), LispLong(1))) must beLispVal(LispBool(true)) }
    "(eqv? 1 2)" in { eval(makeExpr("eqv?", LispLong(1), LispLong(2))) must beLispVal(LispBool(false)) }
    "(eqv? \"a\" \"a\")" in { eval(makeExpr("eqv?", LispString("a"), LispString("a"))) must beLispVal(LispBool(true)) }
    "(eqv? \"a\" \"b\")" in { eval(makeExpr("eqv?", LispString("a"), LispString("b"))) must beLispVal(LispBool(false)) }
    "(eqv? 'a 'a)" in { eval(makeExpr("eqv?", makeExpr("quote", LispAtom("a")), makeExpr("quote", LispAtom("a")))) must beLispVal(LispBool(true)) }
    "(eqv? 'a 'b)" in { eval(makeExpr("eqv?", makeExpr("quote", LispAtom("a")), makeExpr("quote", LispAtom("b")))) must beLispVal(LispBool(false)) }
    "(eqv? '(a . b) '(a . a))" in { eval(makeExpr("eqv?", makeDottedList(List(LispAtom("a")), LispAtom("b")), makeDottedList(List(LispAtom("a")), LispAtom("a")))) must beLispVal(LispBool(false)) }
    "(eqv? 1 2 3)" in { eval(makeExpr("eqv?", LispLong(1), LispLong(2), LispLong(3))) must haveError[NumArgs] }
    "(eqv? '() '())" in { eval(makeExpr("eqv?", makeExpr("quote", LispList(List())), makeExpr("quote", LispList(List())))) must beLispVal(LispBool(true)) }
    "(eqv? '(1) '())" in { eval(makeExpr("eqv?", makeExpr("quote", LispList(List(LispLong(1)))), makeExpr("quote", LispList(List())))) must beLispVal(LispBool(false)) }
  }

  "equal?" should {
    "(equal? 1 \"1\")" in { eval(makeExpr("equal?", LispLong(1), LispString("1"))) must beLispVal(LispBool(true)) }
    "(equal? 1 1)" in { eval(makeExpr("equal?", LispLong(1), LispLong(1))) must beLispVal(LispBool(true)) }
    "(equal? '(1 \"2\") '(1 2))" in { eval(makeExpr("equal?", makeExpr("quote", LispList(List(LispLong(1), LispString("2")))),
      makeExpr("quote", LispList(List(LispLong(1), LispLong(2)))))) must beLispVal(LispBool(true)) }
  }

  "cond" should {
    "(cond (#f 1 2) (#t 3 4))" in { eval(makeExpr("cond", LispList(List(LispBool(false), LispLong(1), LispLong(2))), LispList(List(LispBool(true), LispLong(3), LispLong(4))))) must beLispVal(LispLong(4)) }
    "(cond (#f 1 2) (else 3 4))" in { eval(makeExpr("cond", LispList(List(LispBool(false), LispLong(1), LispLong(2))), LispList(List(LispAtom("else"), LispLong(3), LispLong(4))))) must beLispVal(LispLong(4)) }
    "(cond (1))" in { eval(makeExpr("cond", LispList(List(LispLong(1))))) must beLispVal(LispLong(1))}
    "(cond ((\"a\")))" in { eval(makeExpr("cond", LispList(List(LispList(List(LispString("a"))))))) must haveError[NotFunction] }
    "(cond (1 (\"a\") 2))" in { eval(makeExpr("cond", LispList(List(LispLong(1), LispList(List(LispString("a"))), LispLong(2))))) must haveError[NotFunction] }
    "(cond (1 => number?))" in { eval(makeExpr("cond", LispList(List(LispLong(1),LispAtom("=>"), LispAtom("number?"))))) must beLispVal(LispBool(true))}
  }

  def makeDottedList(xs: List[LispVal], last: LispVal): LispList =
    makeExpr("quote", LispDottedList(xs, last))

  def makeExpr(sym:String, values: LispVal*): LispList =
    LispList(LispAtom(sym) :: values.toList)

  def eval(value: LispVal) = {
    evaluator.eval(evaluator.primitiveBindings.unsafePerformIO())(value)
  }

  def beLispVal(expected:LispVal): Matcher[IOThrowsError[LispVal]] = { value: IOThrowsError[LispVal] =>
    value.toEither.unsafePerformIO() must beRight(expected)
  }

  def haveError[T: ClassTag]: Matcher[IOThrowsError[LispVal]] = { value: IOThrowsError[LispVal] =>
    value.toEither.unsafePerformIO() must beLeft(beAnInstanceOf[T])
  }
}
