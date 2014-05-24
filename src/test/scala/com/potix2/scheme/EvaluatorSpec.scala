package com.potix2.scheme

import com.potix2.scheme.LispError.IOThrowsError
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.matcher.Matcher
import scala.reflect.ClassTag

class EvaluatorSpec extends SpecificationWithJUnit {
  val evaluator = new Evaluator {}

  "eval an atom" should {
    "return the atom" in {
      eval(LispAtom("abcd")) must beLispVal(LispAtom("abcd"))
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
      eval(LispList(List(LispAtom("+"), LispInteger(1), LispInteger(2), LispInteger(3)))) must beLispVal(LispInteger(6))
    }
    "reduce (- 4 1 2) to 1" in {
      eval(LispList(List(LispAtom("-"), LispInteger(4), LispInteger(1), LispInteger(2)))) must beLispVal(LispInteger(1))
    }
    "reduce (+ 2 (- 4 1)) to 5" in {
      eval(LispList(List(LispAtom("+"), LispInteger(2), LispList(List(LispAtom("-"), LispInteger(4), LispInteger(1)))))) must beLispVal(LispInteger(5))
    }
    "reduce (* 2 3) to 6" in {
      eval(LispList(List(LispAtom("*"), LispInteger(2), LispInteger(3)))) must beLispVal(LispInteger(6))
    }
  }
  "type-test boolean?" should {
    "(boolean? #t)" in { eval(makeExpr("boolean?", LispBool(true))) must beLispVal(LispBool(true)) }
    "(boolean? #f)" in { eval(makeExpr("boolean?", LispBool(false))) must beLispVal(LispBool(true)) }
    "(boolean? \"abc\")" in { eval(makeExpr("boolean?", LispString("abc"))) must beLispVal(LispBool(false)) }
    "(boolean? 1)" in { eval(makeExpr("boolean?", LispInteger(1))) must beLispVal(LispBool(false)) }
    "(boolean? a)" in { eval(makeExpr("boolean?", LispAtom("a"))) must beLispVal(LispBool(false)) }
  }

  "type-test string?" should {
    "(string? #t)" in { eval(makeExpr("string?", LispBool(true))) must beLispVal(LispBool(false)) }
    "(string? \"abc\")" in { eval(makeExpr("string?", LispString("abc"))) must beLispVal(LispBool(true)) }
    "(string? 1)" in { eval(makeExpr("string?", LispInteger(1))) must beLispVal(LispBool(false)) }
    "(string? a)" in { eval(makeExpr("string?", LispAtom("a"))) must beLispVal(LispBool(false)) }
  }

  "type-test number?" should {
    "(number? #t)" in { eval(makeExpr("number?", LispBool(true))) must beLispVal(LispBool(false)) }
    "(number? \"abc\")" in { eval(makeExpr("number?", LispString("abc"))) must beLispVal(LispBool(false)) }
    "(number? 1)" in { eval(makeExpr("number?", LispInteger(1))) must beLispVal(LispBool(true)) }
    "(number? a)" in { eval(makeExpr("number?", LispAtom("a"))) must beLispVal(LispBool(false)) }
  }

  "type-test symbol?" should {
    "(symbol? #t)" in { eval(makeExpr("symbol?", LispBool(true))) must beLispVal(LispBool(false)) }
    "(symbol? \"abc\")" in { eval(makeExpr("symbol?", LispString("abc"))) must beLispVal(LispBool(false)) }
    "(symbol? 1)" in { eval(makeExpr("symbol?", LispInteger(1))) must beLispVal(LispBool(false)) }
    "(symbol? a)" in { eval(makeExpr("symbol?", LispAtom("a"))) must beLispVal(LispBool(true)) }
  }

  "if" should {
    "(if #f 1 2)" in { eval(makeExpr("if", LispBool(false), LispInteger(1), LispInteger(2))) must beLispVal(LispInteger(2)) }
    "(if #f 1 2)" in { eval(makeExpr("if", LispBool(true), LispInteger(1), LispInteger(2))) must beLispVal(LispInteger(1)) }
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
    "(cons 'a 'b 'c))" in { eval(makeExpr("cons", makeExpr("quote", LispAtom("a")), LispAtom("b"), LispAtom("c"))) must haveError[NumArgs] }
  }

  "eqv?" should {
    "(eqv? #f #f)" in { eval(makeExpr("eqv?", LispBool(false), LispBool(false))) must beLispVal(LispBool(true)) }
    "(eqv? #f #t)" in { eval(makeExpr("eqv?", LispBool(false), LispBool(true))) must beLispVal(LispBool(false)) }
    "(eqv? 1 1)" in { eval(makeExpr("eqv?", LispInteger(1), LispInteger(1))) must beLispVal(LispBool(true)) }
    "(eqv? 1 2)" in { eval(makeExpr("eqv?", LispInteger(1), LispInteger(2))) must beLispVal(LispBool(false)) }
    "(eqv? \"a\" \"a\")" in { eval(makeExpr("eqv?", LispString("a"), LispString("a"))) must beLispVal(LispBool(true)) }
    "(eqv? \"a\" \"b\")" in { eval(makeExpr("eqv?", LispString("a"), LispString("b"))) must beLispVal(LispBool(false)) }
    "(eqv? a a)" in { eval(makeExpr("eqv?", LispAtom("a"), LispAtom("a"))) must beLispVal(LispBool(true)) }
    "(eqv? a b)" in { eval(makeExpr("eqv?", LispAtom("a"), LispAtom("b"))) must beLispVal(LispBool(false)) }
    "(eqv? '(a . b) '(a . a))" in { eval(makeExpr("eqv?", makeDottedList(List(LispAtom("a")), LispAtom("b")), makeDottedList(List(LispAtom("a")), LispAtom("a")))) must beLispVal(LispBool(false)) }
    "(eqv? 1 2 3)" in { eval(makeExpr("eqv?", LispInteger(1), LispInteger(2), LispInteger(3))) must haveError[NumArgs] }
  }

  "equal?" should {
    "(equal? 1 \"1\")" in { eval(makeExpr("equal?", LispInteger(1), LispString("1"))) must beLispVal(LispBool(true)) }
    "(equal? 1 1)" in { eval(makeExpr("equal?", LispInteger(1), LispInteger(1))) must beLispVal(LispBool(true)) }
    "(equal? '(1 \"2\") '(1 2))" in { eval(makeExpr("equal?", makeExpr("quote", LispList(List(LispInteger(1), LispString("2")))),
      makeExpr("quote", LispList(List(LispInteger(1), LispInteger(2)))))) must beLispVal(LispBool(true)) }
  }

  "cond" should {
    "(cond (#f 1 2) (#t 3 4))" in { eval(makeExpr("cond", LispList(List(LispBool(false), LispInteger(1), LispInteger(2))), LispList(List(LispBool(true), LispInteger(3), LispInteger(4))))) must beLispVal(LispInteger(4)) }
    "(cond (#f 1 2) (else 3 4))" in { eval(makeExpr("cond", LispList(List(LispBool(false), LispInteger(1), LispInteger(2))), LispList(List(LispAtom("else"), LispInteger(3), LispInteger(4))))) must beLispVal(LispInteger(4)) }
    "(cond (1))" in { eval(makeExpr("cond", LispList(List(LispInteger(1))))) must beLispVal(LispInteger(1))}
    "(cond ((\"a\")))" in { eval(makeExpr("cond", LispList(List(LispList(List(LispString("a"))))))) must haveError[BadSpecialForm] }
    "(cond (1 (\"a\") 2))" in { eval(makeExpr("cond", LispList(List(LispInteger(1), LispList(List(LispString("a"))), LispInteger(2))))) must haveError[BadSpecialForm] }
    "(cond (1 => number?))" in { eval(makeExpr("cond", LispList(List(LispInteger(1),LispAtom("=>"), LispAtom("number?"))))) must beLispVal(LispBool(true))}
  }

  def makeDottedList(xs: List[LispVal], last: LispVal): LispList =
    makeExpr("quote", LispDottedList(xs, last))

  def makeExpr(sym:String, values: LispVal*): LispList =
    LispList(LispAtom(sym) :: values.toList)

  def eval(value: LispVal) = evaluator.eval(LispEnv.nullEnv.unsafePerformIO())(value)

  def beLispVal(expected:LispVal): Matcher[IOThrowsError[LispVal]] = { value: IOThrowsError[LispVal] =>
    value.toEither.unsafePerformIO() must beRight(expected)
  }

  def haveError[T: ClassTag]: Matcher[IOThrowsError[LispVal]] = { value: IOThrowsError[LispVal] =>
    value.toEither.unsafePerformIO() must beLeft(beAnInstanceOf[T])
  }
}
