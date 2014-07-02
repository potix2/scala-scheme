package com.potix2.scheme

import scala.reflect.ClassTag

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
      eval(l"'abcd") must beLispVal(l"abcd")
    }
    "''a to 'a" in {
      eval(l"''a") must beLispVal(l"'a")
    }
  }
  "eval" should {
    "reduce (+ 1 2 3) to 6" in {
      eval(l"(+ 1 2 3)") must beLispVal(l"6")
    }
    "reduce (- 4 1 2) to 1" in {
      eval(l"(- 4 1 2)") must beLispVal(l"1")
    }
    "reduce (+ 2 (- 4 1)) to 5" in {
      eval(l"(+ 2 (- 4 1))") must beLispVal(l"5")
    }
    "reduce (* 2 3) to 6" in {
      eval(l"(* 2 3)") must beLispVal(l"6")
    }
  }
  "type-test boolean?" should {
    "(boolean? #t)"      in { eval(l"(boolean? #t)") must beLispVal(l"#t") }
    "(boolean? #f)"      in { eval(l"(boolean? #f)") must beLispVal(l"#t") }
    "(boolean? \"abc\")" in { eval(l"""(boolean? "abc")""") must beLispVal(l"#f") }
    "(boolean? 1)"       in { eval(l"(boolean? 1)") must beLispVal(l"#f") }
    "(boolean? 'a)"      in { eval(l"(boolean? 'a)") must beLispVal(l"#f") }
  }

  "type-test string?" should {
    "(string? #t)"      in { eval(l"(string? #t)") must beLispVal(l"#f") }
    "(string? \"abc\")" in { eval(l"""(string? "abc")""") must beLispVal(l"#t") }
    "(string? 1)"       in { eval(l"(string? 1)") must beLispVal(l"#f") }
    "(string? 'a)"      in { eval(l"(string? 'a)") must beLispVal(l"#f") }
  }

  "type-test number?" should {
    "(number? #t)"      in { eval(l"(number? #t)") must beLispVal(l"#f") }
    "(number? \"abc\")" in { eval(l"""(number? "abc")""") must beLispVal(l"#f") }
    "(number? 1)"       in { eval(l"(number? 1)") must beLispVal(l"#t") }
    "(number? 1.0)"     in { eval(l"(number? 1.0)") must beLispVal(l"#t") }
    "(number? 'a)"      in { eval(l"(number? 'a)") must beLispVal(l"#f") }
  }

  "type-test symbol?" should {
    "(symbol? #t)"      in { eval(l"(symbol? #t)") must beLispVal(l"#f") }
    "(symbol? \"abc\")" in { eval(l"""(symbol? "abc")""") must beLispVal(l"#f") }
    "(symbol? 1)"       in { eval(l"(symbol? 1)") must beLispVal(l"#f") }
    "(symbol? 'foo)"    in { eval(l"(symbol? 'foo)") must beLispVal(l"#t") }
  }

  "type-test pair?" should {
    "(pair? #t)"       in { eval(l"(pair? #t)") must beLispVal(l"#f") }
    "(pair? \"abc\")"  in { eval(l"""(pair? "abc")""") must beLispVal(l"#f") }
    "(pair? 1)"        in { eval(l"(pair? 1)") must beLispVal(l"#f") }
    "(pair? 'foo)"     in { eval(l"(pair? 'foo)") must beLispVal(l"#f") }
    "(pair? '(a . b))" in { eval(l"(pair? '(a . b))") must beLispVal(l"#t") }
    "(pair? '(a b c))" in { eval(l"(pair? '(a b c))") must beLispVal(l"#t") }
    "(pair? '())"      in { eval(l"(pair? '())") must beLispVal(l"#f") }
    "(pair? '#(a b))"  in { eval(l"(pair? '#(a b))") must beLispVal(l"#f") }
  }

  "type-test list?" should {
    "(list? #t)"       in { eval(l"(list? #t)") must beLispVal(l"#f") }
    "(list? \"abc\")"  in { eval(l"""(list? "abc")""") must beLispVal(l"#f") }
    "(list? 1)"        in { eval(l"(list? 1)") must beLispVal(l"#f") }
    "(list? 'foo)"     in { eval(l"(list? 'foo)") must beLispVal(l"#f") }
    "(list? '(a . b))" in { eval(l"(list? '(a . b))") must beLispVal(l"#f") }
    "(list? '(a b c))" in { eval(l"(list? '(a b c))") must beLispVal(l"#t") }
    "(list? '())"      in { eval(l"(list? '())") must beLispVal(l"#t") }
    "(list? '#(a b))"  in { eval(l"(list? '#(a b))") must beLispVal(l"#f") }
  }

  "if" should {
    "(if #f 1 2)" in { eval(l"(if #f 1 2)") must beLispVal(l"2") }
    "(if #t 1 2)" in { eval(l"(if #t 1 2)") must beLispVal(l"1") }
  }

  "car" should {
    "(car 'a)"         in { eval(l"(car 'a)") must haveError[TypeMismatch] }
    "(car 'a 'b)"      in { eval(l"(car 'a 'b)") must haveError[NumArgs] }
    "(car '(a b))"     in { eval(l"(car '(a b))") must beLispVal(l"a") }
    "(car '(a b . c))" in { eval(l"(car '(a b . c))") must beLispVal(l"a") }
  }

  "cdr" should {
    "(cdr 'a)"         in { eval(l"(cdr 'a)") must haveError[TypeMismatch] }
    "(cdr 'a 'b)"      in { eval(l"(cdr 'a 'b)") must haveError[NumArgs] }
    "(cdr '(a b))"     in { eval(l"(cdr '(a b))") must beLispVal(l"(b)") }
    "(cdr '(a . b))"   in { eval(l"(cdr '(a . b))") must beLispVal(l"b") }
    "(cdr '(a b . c))" in { eval(l"(cdr '(a b . c))") must beLispVal(l"(b . c)") }
    "(cdr '(a))"       in { eval(l"(cdr '(a))") must beLispVal(l"()") }
  }

  "cons" should {
    "(cons 'a 'b)"       in { eval(l"(cons 'a 'b)") must beLispVal(l"(a . b)") }
    "(cons 'a '(b . c))" in { eval(l"(cons 'a '(b . c))") must beLispVal(l"(a b . c)") }
    "(cons 'a '(b c))"   in { eval(l"(cons 'a '(b c))") must beLispVal(l"(a b c)") }
    "(cons 1 2 3))"      in { eval(l"(cons 1 2 3)") must haveError[NumArgs] }
  }

  "eqv?" should {
    "(eqv? #f #f)"       in { eval(l"(eqv? #f #f)") must beLispVal(l"#t") }
    "(eqv? #f #t)"       in { eval(l"(eqv? #f #t)") must beLispVal(l"#f") }
    "(eqv? 1 1)"         in { eval(l"(eqv? 1 1)") must beLispVal(l"#t") }
    "(eqv? 1 2)"         in { eval(l"(eqv? 1 2)") must beLispVal(l"#f") }
    "(eqv? \"a\" \"a\")" in { eval(l"""(eqv? "a" "a")""") must beLispVal(l"#t") }
    "(eqv? \"a\" \"b\")" in { eval(l"""(eqv? "a" "b")""") must beLispVal(l"#f") }
    "(eqv? 'a 'a)"       in { eval(l"(eqv? 'a 'a)") must beLispVal(l"#t") }
    "(eqv? 'a 'b)"       in { eval(l"(eqv? 'a 'b)") must beLispVal(l"#f") }
    "(eqv? 1 2 3)"       in { eval(l"(eqv? 1 2 3)") must haveError[NumArgs] }
    "(eqv? '() '())"     in { eval(l"(eqv? '() '())") must beLispVal(l"#t") }
    "(eqv? '(1) '())"    in { eval(l"(eqv? '(1) '())") must beLispVal(l"#f") }
    "(eqv? '(a . b) '(a . a))" in { eval(l"(eqv? '(a . b) '(a . a))") must beLispVal(l"#f") }
  }

  "equal?" should {
    "(equal? 1 \"1\")"           in { eval(l"""(equal? 1 "1")""") must beLispVal(l"#t") }
    "(equal? 1 1)"               in { eval(l"(equal? 1 1)") must beLispVal(l"#t") }
    "(equal? 1 2)"               in { eval(l"(equal? 1 2)") must beLispVal(l"#f") }
    "(equal? '(1 \"2\") '(1 2))" in { eval(l"""(equal? '(1 "2") '(1 2))""") must beLispVal(l"#t") }
    //"(equal? '(1 \"2\") '(1))"   in { eval(l"""(equal? '(1 "2") '(1))""") must beLispVal(l"#f") }
  }

  "cond" should {
    "(cond (#f 1 2) (#t 3 4))"   in { eval(l"(cond (#f 1 2) (#t 3 4))") must beLispVal(l"4") }
    "(cond (#f 1 2) (else 3 4))" in { eval(l"(cond (#f 1 2) (else 3 4))") must beLispVal(l"4") }
    "(cond (1))"                 in { eval(l"(cond (1))") must beLispVal(l"1") }
    "(cond ((\"a\")))"           in { eval(l"""(cond (("a")))""") must haveError[NotFunction] }
    "(cond (1 (\"a\") 2))"       in { eval(l"""(cond (1 ("a") 2))""") must haveError[NotFunction] }
    "(cond (1 => number?))"      in { eval(l"(cond (1 => number?))") must beLispVal(l"#t") }
  }

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
